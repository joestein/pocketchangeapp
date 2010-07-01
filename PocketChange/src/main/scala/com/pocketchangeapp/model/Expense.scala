package com.pocketchangeapp {
package model {

import _root_.java.math.MathContext
import _root_.java.util.Date
import _root_.java.text.{DateFormat,SimpleDateFormat}

import scala.xml.{NodeSeq,Text}

import net.liftweb.common.{Box,Empty,Full,Logger}
import _root_.net.liftweb.util.Helpers._
import _root_.net.liftweb.mongodb._

import org.bson.types.ObjectId
import com.mongodb.DBObject
import com.osinka.mongodb._
import com.osinka.mongodb.shape._

import com.pocketchangeapp.db._
import com.pocketchangeapp.util.Util

class Expense(val account: Account) extends MongoObject with EasyID {
    var dateOf: Date = _
    var serialNumber: Long = 0
    var currentBalance: BigDecimal = Expense.currentBalance.zero
    var amount: BigDecimal = Expense.amount.zero
    var description: String = ""
    var notes: Option[String] = None
    var receipt: Option[ObjectId] = None
    var tags: List[String] = Nil

    def uploadReceipt(mime: String, fileName: String, data: Array[Byte]) {
        Logger(classOf[Expense]).debug("receipt uploaded "+fileName)
        receipt = Some(Database.saveBinary(mime, fileName, data))
    }

    def getReceipt = receipt flatMap Database.getBinary

    def removeReceipt {
        receipt foreach Database.removeBinary
    }

    def accountName = Text("My account is " + account.name)

    final val dateFormat =
      DateFormat.getDateInstance(DateFormat.SHORT)

    def dateAsHtml = Text(dateFormat.format(dateOf))

    def tags(newTags: String): this.type = {
        tags = (tags ++ newTags.roboSplit(",")).removeDuplicates
        this
    }

    def tags(newTags: List[String]): this.type = {
        tags = newTags
        this
    }

    def owner = account.owner

    def showTags = Text(tags.mkString(", "))
    def showXMLTags: NodeSeq = tags.map(t => <tag>{t}</tag>)
    def showJSONTags: String = tags.map(t => {"'" + t + "'" }).mkString(", ")

    override def equals(other: Any) = other match {
        case e : Expense if e.mongoOID == this.mongoOID => true
        case _ => false
    }

    override def hashCode = this.mongoOID.hashCode

    private def getAccountName = account.name

    def toXML: NodeSeq = {
        val id = this.mongoOID map { "http://www.pocketchangeapp.com/api/expense/" + _ }
        val formatter = new  SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'")
        val edate = formatter.format(this.dateOf)

        <expense>
            <id>{id}</id>
            <accountname>{getAccountName}</accountname>
            <date>{edate}</date>
            <description>{description}</description>
            <amount>{amount.toString}</amount>
            <tags>
                {showXMLTags}
            </tags>
        </expense>
    }

    /* Atom requires either an entry or feed to have:
    - title
    - lastupdated
    - uid
    */
    def toAtom = {
        val id = this.mongoOID map { "http://www.pocketchangeapp.com/api/expense/" + _ }
        val formatter = new  SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'")
        val edate = formatter.format(this.dateOf)

        <entry xmlns="http://www.w3.org/2005/Atom">
            <expense>
                <id>{id}</id>
                <accountname>{getAccountName}</accountname>
                <date>{edate}</date>
                <description>{description}</description>
                <amount>{amount.toString}</amount>
                <tags>
                    {showXMLTags}
                </tags>
            </expense>
        </entry>
    }

    def toJSON =
        this.mongoOID map { id =>
            val formatter = new  SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'")
            val edate = formatter.format(this.dateOf)

            "{'expense':{ 'id':'" + "http://www.pocketchangeapp.com/api/expense/" + id + "','accountname':'" + getAccountName + "'," +
            "'date':'" + edate + "'," +
            "'description':'" + description + "'," +
            "'amount':'" + amount.toString + "'," +
            "'tags': [" + showJSONTags + "]}"
        }
}

object Expense extends MongoObjectShape[Expense] with Model[Expense] with BigDecimalFields[Expense,Expense] {
    override val collectionName = "expense"

    override val indexes: List[Seq[FieldIndex]] = List(dateOf.ascending) :: List(serialNumber.ascending) :: Nil

    lazy val account = Field.ref("account", Account.getCollection, _.account)
    lazy val dateOf = Field.scalar("dateOf", _.dateOf, (x: Expense, v: Date) => x.dateOf = v)
    lazy val serialNumber = Field.scalar("serialNumber", _.serialNumber, (x: Expense, v: Long) => x.serialNumber = v)
    object currentBalance extends BigDecimalField("currentBalance", 2, _.currentBalance, Some((x: Expense, v: BigDecimal) => x.currentBalance = v))
    object amount extends BigDecimalField("amount", 2, _.amount, Some((x: Expense, v: BigDecimal) => x.amount = v))
    lazy val description = Field.scalar("description", _.description, (x: Expense, v: String) => x.description = v)
    lazy val notes = Field.optional("notes", _.notes, (x: Expense, v: Option[String]) => x.notes = v)
    lazy val receipt = Field.optional("receipt", _.receipt, (x: Expense, v: Option[ObjectId]) => x.receipt = v)
    lazy val tags = Field.array("tags", _.tags, (x: Expense, l: Seq[String]) => x.tags = l.toList )

    lazy val * = List(account, dateOf, serialNumber, currentBalance, amount, description, notes, tags, receipt)

    override def factory(dbo: DBObject) = for {account(acct) <- Some(dbo)} yield new Expense(acct)

    def getByAcct(acct: Account, startDate: Box[Date], endDate: Box[Date]) = {
        // The method could provide a means to modify Query object as original had. The only use
        // would be to limit query to 1 result, e.g. "query take 1"
        val clauses = Full(account is_== acct) :: startDate.map{dateOf is_>=} :: endDate.map{dateOf is_<=} :: Nil
        val query = clauses.flatMap{x => x} reduceLeft {_ and _}
        Expense where query sortBy serialNumber.descending in getCollection
    }

    // returns the serial and balance of the last entry before this one
    def getLastExpenseData(acct: Account, date: Date): (Long, BigDecimal) = getByAcct(acct, Empty, Full(date)).headOption match {
        case Some(expense) => (expense.serialNumber, expense.currentBalance)
        case _ => (0, 0)
    }

  /**
   * This method should be called before inserting the new serial number or else you'll get
   * a duplicate serial
   */
    def updateEntries(serial: Long, amount: BigDecimal) {
        // Wonderfully concise compared to the original
        val c = getCollection
        c(serialNumber is_> serial) = serialNumber.inc(1) and currentBalance.inc(amount) // "inc" will take care of amount serialization
    }

    def findTagExpenses(search: String): List[Expense] = (this where {tags is_~ search.r} in getCollection).toList.removeDuplicates
}

// Close package statements 
}}
