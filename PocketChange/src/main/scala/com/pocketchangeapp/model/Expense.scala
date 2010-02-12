/**
 Insert copyright boilerplate here
*/

package com.pocketchangeapp.model

import _root_.java.math.MathContext
import _root_.java.util.Date

import com.mongodb.DBObject
import com.pocketchangeapp.db._
import com.osinka.mongodb._
import com.osinka.mongodb.shape._
import _root_.scala.xml.Text

import _root_.net.liftweb.util.Helpers._
import _root_.net.liftweb.util.{Box,Empty,Full}
import _root_.net.liftweb.mongodb.MetaMapper

import scala.xml.{NodeSeq,Text}

import _root_.java.text.{DateFormat,SimpleDateFormat}

import com.pocketchangeapp.util.Util

class Expense(val account: Account) extends MongoObject {
    var dateOf: Date = _
    var serialNumber: Long = 0
    // TODO: BigDecimal
    var currentBalance: Double = 0.0
    var amount: Double = 0.0
    var description: String = ""
    var notes: Option[String] = None
    // TODO: receipt: GridFS
    var receipt = new Array[Byte](0) // should be GridFS
    var receiptMime = ""
    var tags: List[String] = Nil

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

object Expense extends MongoObjectShape[Expense] with Model[Expense] {
    override val collectionName = "expense"

    override val indexes = Nil

    lazy val account = Field.ref("account", Account.getCollection, _.account)
    lazy val dateOf = Field.scalar("dateOf", _.dateOf, (x: Expense, v: Date) => x.dateOf = v)
    lazy val serialNumber = Field.scalar("serialNumber", _.serialNumber, (x: Expense, v: Long) => x.serialNumber = v)
    lazy val currentBalance = Field.scalar("currentBalance", _.currentBalance, (x: Expense, v: Double) => x.currentBalance = v)
    lazy val amount = Field.scalar("amount", _.amount, (x: Expense, v: Double) => x.amount = v)
    lazy val description = Field.scalar("description", _.description, (x: Expense, v: String) => x.description = v)
    lazy val notes = Field.optional("notes", _.notes, (x: Expense, v: Option[String]) => x.notes = v)
//    var receipt: // should be GridFS
//    var receiptMime: Option[String] = None
    lazy val tags = Field.array("tags", _.tags, (x: Expense, l: Seq[String]) => x.tags = l.toList )

    lazy val * = List(account, dateOf, serialNumber, currentBalance, amount, description, notes, tags)

    override def factory(dbo: DBObject) = for {account(acct) <- Some(dbo)} yield new Expense(acct)

    def getByAcct(acct: Account, startDate: Box[Date], endDate: Box[Date]) = {
        val clauses = Full(account is_== acct) :: startDate.map{dateOf is_>=} :: endDate.map{dateOf is_<=} :: Nil
        val query = clauses.flatMap{x => x} reduceLeft {_ and _}
        Expense where query sortBy serialNumber.descending in getCollection
    }

//def getByAcct (account : Account, startDate : Box[Date], endDate : Box[Date], order : Box[OrderBy[Expense,_]], params : QueryParam[Expense]*) : List[Expense] = {
//    // Set up some query parameters
//    val dateClause : QueryParam[Expense] = (startDate,endDate) match {
//      case (Full(start), Full(end)) => BySql("expense.dateOf between ? and ?",
//					     IHaveValidatedThisSQL("dchenbecker", "2009-10-08"),
//					     start, end)
//      case (Full(start), Empty) => BySql("expense.dateOf >= ?",
//					 IHaveValidatedThisSQL("dchenbecker", "2009-10-08"),
//					 start)
//      case (Empty, Full(end)) => BySql("expense.dateOf <= ?",
//				       IHaveValidatedThisSQL("dchenbecker", "2009-10-08"),
//				       end)
//      case _ => new Ignore[Expense]
//    }
//
//    val entryOrder : QueryParam[Expense] = order openOr OrderBy(Expense.serialNumber, Descending)
//
//
//    Expense.findAll((By(Expense.account, account.id) :: dateClause :: entryOrder :: params.toList).toSeq : _*)
//  }

    def getLastExpenseData(acct: Account, date: Date): (Long, Double) = getByAcct(acct, Empty, Full(date)).headOption match {
        case Some(expense) => (expense.serialNumber, expense.currentBalance)
        case _ => (0, 0)
    }

//  // returns the serial and balance of the last entry before this one
//  def getLastExpenseData (acct : Account, date : Date) : (Long,BigDecimal) = {
//    // Find the last entry on or before the given date
//    val results = getByAcct(acct, Empty, Full(date), Empty, MaxRows(1))
//
//    results match {
//      case entry :: Nil => (entry.serialNumber.is, entry.currentBalance.is)
//      case Nil => (0,BigDecimal(0))
//      case _ => throw new Exception("Invalid prior entry query results") // TODO: handle this better
//    }
//  }
//
//  val updateString = String.format("update %s set %s = %s + 1, %s = %s + ? where %s >= ?",
//				   Expense.dbTableName,
//				   Expense.serialNumber.dbColumnName,
//				   Expense.serialNumber.dbColumnName,
//				   Expense.currentBalance.dbColumnName,
//				   Expense.currentBalance.dbColumnName,
//				   Expense.serialNumber.dbColumnName)
//
//  /**
//   * This method should be called before inserting the new serial number or else you'll get
//   * a duplicate serial
//   */
    def updateEntries(serial: Long, amount: Double) {
        val c = getCollection
        c(serialNumber is_> serial) = serialNumber.inc(1) and currentBalance.inc(amount)
    }

//  def updateEntries (serial : Long, amount : BigDecimal) = {
//    // Simpler to do a bulk update via SQL, unfortunately
//    DB.use(DefaultConnectionIdentifier) { conn =>
//	DB.prepareStatement(updateString, conn) { stmt =>
//	  // pass in the underlying java BigDecimal
//	  stmt.setBigDecimal(1, amount.bigDecimal)
//	  stmt.setLong(2, serial)
//	  stmt.executeUpdate()
//	}
//    }
//  }

    def findTagExpenses(search: String): List[Expense] = (this where {tags is_~ search.r} in getCollection).toList.removeDuplicates

//  def findTagExpenses (search : String) : List[Expense] =
//    findAll(In(ExpenseTag.tag,
//	       Tag.id,
//	       Like(Tag.name, search))).map(_.expense.obj.open_!).removeDuplicates
}
