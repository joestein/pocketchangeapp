package com.pocketchangeapp.model

import java.math.MathContext
import net.liftweb.common.{Box,Empty}
import net.liftweb.mongodb._

import com.pocketchangeapp.db._
import org.bson.types.ObjectId
import com.mongodb.DBObject
import com.osinka.mongodb._
import com.osinka.mongodb.shape._

class Account(val owner: User) extends MongoObject with EasyID {
    // The name of this account
    var name: String = ""
  
    // A description of this account
    var description: String = ""

    // Which users, besides the owner, are allowed to modify this account?
    var admins: List[User] = Nil

    // Which users, besides the owner and admins, are allowed to view this account?
    var viewers: List[User] = Nil

    // An optional external account identifier
    var externalAccount: Option[String] = None

    /* Setting this to true allows anyone to view this account and its activity.
     * This is read-only access. */
    var is_public: Boolean = false

    // The balance has up to 16 digits and 2 decimal places
    var balance: BigDecimal = Account.balance.zero

    // Optional notes about the account
    var notes: List[String] = Nil

    // All tags used on expenses for this account
    def tags = entries.flatMap(_.tags).toList.sorted.distinct

    // The actual expense entries
    def entries = Expense.getByAcct(this, Empty, Empty, Empty)
    def addAdmin(user: User) { admins ::= user }

    // This method checks view access by a particular user
    def isViewableBy (user : Box[User]) : Boolean = {
      is_public ||
      user.map(_.allAccounts.contains(this)).openOr(false)
    }

    // This method checks edit access by a particular user
    def isEditableBy (user : Box[User]) : Boolean =
      user.map(_.editable.contains(this)).openOr(false)
}

object Account extends MongoObjectShape[Account] with Model[Account] with BigDecimalFields[Account,Account]  { account =>
    override val collectionName = "account"
    
    override val indexes: List[Seq[FieldIndex]] = List(name.ascending) :: Nil

    lazy val owner = Field.ref("owner", User.getCollection, _.owner)
    lazy val name = Field.scalar("name", _.name, (x: Account, v: String) => x.name = v)
    lazy val description = Field.scalar("description", _.description, (x: Account, v: String) => x.description = v)
    lazy val admins = Field.arrayRef("admins", User.getCollection, _.admins, (x: Account, l: Seq[User]) => x.admins = l.toList)
    lazy val viewers = Field.arrayRef("viewers", User.getCollection, _.viewers, (x: Account, l: Seq[User]) => x.viewers = l.toList)
    lazy val externalAccount = Field.optional("external", _.externalAccount, (x: Account, v: Option[String]) => x.externalAccount = v)
    lazy val is_public = Field.scalar("public", _.is_public, (x: Account, v: Boolean) => x.is_public = v)
    object balance extends BigDecimalField("balance", 2, _.balance, Some((x: Account, v: BigDecimal) => x.balance = v))
    lazy val notes = Field.array("notes", _.notes, (x: Account, l: Seq[String]) => x.notes = l.toList )
    
    lazy val * = List(owner, name, description, admins, viewers, externalAccount, is_public, balance, notes)

    override def factory(dbo: DBObject) = for {owner(user) <- Some(dbo)} yield new Account(user)

    def findByName(u: User, n: String): Box[Account] = this where {(owner is_== u) and (name is_== n)} in getCollection headOption

    /**
     * Define an extractor that can be used to locate an Account based
     * on its ID.
     */
    def unapply (id : String) : Option[Account] = {
        this where {oid is_== new ObjectId(id)} in getCollection headOption
    }
}
