/**
 Insert copyright boilerplate here
*/

package com.pocketchangeapp.model

import _root_.java.math.MathContext
import _root_.net.liftweb.util._
import _root_.net.liftweb.mongodb.MetaMapper

import com.pocketchangeapp.db._
import com.mongodb.DBObject
import com.osinka.mongodb._
import com.osinka.mongodb.shape._

class Account(val owner: User) extends MongoObject {
    var name: String = _
    var description: String = _
    var admins: List[User] = Nil
    var viewers: List[User] = Nil
    var tags: List[String] = Nil
    var externalAccount: Option[String] = None
    var is_public: Boolean = false
    var balance: Double = 0.0
    var notes: List[String] = Nil

    def entries = Expense.getByAcct(this, Empty, Empty)
    def addAdmin(user: User) { admins ::= user }
}

object Account extends MongoObjectShape[Account] with Model[Account] { account =>
    override val collectionName = "account"
    
    override val indexes = List(name.ascending) :: Nil

    lazy val owner = Field.ref("owner", User.getCollection, _.owner)
    lazy val name = Field.scalar("name", _.name, (x: Account, v: String) => x.name = v)
    lazy val description = Field.scalar("description", _.description, (x: Account, v: String) => x.description = v)
    lazy val admins = Field.arrayRef("admins", User.getCollection, _.admins, (x: Account, l: Seq[User]) => x.admins = l.toList)
    lazy val viewers = Field.arrayRef("viewers", User.getCollection, _.viewers, (x: Account, l: Seq[User]) => x.viewers = l.toList)
    lazy val tags = Field.array("tags", _.tags, (x: Account, l: Seq[String]) => x.tags = l.toList )
    lazy val externalAccount = Field.optional("external", _.externalAccount, (x: Account, v: Option[String]) => x.externalAccount = v)
    lazy val is_public = Field.scalar("public", _.is_public, (x: Account, v: Boolean) => x.is_public = v)
    // TODO: custom type balance is BigDecimal
    lazy val balance = Field.scalar("balance", _.balance, (x: Account, v: Double) => x.balance = v)
    lazy val notes = Field.array("notes", _.notes, (x: Account, l: Seq[String]) => x.notes = l.toList )
    
    lazy val * = List(owner, name, description, admins, viewers, tags, externalAccount, is_public, balance, notes)

    override def factory(dbo: DBObject) = for {owner(user) <- Some(dbo)} yield new Account(user)

    def findByName(u: User, n: String): Box[Account] = this where {(owner is_== u) and (name is_== n)} in getCollection firstOption
}