package com.pocketchangeapp.model

import _root_.net.liftweb.util._
import _root_.net.liftweb.mongodb._

import com.mongodb.{DBObject,ObjectId}
import com.pocketchangeapp.db._
import com.osinka.mongodb._
import com.osinka.mongodb.shape._
import Preamble._

class User extends MegaProtoUser {
    def accounts = Account where {Account.owner is_== this} in Account.getCollection

    def administred = Account where {Account.admins has this} in Account.getCollection

    def editable = accounts ++ administred

    def viewed = Account where {Account.viewers has this} in Account.getCollection

    def allAccounts = accounts ++ administred ++ viewed
}

object User extends MetaMegaProtoUser[User] with Model[User] {
    override val collectionName = "user"
    override val indexes = Nil

    override lazy val * = super.*

    override def factory(dbo: DBObject) = Some(create)

    // Access methods, calling collection
    override def create = new User

    // Just for testing purposes. In production we remove this
    override def skipEmailValidation = true

    // Spruce up the forms a bit
    override def screenWrap =
        Full(<lift:surround with="default" at="content"><div id="formBox"><lift:bind /></div></lift:surround>)
}