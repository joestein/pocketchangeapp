package net.liftweb.mongodb

import _root_.com.osinka.mongodb.MongoObject

trait EasyID { self: MongoObject =>

    def id = mongoOID.map {_.toString} getOrElse ""

    override def hashCode = id.hashCode

    override def equals(other: Any) = other match {
        case that: EasyID => that.id == id
        case _ => false
    }
}
