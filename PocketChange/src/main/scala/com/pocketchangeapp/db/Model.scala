package com.pocketchangeapp.db

import _root_.net.liftweb.util.Helpers._
import _root_.net.liftweb.util._
import _root_.net.liftweb.mongodb.MetaMapper
import _root_.com.mongodb.ObjectId
import _root_.com.osinka.mongodb._
import _root_.com.osinka.mongodb.shape._
import Preamble._

trait Model[T] extends MetaMapper[T] { shape: ObjectShape[T] =>
    type FieldIndex = (ObjectField, SortOrder)

    val collectionName: String

    def indexes: List[Seq[FieldIndex]]

    def getCollection = shape.collection(Database getCollection collectionName)

    def save(u: T) {
        getCollection += u
    }

    def byId(oid: String): Box[T] = getCollection.get(new ObjectId(oid))

    def byId(oid: ObjectId): Box[T] = getCollection.get(oid)

    def one(qt: QueryTerm[T]): Box[T] = (shape where qt in getCollection).firstOption

    def one(q: ShapeQuery): Box[T] = (shape where q in getCollection).firstOption

    def all(qt: QueryTerm[T]) = shape where qt in getCollection

    def all(q: ShapeQuery) = shape where q in getCollection
}
