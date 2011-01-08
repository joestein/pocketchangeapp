package com.pocketchangeapp.db

import net.liftweb.common.Box
import net.liftweb.util.Helpers._
import net.liftweb.mongodb.MetaMapper

import org.bson.types.ObjectId
import com.osinka.mongodb._
import com.osinka.mongodb.shape._

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

    def one(qt: QueryTerm[T]): Box[T] = (shape where qt in getCollection).headOption

    def one(q: ShapeQuery): Box[T] = (shape where q in getCollection).headOption

    def all(qt: QueryTerm[T]): Iterable[T] = shape where qt in getCollection

    def all(q: ShapeQuery): Iterable[T] = shape where q in getCollection
}
