package com.pocketchangeapp.db

import com.mongodb._
import com.pocketchangeapp.model._
import com.osinka.mongodb._
import Preamble._

object Database {
    val Host = "localhost"
    val Port = 27017
    val Name = "pocketchange"

    val Models = Account :: Nil

    lazy val mongo = new Mongo(Host, Port).getDB(Name)

    def getCollection(name: String) = mongo.getCollection(name)

    def ensureIndexes() {
        for (m <- Models; fields <- m.indexes)
            m.coll.underlying.ensureIndex( (fields foldLeft Map.empty) { (m,f) => m + f} )
    }
}
