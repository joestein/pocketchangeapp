package com.pocketchangeapp.db

import net.liftweb.util.LoanWrapper
import com.mongodb._
import com.pocketchangeapp.model._
import com.osinka.mongodb._
import Preamble._

object Database {
    val Host = "localhost"
    val Port = 27017
    val Name = "pocketchange"

    val Models = Account :: Expense :: Nil

    lazy val mongo = new Mongo(Host, Port).getDB(Name)

    def getCollection(name: String) = mongo.getCollection(name)

    def ensureIndexes() {
        for {m <- Models
             mongodbColl = m.getCollection.underlying
             fields <- m.indexes} {

            val indexesRequired: DBObject =
                (fields map {i => i._1.mongoFieldName -> i._2.mongoOrder}
                    foldLeft Map.empty[String,Any]) {(m,f) => m + f}
            mongodbColl.ensureIndex( indexesRequired )
        }
    }

    object inLiftRequest extends LoanWrapper {
        def apply[T] (f : => T): T =
            try {
                mongo.requestStart()
                f
            } finally {
                mongo.requestDone()
            }
    }
}
