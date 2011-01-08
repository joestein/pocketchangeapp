package com.pocketchangeapp.db

import net.liftweb.util._
import com.mongodb._
import com.mongodb.gridfs._
import com.pocketchangeapp.model._
import org.bson.types.ObjectId
import com.osinka.mongodb._

object Database {
    val Host       = "localhost"
    val Port       = 27017
    val Name       = "pocketchange"
    val GridBucket = "receipts"

    val Models = Account :: Expense :: Nil

    lazy val mongo = new Mongo(Host, Port).getDB(Name)
    lazy val gridFS = new GridFS(mongo)

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

    def getBinary(oid: ObjectId) = Option(gridFS.find(oid))

    def saveBinary(mime: String, fileName: String, data: Array[Byte]) = {
        val file = gridFS.createFile(data)
        file.setContentType(mime)
        file.setFilename(fileName)
        file.save
        file.getId.asInstanceOf[ObjectId]
    }

    def removeBinary(oid: ObjectId) { gridFS.remove(oid) }
}
