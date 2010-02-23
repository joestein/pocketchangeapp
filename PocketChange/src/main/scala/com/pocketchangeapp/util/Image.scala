package com.pocketchangeapp.util

import _root_.net.liftweb.http._
import _root_.net.liftweb.util._
import _root_.net.liftweb.mapper._
import S._
import Helpers._

import _root_.com.mongodb.gridfs._
import com.pocketchangeapp.db.Database
import com.pocketchangeapp.model._

object Image {
    /* Generally using nginx-gridfs would be much more appropriate.
     * See http://github.com/mdirolf/nginx-gridfs
     * However, as of writing this mongo-gridfs supports querying by
     * filename only -- I would like to query by oid */
    def viewImage(id : String) : LiftResponse = {
        def bufferedResponse(image: GridFSDBFile) = {
            val buffer = new java.io.ByteArrayOutputStream
            image.writeTo(buffer)
            InMemoryResponse(buffer.toByteArray, List("Content-Type" -> image.getContentType), Nil, 200)
        }

        def streamingResponse(image: GridFSDBFile) = {
            val streamProxy = new AnyRef {
                    private val s = image.getInputStream
                    def read(bytes: Array[Byte]) = s.read(bytes)
                }
            def whenComplete { }

            StreamingResponse(streamProxy, whenComplete _, image.getLength,
                              List("Content-Type" -> image.getContentType), Nil, 200)
        }

        User.currentUser match {
            case Full(user) =>
                Expense.byId(id) match {
                    case Full(e) if Full(e.owner) != User.currentUser => ForbiddenResponse()
                    case Full(e) =>
                        // can use either bufferedResponse or streamingResponse
                        e.getReceipt map streamingResponse getOrElse PlainTextResponse("Receipt has not been found", Nil, 404)
                    case _ => PlainTextResponse("No such expense item", Nil, 404)
                }
            case _ => RedirectResponse("/user_mgt/login")
        }
    }
}
