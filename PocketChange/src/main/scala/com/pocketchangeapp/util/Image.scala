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
    def viewImage(id : String) : LiftResponse = {
        def imageResponse(image: GridFSDBFile) = {
            val os = new java.io.ByteArrayOutputStream
            image.writeTo(os)
            // TODO: streaming response
//            StreamingResponse(image.getInputStream,
//                              () => Log.info("File "+image.getFilename+" returned"),
//                              image.getLength,
//                              List("Content-Type" -> image.getContentType), Nil, 200)
            InMemoryResponse(os.toByteArray, List("Content-Type" -> image.getContentType), Nil, 200)
        }

        User.currentUser match {
            case Full(user) =>
                Expense.byId(id) match {
                    case Full(e) if Full(e.owner) != User.currentUser => ForbiddenResponse()
                    case Full(e) =>
                        e.getReceipt map imageResponse getOrElse PlainTextResponse("Receipt has not been found", Nil, 404)
                    case _ => PlainTextResponse("No such expense item", Nil, 404)
                }
            case _ => RedirectResponse("/user_mgt/login")
        }
    }
}
