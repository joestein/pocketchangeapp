package com.pocketchangeapp.snippet

import scala.xml._
import net.liftweb._
import http._
import util._
import S._
import SHtml._
import scala.xml._
import Helpers._

import com.pocketchangeapp.model._
import com.pocketchangeapp.util.Util

import java.util.Date

/* date | desc | tags | value */ 
class AddEntry extends StatefulSnippet {
  def dispatch = {
    case "addentry" => add _
  }

  var account : String = _
  var date = ""
  var desc = ""
  var value = ""
  var tags = S.param("tag") openOr ""
  var fileHolder : Box[FileParamHolder] = Empty
  
  def add(in: NodeSeq): NodeSeq = User.currentUser match {
    case Full(user) if user.editable.size > 0 => {

        def doTagsAndSubmit(t: String) {
          tags = t
          if (tags.trim.length == 0) error("We're going to need at least one tag.")
          else {
            /* Get the date correctly, add the datepicker: comes in as yyyy/mm/dd */
            val entryDate = Util.slashDate.parse(date)

            val amount = value.toFloat

            // Rework to not throw exceptions
            val currentAccount = Account.byId(account).get

            // We need to determine the last serial number and balance for the date in question
            val (entrySerial,entryBalance) = Expense.getLastExpenseData(currentAccount, entryDate)

	  val e = new Expense(currentAccount)
          e.dateOf = entryDate
          e.serialNumber = entrySerial + 1
          e.description = desc
          e.amount = value.toFloat
          e.tags(tags)
          e.currentBalance = entryBalance + amount

	  // Add the optional receipt if it's the correct type
          val receiptOk = true
          /*
	  val receiptOk = fileHolder match {
	    case Full(FileParamHolder(_, null, _, _)) => true
	    case Full(FileParamHolder(_, mime, _, data)) 
		      if mime.startsWith("image/") => {
			e.receipt(data).receiptMime(mime)
			true
		      }
	    // If someone sends nothing...
	    case Full(FileParamHolder(_, _, "", _)) => true
	    case Full(something) => {
	      Log.error("Received file attachment: " + something)
	      S.error("Invalid receipt attachment")
	      false
	    }
	    case _ => true
	  }
          */
	      
	  (Expense.validate(e),receiptOk) match {
            case (Nil,true) => {
	      Expense.updateEntries(entrySerial + 1, amount)
              Expense save e

              val newBalance = currentAccount.balance + e.amount
	      currentAccount.balance = newBalance
              Account save currentAccount
              notice("Entry added!")
	      unregisterThisSnippet() // dpp: remove the statefullness of this snippet
	    }
            case (x,_) => error(x)
	  }
	}
      }

        bind("e", in, 
            "account" -> select(user.editable.map(acct => (acct.id.get, acct.name)).toSeq, Empty, oid => account = oid),
            "dateOf" -> text("", date = _) % ("size" -> "10"),
            "desc" -> text("", desc = _),
            "value" -> text("", value = _),
	     "receipt" -> fileUpload(fph => fileHolder = Full(fph)),
            "tags" -> text(tags, doTagsAndSubmit))
      }
    case _ => Text("")
  }
}

