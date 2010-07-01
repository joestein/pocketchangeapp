package com.pocketchangeapp {
package snippet {

import java.util.Date

import scala.xml.{NodeSeq,Text}

import net.liftweb.common.{Box,Empty,Full,Logger}
import net.liftweb.http.{FileParamHolder,S,SHtml,StatefulSnippet}

// Import "bind", as well as the implicits that make bind easy
import net.liftweb.util.Helpers._

import com.pocketchangeapp.db.Database
import com.pocketchangeapp.model._
import com.pocketchangeapp.util.Util

import com.pocketchangeapp.model.{Account,Expense,User}
import com.pocketchangeapp.util.Util

/* date | desc | tags | value */ 
class AddEntry extends StatefulSnippet {
  def dispatch : DispatchIt = {
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

            val amount = Expense.amount.fromString(value)

            // Rework to not throw exceptions
            val currentAccount = Account.byId(account).get

          // We need to determine the last serial number and balance for the date in question
          val (entrySerial,entryBalance) = Expense.getLastExpenseData(currentAccount, entryDate)

	  val e = new Expense(currentAccount)
          e.dateOf = entryDate
          e.serialNumber = entrySerial + 1
          e.description = desc
          e.amount = amount
          e.tags(tags)
          e.currentBalance = entryBalance + amount

	  // Add the optional receipt if it's the correct type
	  val receiptOk = fileHolder match {
	    case Full(FileParamHolder(_, null, _, _)) => true
	    case Full(FileParamHolder(_, mime, fileName, data))
		      if mime.startsWith("image/") => {
			e.uploadReceipt(mime, fileName, data)
			true
		      }
	    // If someone sends nothing...
	    case Full(FileParamHolder(_, _, "", _)) => true
	    case Full(something) => {
	      Logger(classOf[AddEntry]).error("Received invalid file attachment: " + something)
	      S.error("Invalid receipt attachment")
	      false
	    }
	    case _ => true
	  }
	      
	  (Expense.validate(e),receiptOk) match {
            case (Nil,true) => {
	      Expense.updateEntries(entrySerial + 1, amount)
              Expense save e

              val newBalance = currentAccount.balance + e.amount
	      currentAccount.balance = newBalance
              Account save currentAccount
              S.notice("Entry added!")
	      unregisterThisSnippet() // dpp: remove the statefullness of this snippet
	    }
            case (x,_) => S.error(x)
	  }
	}
      }

        bind("e", in, 
            "account" -> SHtml.select(user.editable.map(acct => (acct.id, acct.name)).toSeq, Empty, oid => account = oid),
            "dateOf" -> SHtml.text("", date = _) % ("size" -> "10"),
            "desc" -> SHtml.text("", desc = _),
            "value" -> SHtml.text("", value = _),
	     "receipt" -> SHtml.fileUpload(fph => fileHolder = Full(fph)),
            "tags" -> SHtml.text(tags, doTagsAndSubmit))
      }
    case _ => Text("")
  }
}

// Close package statements
}}
