/*
 * RestAPI.scala
 */
package com.pocketchangeapp.api

import net.liftweb._
import http._
import rest._
import util._
import mapper._
import Helpers._
import net.liftweb.util.Helpers.toLong

import scala.xml.{Node, NodeSeq}

import com.osinka.mongodb.Preamble._

import com.pocketchangeapp.model._

object RestAPI extends XMLApiHelper{
  def dispatch: LiftRules.DispatchPF = {     
    case Req("api" :: "expense" :: eid :: Nil, "", GetRequest) => () => showExpenseXml(eid) // old
    case Req("api" :: "expense" :: eid :: "xml" :: Nil, "", GetRequest) => () => showExpenseXml(eid) // new
    case Req("api" :: "expense" :: eid :: "atom" :: Nil, "", GetRequest) => () => showExpenseAtom(eid) // new

    case r @ Req("api" :: "expense" :: Nil, "", PutRequest) => () => addExpense(r)
    // Invalid API request - route to our error handler
    case Req("api" :: x :: Nil, "", _) => failure _ 

  }


  def failure(): LiftResponse = {
    val ret: Box[NodeSeq] = Full(<op id="FAILURE"></op>)
    NotFoundResponse()
  }

    // final wrap of responses
  def createTag(in: NodeSeq) = {
    println("[CreateTag] " + in)
    <pca_api>{in}</pca_api>

  }

  def wrapXmlBody(in: NodeSeq) = {
    println("[WrapXMLBody]: " + in)
    <pca_api>{in}</pca_api>
  }

  // reacts to the GET Request
  def showExpenseXml(eid: String): LiftResponse = {
    val e: Box[NodeSeq] = for(e <- Expense byId eid) yield {
      <operation id="show_expense" success="true">{e.toXML}</operation>

    }
    e
  }

  // reacts to the GET Request
  def showExpenseAtom(eid: String): AtomResponse = {
    val e: Box[Node] = for(e <- Expense.byId(eid)) yield {
      e.toAtom

    }
    AtomResponse(e.open_!)
  }




  private def getAccount(e: String, n: String): Account = {
    val u = User one {User.email is_== e} open_!

    Account.findByName(u, n) getOrElse new Account(u)
  }


  // reacts to the PUT Request
  def addExpense(req: Req): LiftResponse = {
    var tempEmail = ""
    var tempPass = ""
    var tempAccountName = ""
    var tempDateOf: java.util.Date = null
    var tempAmount: BigDecimal = 0
    var tempDescription = ""

    req.xml match {
      case Full(<expense>{parameters @ _*}</expense>) => {
        for(parameter <- parameters){ parameter match {
          case <email>{email}</email> => tempEmail = email.text
          case <password>{password}</password> => tempPass = password.text
          case <accountName>{name}</accountName> => tempAccountName = name.text
          case <dateOf>{dateof}</dateOf> => tempDateOf = new java.util.Date(dateof.text)
          case <amount>{value}</amount> => tempAmount = Expense.amount.fromString(value.text)
          case <desc>{description}</desc> => tempDescription = description.text
          case _ =>
        }
      }

      try {
        val u:User = User one {User.email is_== tempEmail} match {
          case Full(user) if user.validated && user.passwordMatch_?(tempPass) =>
            User.logUserIn(user)
            user
          case _ => new User
        }

        val currentAccount = Account one {Account.owner.is_==(u) and Account.name.is_==(tempAccountName)} open_!
        val expense = new Expense(currentAccount)
        expense.dateOf = tempDateOf
        expense.amount = tempAmount
        expense.description = tempDescription

        val (entrySerial,entryBalance) = Expense.getLastExpenseData(currentAccount, expense.dateOf)

        expense.serialNumber = entrySerial + 1
        expense.tags ::= "api"
        expense.currentBalance = entryBalance + expense.amount

        Expense.validate(expense) match {
          case Nil =>
            Expense.updateEntries(entrySerial + 1, expense.amount)
            Expense.save(expense)
            println(currentAccount.name)
            //val acct = Account.find(currentAccount.name.is).open_!
            val newBalance = currentAccount.balance + expense.amount
            currentAccount.balance = newBalance
            Account.save(currentAccount)

            CreatedResponse(wrapXmlBody(<operation id="add_expense" success="true"></operation>), "text/xml")
          case _ =>
            CreatedResponse(wrapXmlBody(<operation id="add_expense" success="false"></operation>), "text/xml")
        }
      }
      catch {
        case e => Log.error("Could not add expense", e); BadResponse()
      }
    }
    case _ => Log.error("Request was malformed"); BadResponse()
    }
  }

}
