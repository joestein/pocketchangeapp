/*
 * RestAPI.scala
 *
 * Copyright 2008-2010 Derek Chen-Becker, Marius Danciu and Tyler Wier
 * 
 */
package com.pocketchangeapp
package api

import java.text.SimpleDateFormat

import scala.xml.{Node, NodeSeq, Text}

import net.liftweb.common.{Box,Empty,Failure,Full}
import net.liftweb.json.JsonAST.{JObject,JValue}

import model._

/**
 * This object provides some conversion and formatting specific to our
 * REST API.
 */
object RestFormatters {
  /* The REST timestamp format. Not threadsafe, so we create
   * a new one each time. */
  def timestamp = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'")

  // A simple helper to generate the REST ID of an Expense
  def restId (e : Expense) = 
    "http://www.pocketchangeapp.com/api/expense/" + e.id

  // A simple helper to generate the REST timestamp of an Expense
  def restTimestamp (e : Expense) : String = 
    timestamp.format(e.dateOf)

  import net.liftweb.json.Xml
  /**
   * Generates the XML REST representation of an Expense
   */
  def toXML (e : Expense) : Node = Xml.toXml(toJSON(e)).head

  /**
   * Generates the JSON REST representation of an Expense
   */
  def toJSON (e : Expense) : JValue = {
    import net.liftweb.json.JsonDSL._
    import net.liftweb.json.JsonAST._

    ("expense" -> 
     ("id" -> restId(e)) ~
     ("date" -> restTimestamp(e)) ~
     ("description" -> e.description) ~
     ("accountname" -> e.accountName) ~
     ("accountid" -> e.account.id) ~
     ("amount" -> e.amount.toString) ~
     ("tags" -> e.tags.mkString(",")))
  }

  /*
   * Generates an Atom 1.0 feed from the last 10 Expenses for the given
   * account.
   */
  def toAtom (a : Account) : Node = {
    val entries = Expense.getByAcct(a,Empty,Empty,Full(10))
    
    <feed xmlns="http://www.w3.org/2005/Atom">
      <title>{a.name}</title>
      <id>urn:uuid:{a.id}</id>
      <updated>{entries.toList.headOption.map(restTimestamp) getOrElse
                timestamp.format(new java.util.Date)}</updated>
      { entries.flatMap(toAtom) }
    </feed>
  }

  /*
   * Generates the XML Atom representation of an Expense
   */
  def toAtom (e : Expense) : Node = 
    <entry>
      <id>urn:uuid:{restId(e)}</id>
      <title>{e.description}</title>
      <updated>{restTimestamp(e)}</updated>
      <content type="xhtml">
        <div xmlns="http://www.w3.org/1999/xhtml">
          <table>
          <tr><th>Amount</th><th>Tags</th><th>Receipt</th></tr>
          <tr><td>{e.amount.toString}</td>
              <td>{e.tags.mkString(", ")}</td>
              <td>{
                e.receipt match {
                  case Some(r) => <img src={"/image/" + e.id} />
                  case None => Text("None")
                }
              }</td></tr>
          </table>
        </div>
      </content>
    </entry>

  /**
   * Parses an Expense from a Map of string/value pairs.This method does
   * only rudimentary validation of the parsed Expense and tries to be
   * lenient on input.
   */
  def fromMap (data : scala.collection.Map[String,String], account : Account) : Box[Expense] = {
    val expense = new Expense(account)

    try {
      val fieldParsers : List[(String, String => Expense)] = {
          def setDate(date: String) = {expense.dateOf = timestamp.parse(date); expense}
          def setDescription(desc: String) = {expense.description = desc; expense}
          def setAmount(amount: String) = {expense.amount = BigDecimal(amount); expense}

          ("date", setDate _) :: ("description", setDescription _) :: ("amount", setAmount _) :: Nil
        }

      val missing = fieldParsers.flatMap { 
        field => // We invert the flatMap here to only give us missing values
          if (data.get(field._1).map(field._2).isDefined) None else Some(field._1)
      }
      
      if (missing.isEmpty) {
        data.get("tags").foreach {
          tags => expense.tags(tags.split(",").toList)
        }
        Full(expense)
      } else {
        Failure(missing.mkString("Invalid expense. Missing: ", ",", ""))
      }
    } catch {
      case pe : java.text.ParseException => Failure("Failed to parse date")
      case nfe : java.lang.NumberFormatException => Failure("Failed to parse amount")
    }
  }

  /**
   * Parses an Expense from JSON input.
   */
  def fromJSON (obj : Box[Array[Byte]], account : Account) : Box[Expense] = obj match {
    case Full(rawBytes) => {
      // We use the Scala util JSON parser here because we want to avoid parsing
      // numeric values into doubles. We'll just leave them as Strings
      import scala.util.parsing.json.JSON
      JSON.perThreadNumberParser = { in : String => in }
      
      val contents = new String(rawBytes, "UTF-8")
      JSON.parseFull(contents) match {
        case Some(data : Map[String,Any]) => {
          fromMap(data.mapValues(_.toString), account)
        }
        case other => Failure("Invalid JSON submitted: \"%s\"".format(contents))
      }
    }
    case _ => Failure("Empty body submitted")
  }

  /**
   * Parses an Expense from XML input.
   */
  def fromXML (rootNode : Box[Node], account : Account) : Box[Expense] = rootNode match {
    case Full(<expense>{parameters @ _*}</expense>) => {
      var data = Map[String,String]()

      for(parameter <- parameters) { 
        parameter match {
          case <date>{date}</date> => data += "date" -> date.text
          case <description>{description}</description> => 
            data += "description" -> description.text
          case <amount>{amount}</amount> => data += "amount" -> amount.text
          case <tags>{ tags }</tags> => data += "tags" -> tags.text
          case _ => // Ignore (could be whitespace)
        }
      }
      
      fromMap(data, account)
    }
    case other => Failure("Missing root expense element")
  }
}
