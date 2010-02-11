/*
 * Field.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.liftweb.mongodb

import _root_.net.liftweb.http._
import _root_.net.liftweb.util.Helpers._
import _root_.net.liftweb.util._
import S._
import _root_.com.osinka.mongodb.shape._

trait LiftField extends FieldIdentifier { dbField: ObjectField =>
    /**
     * A unique 'id' for the field for form generation
     */
    def fieldId: Option[NodeSeq] = None

    def uniqueFieldId = Full(mongoFieldName)

    /**
     * The display name of this field (e.g., "First Name")
     */
    def displayName: String

    /**
     * Validate this field and return a list of Validation Issues
     */
    def validate: List[FieldError] = Nil
}
