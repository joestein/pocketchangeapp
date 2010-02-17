package net.liftweb.mongodb

import scala.xml._
import _root_.net.liftweb.http._
import _root_.net.liftweb.util.Helpers._
import _root_.net.liftweb.util._
import S._
import _root_.com.mongodb.ObjectId
import _root_.com.osinka.mongodb.shape._

trait MetaMapper[T] { self: ObjectShape[T] =>

    def save(u: T): Unit

    def byId(oid: String): Box[T]

    def byId(oid: ObjectId): Box[T]

    def one(qt: QueryTerm[T]): Box[T]

    def one(q: ShapeQuery): Box[T]

    def all(qt: QueryTerm[T]): Iterable[T]

    def all(q: ShapeQuery): Iterable[T]

    def validation: List[T => List[FieldError]] = Nil
    
    def validate(obj: T): List[FieldError] =
        fieldList.flatMap {
            case f: MappedField[_] => f.validate(obj)
            case _ => Nil
        } ::: validation.flatMap {
            case pf: PartialFunction[T, List[FieldError]] =>
                if (pf.isDefinedAt(obj)) pf(obj)
                else Nil
            case f => f(obj)
        }

    trait MappedField[A] extends FieldIdentifier { field: MongoScalar[A] =>
        /**
         * A unique 'id' for the field for form generation
         */
        def fieldId: Option[NodeSeq] = None

        override def uniqueFieldId = Empty // TODO: (requires displaying field errors): Full(mongoFieldName)

        /**
         * The display name of this field (e.g., "First Name")
         */
        def displayName: String

        def validations: List[A => List[FieldError]] = Nil

        /**
         * Validate this field and return a list of Validation Issues
         */
        def validate(obj: T): List[FieldError] = rep.get(obj) map { cv =>
            validations.flatMap {
                case pf: PartialFunction[A, List[FieldError]] =>
                    if (pf.isDefinedAt(cv)) pf(cv)
                    else Nil
                case f => f(cv)
            }
        } getOrElse Nil

        /**
         * Attempt to figure out what the incoming value is and set the field to that value.
         */
        def setFromAny(obj: T, in: Any): Boolean

        def toFormAppendedAttributes: MetaData =
            if (Props.mode == Props.RunModes.Test)
                new PrefixedAttribute("lift", "field_name", Text(calcFieldName), Null)
            else Null

        def calcFieldName: String = mongoFieldName

        final def toForm(obj: T): Box[NodeSeq] = {
            def mf(in: Node): NodeSeq = in match {
                case g: Group => g.nodes.flatMap(mf)
                case e: Elem => e % toFormAppendedAttributes
                case other => other
            }

            _toForm(obj).map(_.flatMap(mf) )
        }

        /**
        * Create an input field for the item
        */
        def _toForm(obj: T): Box[NodeSeq] =
            S.fmapFunc({s: List[String] => setFromAny(obj, s)}){funcName =>
                Full(<input type='text' id={fieldId}
                    name={funcName} lift:gc={funcName}
                    value={rep.get(obj) map { _.toString } getOrElse ""}/>)
        }
    }

    trait MappedString extends MappedField[String] { field: MongoScalar[String] =>
        def setFromAny(obj: T, in: Any): Boolean = in match {
            case seq: Seq[_] if !seq.isEmpty => seq.map(x => setFromAny(obj,x))(0); true
            case (s: String) :: _ => rep.put(obj)(Some(s)); true
            case null => rep.put(obj)(None); false
            case s: String => rep.put(obj)(Some(s)); true
            case Some(s: String) => rep.put(obj)(Some(s)); true
            case Full(s: String) => rep.put(obj)(Some(s)); true
            case None | Empty | Failure(_, _, _) => rep.put(obj)(None); false
            case o => rep.put(obj)(Some(o.toString)); true
        }
    }
}
