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

    def all(qt: QueryTerm[T])

    def all(q: ShapeQuery)

    def validate(obj: T): List[FieldError] = Nil

    trait MappedField[A] extends FieldIdentifier { field: MongoField[A] =>
        /**
         * A unique 'id' for the field for form generation
         */
        def fieldId: Option[NodeSeq] = None

        override def uniqueFieldId = Full(mongoFieldName)

        /**
         * The display name of this field (e.g., "First Name")
         */
        def displayName: String

        /**
         * Validate this field and return a list of Validation Issues
         */
        def validate(value: A): List[FieldError] = Nil

        /**
         * Attempt to figure out what the incoming value is and set the field to that value.
         */
        def setFromAny(obj: T, in: Any): A

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
        def setFromAny(obj: T, in: Any) = in match {
            case seq: Seq[_] if !seq.isEmpty => seq.map(x => setFromAny(obj,x))(0)
            case (s: String) :: _ => rep.put(obj)(Some(s)); s
            case null => rep.put(obj)(None); null
            case s: String => rep.put(obj)(Some(s)); s
            case Some(s: String) => rep.put(obj)(Some(s)); s
            case Full(s: String) => rep.put(obj)(Some(s)); s
            case None | Empty | Failure(_, _, _) => rep.put(obj)(None); null
            case o => rep.put(obj)(Some(o.toString)); o.toString
        }
    }
}
