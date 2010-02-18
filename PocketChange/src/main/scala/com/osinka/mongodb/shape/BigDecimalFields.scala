package com.osinka.mongodb.shape

import java.math.{BigDecimal => JBigD}
import java.math.{BigInteger => JBigI}

trait BigDecimalFields[T, QueryType] extends ShapeFields[T, QueryType] {

    trait BigDecimalContent extends ScalarContent[BigDecimal] { self: MongoField[BigDecimal] =>
        def scale: Int
        private lazy val mult = List.make(scale, 10) reduceLeft{_ * _}

        override def serialize(a: BigDecimal) = {
            assert(a.scale == scale)
            Some( (a*mult).setScale(0).intValue )
        }

        override def deserialize(v: Any): Option[BigDecimal] = v match {
            case i: Int => Some( new BigDecimal( new JBigD(JBigI.valueOf(i), scale) ) )
            case _ => None
        }
    }

    class BigDecimalField(override val mongoFieldName: String, override val scale: Int, val g: T => BigDecimal, val p: Option[(T,BigDecimal) => Unit])
            extends MongoScalar[BigDecimal] with BigDecimalContent with ScalarFieldModifyOp[BigDecimal] {
        override val rep = Represented.by(g, p)
        override def canEqual(other: Any): Boolean = other.isInstanceOf[BigDecimalField]

        lazy val zero = new BigDecimal( new JBigD(JBigI.valueOf(0), scale) )
        def fromString(value: String) = BigDecimal(value).setScale(scale, BigDecimal.RoundingMode.ROUND_FLOOR)
    }
}
