package com.osinka.mongodb.shape

import java.math.{BigDecimal => JBigD}
import java.math.{BigInteger => JBigI}

trait BigDecimalFields[T, QueryType] extends ShapeFields[T, QueryType] {

    trait BigDecimalContent extends ScalarContent[BigDecimal] { self: MongoField[BigDecimal] =>
        override def serialize(a: BigDecimal) = Some( (a*100).setScale(0).intValue )
        override def deserialize(v: Any): Option[BigDecimal] = v match {
            case i: Int => Some( new BigDecimal( new JBigD(JBigI.valueOf(i), 2) ) )
            case _ => None
        }
    }

    class BigDecimalField(override val mongoFieldName: String, val g: T => BigDecimal, val p: Option[(T,BigDecimal) => Unit])
            extends MongoScalar[BigDecimal] with BigDecimalContent with ScalarFieldModifyOp[BigDecimal] {
        override val rep = Represented.by(g, p)
        override def canEqual(other: Any): Boolean = other.isInstanceOf[BigDecimalField]
    }
}
