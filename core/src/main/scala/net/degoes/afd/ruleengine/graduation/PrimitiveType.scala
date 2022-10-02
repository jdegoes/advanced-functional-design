package net.degoes.afd.ruleengine.graduation

import zio._
import scala.annotation._
import scala.language.implicitConversions

  /**
   * A type class that represents the supported types for fact values.
   */
  @implicitNotFound("The type ${A} is not supported as a fact type and cannot be used for this method.")
  sealed trait PrimitiveType[A] {
    def ordering[T]: scala.math.Ordering[T]
  }

  object PrimitiveType {
    def apply[T](implicit factType: PrimitiveType[T]): PrimitiveType[T] = factType

    implicit case object IntType     extends PrimitiveType[scala.Int] {
      def ordering[T]: scala.math.Ordering[T] = Ordering[Int].asInstanceOf[Ordering[T]]
    }
    implicit case object LongType    extends PrimitiveType[scala.Long]{
      def ordering[T]: scala.math.Ordering[T] = Ordering[Long].asInstanceOf[Ordering[T]]
    }
    implicit case object StringType  extends PrimitiveType[java.lang.String]{
      def ordering[T]: scala.math.Ordering[T] = Ordering[String].asInstanceOf[Ordering[T]]
    }
    implicit case object DoubleType  extends PrimitiveType[scala.Double]{
      def ordering[T]: scala.math.Ordering[T] = Ordering[Double].asInstanceOf[Ordering[T]]
    }
    implicit case object ByteType    extends PrimitiveType[scala.Byte]{
      def ordering[T]: scala.math.Ordering[T] = Ordering[Byte].asInstanceOf[Ordering[T]]
    }
    implicit case object CharType    extends PrimitiveType[scala.Char]{
      def ordering[T]: scala.math.Ordering[T] = Ordering[Char].asInstanceOf[Ordering[T]]
    }
    implicit case object FloatType   extends PrimitiveType[scala.Float]{
      def ordering[T]: scala.math.Ordering[T] = Ordering[Float].asInstanceOf[Ordering[T]]
    }
    implicit case object BooleanType extends PrimitiveType[scala.Boolean]{
      def ordering[T]: scala.math.Ordering[T] = Ordering[Boolean].asInstanceOf[Ordering[T]]
    }
    implicit case object InstantType extends PrimitiveType[java.time.Instant]{
      def ordering[T]: scala.math.Ordering[T] = Ordering[java.time.Instant].asInstanceOf[Ordering[T]]
    }
  }
