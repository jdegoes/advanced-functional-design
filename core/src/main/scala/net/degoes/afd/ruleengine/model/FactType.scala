package net.degoes.afd.ruleengine.model

import scala.annotation.implicitNotFound

/**
 * A type class that represents the supported types for fact values.
 */
@implicitNotFound("The type ${A} is not supported as a fact type and cannot be used for this method.")
sealed trait FactType[A]
object FactType {
  def apply[T](implicit factType: FactType[T]): FactType[T] = factType

  implicit case object Int     extends FactType[scala.Int]
  implicit case object String  extends FactType[java.lang.String]
  implicit case object Double  extends FactType[scala.Double]
  implicit case object Boolean extends FactType[scala.Boolean]
  implicit case object Instant extends FactType[java.time.Instant]
}
