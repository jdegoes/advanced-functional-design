package net.degoes.afd.ruleengine.graduation

import zio._
import scala.annotation._
import scala.language.implicitConversions

sealed trait EngineType[A] {
  def equals(left: A, right: A): Boolean
  def lessThan(left: A, right: A): Boolean
}
object EngineType {

  final case class Primitive[A](primitiveType: PrimitiveType[A]) extends EngineType[A] {
    def equals(left: A, right: A): Boolean = primitiveType.ordering.equiv(left, right)

    def lessThan(left: A, right: A): Boolean = primitiveType.ordering.lt(left, right)
  }

  final case class Composite[Fields](factsType: FactsType[Fields]) extends EngineType[Facts[Fields]] {
    def equals(left: Facts[Fields], right: Facts[Fields]): Boolean = left == right

    def lessThan(left: Facts[Fields], right: Facts[Fields]): Boolean = left.lessThan(right)
  }

  def fromPrimitive[A](implicit primitiveType: PrimitiveType[A]): EngineType[A] =
    Primitive(primitiveType)

  def fromFacts[Types](facts: Facts[Types]): EngineType[Facts[Types]] =
    EngineType.Composite(FactsType.fromFacts(facts))
}
