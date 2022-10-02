package net.degoes.afd.ruleengine.graduation

import zio._
import scala.annotation._
import scala.language.implicitConversions

class FactsType[KeyValues] private (private val definitions: Chunk[FactDefinition[_]]) { self =>
  def ++[KeyValues2](that: FactsType[KeyValues2]): FactsType[KeyValues & KeyValues2] =
    new FactsType(self.definitions ++ that.definitions)

  def add[KeyValue](definition: FactDefinition[KeyValue]): FactsType[KeyValues & KeyValue] =
    new FactsType(self.definitions :+ definition)
}

object FactsType {
  val empty: FactsType[Any] = new FactsType(Chunk.empty)

  def fromFacts[KeyValues](facts: Facts[KeyValues]): FactsType[KeyValues] =
    new FactsType(facts.definitions)
}

sealed trait FactDefinition[KeyValue] { self0 =>
  type Key <: Singleton with String
  type Value

  def name: Key

  def tag: EngineType[Value]

  def self: FactDefinition.KeyValue[Key, Value] = self0.asInstanceOf[FactDefinition.KeyValue[Key, Value]]

  val singletonType: EngineType[Facts[(Key, Value)]] = EngineType.Composite(FactsType.empty.add[(Key, Value)](self))

  def get: Expr[Facts[(Key, Value)], Value] = {
    val factsExpr = Expr.input(singletonType)

    Expr.Get(factsExpr, self)
  }

  def set[In](value: Expr[In, Value]): Expr[In, Facts[(Key, Value)]] =
    Expr.fact(self, value)

  def :=[In](value: Expr[In, Value]) = set(value)

  override final def toString(): String = s"FactDefinition($name, $tag)"
}

object FactDefinition {

  type KeyValue[K <: Singleton with String, V] = FactDefinition[(K, V)] { type Key = K; type Value = V }

  def apply[N <: Singleton with String, T](name0: N, fact0: EngineType[T]): KeyValue[N, T] =
    new FactDefinition[(N, T)] {
      type Key   = N
      type Value = T
      def name: N            = name0
      def tag: EngineType[T] = fact0
    }

  def facts[N <: Singleton with String, Fields](name: N, factsType: FactsType[Fields]): KeyValue[N, Facts[Fields]] =
    FactDefinition[N, Facts[Fields]](name, EngineType.Composite(factsType))

  def prim[N <: Singleton with String, T](name0: N)(implicit tag0: PrimitiveType[T]): KeyValue[N, T] =
    new FactDefinition[(N, T)] {
      type Key   = N
      type Value = T
      def name: N            = name0
      def tag: EngineType[T] = EngineType.Primitive(tag0)
    }

  def boolean[N <: Singleton with String](name0: N): KeyValue[N, Boolean] = FactDefinition.prim[N, Boolean](name0)

  def byte[N <: Singleton with String](name0: N): KeyValue[N, Byte] = FactDefinition.prim[N, Byte](name0)

  def char[N <: Singleton with String](name0: N): KeyValue[N, Char] = FactDefinition.prim[N, Char](name0)

  def int[N <: Singleton with String](name0: N): KeyValue[N, Int] = FactDefinition.prim[N, Int](name0)

  def long[N <: Singleton with String](name0: N): KeyValue[N, Long] = FactDefinition.prim[N, Long](name0)

  def float[N <: Singleton with String](name0: N): KeyValue[N, Float] = FactDefinition.prim[N, Float](name0)

  def double[N <: Singleton with String](name0: N): KeyValue[N, Double] = FactDefinition.prim[N, Double](name0)

  def string[N <: Singleton with String](name0: N): KeyValue[N, String] = FactDefinition.prim[N, String](name0)

  def instant[N <: Singleton with String](name0: N): KeyValue[N, java.time.Instant] =
    FactDefinition.prim[N, java.time.Instant](name0)

}
