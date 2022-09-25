package net.degoes.afd.ruleengine.model

sealed trait FactDefinition[KeyValue] { self =>
  type Key <: Singleton with String
  type Value

  def name: Key

  def paramType: FactType[Value]

  override final def toString(): String = s"FactDefinition($name, $paramType)"
}
object FactDefinition {
  def apply[N <: Singleton with String, T](name0: N)(implicit paramType0: FactType[T]): FactDefinition[(N, T)] =
    new FactDefinition[(N, T)] {
      type Key   = N
      type Value = T
      def name: N                = name0
      def paramType: FactType[T] = paramType0
    }

  def boolean[N <: Singleton with String](name0: N): FactDefinition[(N, Boolean)] = FactDefinition[N, Boolean](name0)

  def double[N <: Singleton with String](name0: N): FactDefinition[(N, Double)] = FactDefinition[N, Double](name0)

  def int[N <: Singleton with String](name0: N): FactDefinition[(N, Int)] = FactDefinition[N, Int](name0)

  def instant[N <: Singleton with String](name0: N): FactDefinition[(N, java.time.Instant)] =
    FactDefinition[N, java.time.Instant](name0)

  def string[N <: Singleton with String](name0: N): FactDefinition[(N, String)] = FactDefinition[N, String](name0)
}
