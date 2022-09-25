package net.degoes.afd.ruleengine.model

import zio._

/**
 * Contains a collection of facts, whose structure is described by a phantom
 * type parameter.
 */
sealed abstract case class Facts[Types] private (private val data: Map[FactDefinition[_], Any]) {
  def get[Key <: Singleton with String, Value: FactType](pd: FactDefinition[(Key, Value)])(implicit
    subset: Types <:< (Key, Value)
  ): Value =
    data(pd).asInstanceOf[Value]

  /**
   * Returns a new facts collection with the specified fact added.
   */
  def add[Key <: Singleton with String, Value: FactType](
    pd: FactDefinition[(Key, Value)],
    value: Value
  ): Facts[Types & (Key, Value)] =
    new Facts[Types & (Key, Value)](data + (pd -> value)) {}

  private def add[Key <: Singleton with String, Value: FactType](
    name: Key,
    value: Value
  ): Facts[Types & (Key, Value)] =
    new Facts[Types & (Key, Value)](data + (FactDefinition[Key, Value](name) -> value)) {}

  object unsafe {
    def get(pd: FactDefinition[_])(implicit unsafe: Unsafe): Option[Any] = data.get(pd)
  }
}
object Facts {

  /**
   * An empty facts collection.
   */
  val empty: Facts[Any] = new Facts[Any](Map.empty) {}

  def apply[Key <: Singleton with String, Value: FactType](key: Key, value: Value): Facts[(Key, Value)] =
    empty.add(key, value)

  def apply[Key1 <: Singleton with String, Value1: FactType, Key2 <: Singleton with String, Value2: FactType](
    tuple1: (Key1, Value1),
    tuple2: (Key2, Value2)
  ): Facts[(Key1, Value1) & (Key2, Value2)] =
    empty.add[Key1, Value1](tuple1._1, tuple1._2).add[Key2, Value2](tuple2._1, tuple2._2)

  def apply[
    Key1 <: Singleton with String,
    Value1: FactType,
    Key2 <: Singleton with String,
    Value2: FactType,
    Key3 <: Singleton with String,
    Value3: FactType
  ](
    tuple1: (Key1, Value1),
    tuple2: (Key2, Value2),
    tuple3: (Key3, Value3)
  ): Facts[(Key1, Value1) & (Key2, Value2) & (Key3, Value3)] =
    empty
      .add[Key1, Value1](tuple1._1, tuple1._2)
      .add[Key2, Value2](tuple2._1, tuple2._2)
      .add[Key3, Value3](tuple3._1, tuple3._2)

  def apply[
    Key1 <: Singleton with String,
    Value1: FactType,
    Key2 <: Singleton with String,
    Value2: FactType,
    Key3 <: Singleton with String,
    Value3: FactType,
    Key4 <: Singleton with String,
    Value4: FactType
  ](
    tuple1: (Key1, Value1),
    tuple2: (Key2, Value2),
    tuple3: (Key3, Value3),
    tuple4: (Key4, Value4)
  ): Facts[(Key1, Value1) & (Key2, Value2) & (Key3, Value3) & (Key4, Value4)] =
    empty
      .add[Key1, Value1](tuple1._1, tuple1._2)
      .add[Key2, Value2](tuple2._1, tuple2._2)
      .add[Key3, Value3](tuple3._1, tuple3._2)
      .add[Key4, Value4](tuple4._1, tuple4._2)

  def apply[
    Key1 <: Singleton with String,
    Value1: FactType,
    Key2 <: Singleton with String,
    Value2: FactType,
    Key3 <: Singleton with String,
    Value3: FactType,
    Key4 <: Singleton with String,
    Value4: FactType,
    Key5 <: Singleton with String,
    Value5: FactType
  ](
    tuple1: (Key1, Value1),
    tuple2: (Key2, Value2),
    tuple3: (Key3, Value3),
    tuple4: (Key4, Value4),
    tuple5: (Key5, Value5)
  ): Facts[(Key1, Value1) & (Key2, Value2) & (Key3, Value3) & (Key4, Value4) & (Key5, Value5)] =
    empty
      .add[Key1, Value1](tuple1._1, tuple1._2)
      .add[Key2, Value2](tuple2._1, tuple2._2)
      .add[Key3, Value3](tuple3._1, tuple3._2)
      .add[Key4, Value4](tuple4._1, tuple4._2)
      .add[Key5, Value5](tuple5._1, tuple5._2)
}
