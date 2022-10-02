package net.degoes.afd.ruleengine.graduation

import zio._
import scala.annotation._
import scala.language.implicitConversions

/**
 * Contains a collection of facts, whose structure is described by a phantom
 * type parameter.
 */
sealed abstract case class Facts[+Types] private (private val data: Map[FactDefinition[_], Any]) { self =>
  def ++[Types2](that: Facts[Types2]): Facts[Types & Types2] =
    new Facts[Types & Types2](data ++ that.data) {}

  def definitions: Chunk[FactDefinition[_]] = Chunk.fromIterable(data.keys)

  override def equals(that: Any): Boolean =
    that match {
      case that: Facts[_] => self.data == that.data
      case _              => false
    }

  def lessThan(that: Facts[_]) = false

  def get[Key <: Singleton with String, Value: PrimitiveType](pd: FactDefinition[(Key, Value)])(implicit
    subset: Types <:< (Key, Value)
  ): Value =
    data(pd).asInstanceOf[Value]

  /**
   * Returns a new facts collection with the specified primitive fact added.
   */
  def add[Key <: Singleton with String, Value](
    pd: FactDefinition.KeyValue[Key, Value],
    value: Value
  ): Facts[Types & (Key, Value)] =
    new Facts[Types & (Key, Value)](data + (pd -> value)) {}

  /**
   * Returns a new facts collection with the specified fact added.
   */
  def add[Key <: Singleton with String, Types2](
    pd: FactDefinition.KeyValue[Key, Facts[Types2]],
    value: Facts[Types2]
  ): Facts[Types & (Key, Facts[Types2])] =
    new Facts[Types & (Key, Facts[Types2])](data + (pd -> value)) {}

  object unsafe {
    def get(pd: FactDefinition[_])(implicit unsafe: Unsafe): Any = data(pd)
  }
}
object Facts {

  /**
   * An empty facts collection.
   */
  val empty: Facts[Any] = new Facts[Any](Map.empty) {}

  def engineTypeOf[Types](facts: Facts[Types]): EngineType[Facts[Types]] =
    EngineType.Composite(FactsType.fromFacts(facts))

}
