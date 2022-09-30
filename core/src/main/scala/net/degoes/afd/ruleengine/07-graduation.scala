/**
 * After much work, you have a rule engine you can be proud of: generic,
 * compositional, type-safe, declarative, persistable, efficient, and even
 * suitable for streaming applications.
 *
 * Now, it's time you put that rule engine to use. In this section, you will use
 * your rule engine to build out an application of your choosing, possibly
 * chosen from among the motivating examples.
 */
package net.degoes.afd.ruleengine

import zio._
import scala.annotation._
import scala.language.implicitConversions
/**
 * Use your rule engine to build out an application of your choosing. You should
 * have a main function that executes a sample rule set on some sample data, and
 * prints out or executes the actions produced.
 */
object graduation {

  sealed trait Numeric[A]
  object Numeric {
    implicit case object ByteIsNumeric    extends Numeric[Byte]
    implicit case object CharIsNumeric    extends Numeric[Char]
    implicit case object IntIsNumeric     extends Numeric[Int]
    implicit case object LongIsNumeric    extends Numeric[Long]
    implicit case object FloatIsNumeric   extends Numeric[Float]
    implicit case object DoubleIsNumeric  extends Numeric[Double]

}

  /**
   * A type class that represents the supported types for fact values.
   */
  @implicitNotFound("The type ${A} is not supported as a fact type and cannot be used for this method.")
  sealed trait PrimitiveType[A]
  object PrimitiveType {
    def apply[T](implicit factType: PrimitiveType[T]): PrimitiveType[T] = factType

    implicit case object Int     extends PrimitiveType[scala.Int]
    implicit case object Long    extends PrimitiveType[scala.Long]
    implicit case object String  extends PrimitiveType[java.lang.String]
    implicit case object Double  extends PrimitiveType[scala.Double]
    implicit case object Byte    extends PrimitiveType[scala.Byte]
    implicit case object Char    extends PrimitiveType[scala.Char]
    implicit case object Float   extends PrimitiveType[scala.Float]
    implicit case object Boolean extends PrimitiveType[scala.Boolean]
    implicit case object Instant extends PrimitiveType[java.time.Instant]
  }

  sealed trait Expr[-In, +Out] { self =>
    
    final def ++[In1 <: In, Fields1, Fields2](that: Expr[In1, Facts[Fields2]])(implicit ev: Out <:< Facts[Fields1]): Expr[In1, Facts[Fields1 & Fields2]] =
      Expr.CombineFacts(self.widen[Facts[Fields1]], that)

    final def +[In1 <: In, Out1 >: Out](that: Expr[In1, Out1])(implicit tag: Numeric[Out1]): Expr[In1, Out1] =
      Expr.BinaryNumericOp(self.widen, that, Expr.NumericBinOpType.Add, tag)

    final def -[In1 <: In, Out1 >: Out](that: Expr[In1, Out1])(implicit tag: Numeric[Out1]): Expr[In1, Out1] =
      Expr.BinaryNumericOp(self.widen, that, Expr.NumericBinOpType.Subtract, tag)

    final def *[In1 <: In, Out1 >: Out](that: Expr[In1, Out1])(implicit tag: Numeric[Out1]): Expr[In1, Out1] =
      Expr.BinaryNumericOp(self.widen, that, Expr.NumericBinOpType.Multiply, tag)

    final def /[In1 <: In, Out1 >: Out](that: Expr[In1, Out1])(implicit tag: Numeric[Out1]): Expr[In1, Out1] =
      Expr.BinaryNumericOp(self.widen, that, Expr.NumericBinOpType.Divide, tag)

    final def %[In1 <: In, Out1 >: Out](that: Expr[In1, Out1])(implicit tag: Numeric[Out1]): Expr[In1, Out1] =
      Expr.BinaryNumericOp(self.widen, that, Expr.NumericBinOpType.Modulo, tag)

    final def &&[In1 <: In](that: Expr[In1, Boolean])(implicit ev: Out <:< Boolean): Expr[In1, Boolean] =
      Expr.And(self.widen, that)

    final def ||[In1 <: In](that: Expr[In1, Boolean])(implicit ev: Out <:< Boolean): Expr[In1, Boolean] =
      Expr.Or(self.widen, that)

    final def !=[In1 <: In, Out1 >: Out](that: Expr[In1, Out1]): Expr[In1, Boolean] =
      !(self === that)

    final def <[In1 <: In, Out1 >: Out](that: Expr[In1, Out1]): Expr[In1, Boolean] =
      Expr.LessThan(self, that)

    final def <=[In1 <: In, Out1 >: Out](that: Expr[In1, Out1]): Expr[In1, Boolean] =
      Expr.LessThan(self, that) || (self === that)

    final def >[In1 <: In, Out1 >: Out](that: Expr[In1, Out1]): Expr[In1, Boolean] =
      !(self <= that)

    final def >=[In1 <: In, Out1 >: Out](that: Expr[In1, Out1]): Expr[In1, Boolean] =
      !(self < that)

    final def ===[In1 <: In, Out1 >: Out](that: Expr[In1, Out1]): Expr[In1, Boolean] =
      Expr.EqualTo(self, that)

    final def >>>[Out2](that: Expr[Out, Out2]): Expr[In, Out2] =
      Expr.Pipe(self, that)

    final def unary_!(implicit ev: Out <:< Boolean): Expr[In, Boolean] =
      Expr.Not(self.widen)

    final def widen[Out2](implicit ev: Out <:< Out2): Expr[In, Out2] = self.asInstanceOf[Expr[In, Out2]]

  }

  object Expr {

    final case class Fact[In, K <: Singleton with String, V](
      factDef: FactDefinition.KeyValue[K, V], 
      value: Expr[In, V]) extends Expr[In, Facts[(K, V)]]
      final case class CombineFacts[In, V1, V2](
        left: Expr[In, Facts[V1]],
        right: Expr[In, Facts[V2]]
      ) extends Expr[In, Facts[V1 & V2]]
    final case class Constant[Out](value: Out, tag: PrimitiveType[Out])         extends Expr[Any, Out]
    final case class And[In](left: Expr[In, Boolean], right: Expr[In, Boolean]) extends Expr[In, Boolean]
    final case class Or[In](left: Expr[In, Boolean], right: Expr[In, Boolean])  extends Expr[In, Boolean]
    final case class Not[In](condition: Expr[In, Boolean])                      extends Expr[In, Boolean]
    final case class EqualTo[In, Out](lhs: Expr[In, Out], rhs: Expr[In, Out])   extends Expr[In, Boolean]
    final case class LessThan[In, Out](lhs: Expr[In, Out], rhs: Expr[In, Out])  extends Expr[In, Boolean]
    final case class Input[K <: Singleton with String, V](factDef: FactDefinition.KeyValue[K, V])
        extends Expr[(K, V), V] // TODO : split into READ & GET operators
    final case class Pipe[In, Out1, Out2](left: Expr[In, Out1], right: Expr[Out1, Out2]) extends Expr[In, Out2]
    final case class BinaryNumericOp[In, Out](
      lhs: Expr[In, Out],
      rhs: Expr[In, Out],
      op: NumericBinOpType,
      tag: Numeric[Out]
    ) extends Expr[In, Out]

    sealed trait NumericBinOpType
    object NumericBinOpType {
      case object Add      extends NumericBinOpType
      case object Subtract extends NumericBinOpType
      case object Multiply extends NumericBinOpType
      case object Divide   extends NumericBinOpType
      case object Modulo   extends NumericBinOpType
    }

    implicit def apply[Out](out: Out)(implicit tag: PrimitiveType[Out]): Expr[Any, Out] = Constant(out, tag)

    def input[K <: Singleton with String, V](factDef: FactDefinition.KeyValue[K, V]): Expr[(K, V), V] =
        Input(factDef)

    def fact[In, K <: Singleton with String, V](factDef: FactDefinition.KeyValue[K, V], value: Expr[In, V]) = Fact(factDef, value)
  }

  sealed trait FactDefinition[KeyValue] { self =>
    type Key <: Singleton with String
    type Value

    def name: Key

    def paramType: PrimitiveType[Value]

    // Added
    def get: Expr[(Key, Value), Value] = Expr.input(self.asInstanceOf[FactDefinition.KeyValue[Key, Value]])

    def set[In](value: Expr[In, Value]): Expr[In, Facts[(Key, Value)]] =
      Expr.fact(self.asInstanceOf[FactDefinition.KeyValue[Key, Value]], value)

    def := [In](value: Expr[In, Value]) = set(value)
    
    override final def toString(): String = s"FactDefinition($name, $paramType)"
  }

  object FactDefinition {

    type KeyValue[K <: Singleton with String, V] = FactDefinition[(K, V)] { type Key = K; type Value = V }

    def apply[N <: Singleton with String, T](name0: N)(implicit paramType0: PrimitiveType[T]): KeyValue[N, T] =
      new FactDefinition[(N, T)] {
        type Key   = N
        type Value = T
        def name: N                     = name0
        def paramType: PrimitiveType[T] = paramType0
      }

    def boolean[N <: Singleton with String](name0: N): KeyValue[N, Boolean] = FactDefinition[N, Boolean](name0)

    def byte[N <: Singleton with String](name0: N): KeyValue[N, Byte] = FactDefinition[N, Byte](name0)

    def char[N <: Singleton with String](name0: N): KeyValue[N, Char] = FactDefinition[N, Char](name0)

    def int[N <: Singleton with String](name0: N): KeyValue[N, Int] = FactDefinition[N, Int](name0)

    def long[N <: Singleton with String](name0: N): KeyValue[N, Long] = FactDefinition[N, Long](name0)

    def float[N <: Singleton with String](name0: N): KeyValue[N, Float] = FactDefinition[N, Float](name0)

    def double[N <: Singleton with String](name0: N): KeyValue[N, Double] = FactDefinition[N, Double](name0)

    def string[N <: Singleton with String](name0: N): KeyValue[N, String] = FactDefinition[N, String](name0)

    def instant[N <: Singleton with String](name0: N): KeyValue[N, java.time.Instant] =
      FactDefinition[N, java.time.Instant](name0)

  }

  final case class RuleEngine[-In, +Out](update: Facts[In] => Option[List[Out]]) { self =>
    def contramap[In2](f: Facts[In2] => Facts[In]): RuleEngine[In2, Out] = 
      RuleEngine(in => self.update(f(in))) 
      
    def orElse[In1 <: In, Out1 >: Out](that: RuleEngine[In1, Out1]): RuleEngine[In1, Out1] =
        RuleEngine(in => self.update(in) orElse that.update(in))

    def updateWith[Out1 >: Out](in: Facts[In])(defaultOut: Out1, combine: (Out1, Out1) => Out1): Out1 =
      self.update(in) match {
        case None => defaultOut
        case Some(outs) => 
          outs.reduceOption(combine).getOrElse(defaultOut)
      }
  }

  object RuleEngine {
    val empty: RuleEngine[Any, Nothing] = RuleEngine(_ => None)

    def constant[Out](out: Out): RuleEngine[Any, Out] = fromFunction(_ => out)

    def fromFunction[In, Out](f: Facts[In] => Out): RuleEngine[In, Out] = RuleEngine(in => Some(List(f(in))))

    def fromRuleSet[In, Out](ruleSet: RuleSet[In, Out]): RuleEngine[In, Out] = 
      RuleEngine(???)
  }

  final case class Rule[-In, +Out](condition: Condition[In], action: Action[In, Out])

  final case class RuleSet[-In, +Out](rules: Vector[Rule[In, Out]]) { self =>
    
    def + [In1 <: In, Out1 >: Out](that: Rule[In1, Out1]): RuleSet[In1, Out1] = 
      RuleSet(self.rules :+ that)
    
    def ++[In1 <: In, Out1 >: Out](that: RuleSet[In1, Out1]): RuleSet[In1, Out1] =
      RuleSet(self.rules ++ that.rules)
    
    def addRule[In1 <: In, Out1 >: Out](that: Rule[In1, Out1]): RuleSet[In1, Out1] = 
      self + that
  }

  object RuleSet {
    
    def apply[In, Out](rule1: Rule[In, Out], rules: Rule[In, Out]*): RuleSet[In, Out] =
      RuleSet(rule1 +: rules.toVector)

    val empty: RuleSet[Any, Nothing] = RuleSet(Vector.empty)

  }


  /**
   * Contains a collection of facts, whose structure is described by a phantom
   * type parameter.
   */
  sealed abstract case class Facts[+Types] private (private val data: Map[FactDefinition[_], Any]) {
    def ++ [Types2](that: Facts[Types2]): Facts[Types & Types2] =
      new Facts[Types & Types2](data ++ that.data) {}

    def get[Key <: Singleton with String, Value: PrimitiveType](pd: FactDefinition[(Key, Value)])(implicit
      subset: Types <:< (Key, Value)
    ): Value =
      data(pd).asInstanceOf[Value]

    /**
     * Returns a new facts collection with the specified fact added.
     */
    def add[Key <: Singleton with String, Value: PrimitiveType](
      pd: FactDefinition[(Key, Value)],
      value: Value
    ): Facts[Types & (Key, Value)] =
      new Facts[Types & (Key, Value)](data + (pd -> value)) {}

    private def add[Key <: Singleton with String, Value: PrimitiveType](
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

    def apply[Key <: Singleton with String, Value: PrimitiveType](key: Key, value: Value): Facts[(Key, Value)] =
      empty.add(key, value)

    def apply[
      Key1 <: Singleton with String,
      Value1: PrimitiveType,
      Key2 <: Singleton with String,
      Value2: PrimitiveType
    ](
      tuple1: (Key1, Value1),
      tuple2: (Key2, Value2)
    ): Facts[(Key1, Value1) & (Key2, Value2)] =
      empty.add[Key1, Value1](tuple1._1, tuple1._2).add[Key2, Value2](tuple2._1, tuple2._2)

    def apply[
      Key1 <: Singleton with String,
      Value1: PrimitiveType,
      Key2 <: Singleton with String,
      Value2: PrimitiveType,
      Key3 <: Singleton with String,
      Value3: PrimitiveType
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
      Value1: PrimitiveType,
      Key2 <: Singleton with String,
      Value2: PrimitiveType,
      Key3 <: Singleton with String,
      Value3: PrimitiveType,
      Key4 <: Singleton with String,
      Value4: PrimitiveType
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
      Value1: PrimitiveType,
      Key2 <: Singleton with String,
      Value2: PrimitiveType,
      Key3 <: Singleton with String,
      Value3: PrimitiveType,
      Key4 <: Singleton with String,
      Value4: PrimitiveType,
      Key5 <: Singleton with String,
      Value5: PrimitiveType
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

  final case class Condition[-In] (expr: Expr[In, Boolean]) { self => 
    def &&[In1 <: In](that: Condition[In1]): Condition[In1] = 
      Condition(self.expr && that.expr)

    def ||[In1 <: In](that: Condition[In1]): Condition[In1] = 
      Condition(self.expr && that.expr)
      
    def unary_! : Condition[In] = Condition(!expr)
  }

  object Condition {
    val always: Condition[Any] = constant(true)
    val never: Condition[Any] = constant(false)

    def constant[In](value: Boolean): Condition[In] = Condition(Expr(value))
    

  }

  sealed trait Action[-In, +Out] { self =>
    def ++[In1 <: In, Out1 >: Out](that: Action[In1, Out1]): Action[In1, Out1] = 
        Action.Concat(self, that)
        
    def >>>[Out2](that: Action[Out, Out2]): Action[In, Out2] = 
        Action.Pipe(self, that)
  }
  object Action {
    final case class Concat[In, Out](left: Action[In, Out], right: Action[In, Out]) extends Action[In, Out]
    final case class Pipe[In, Out1, Out2](left: Action[In, Out1], right: Action[Out1, Out2]) extends Action[In, Out2]
    final case class FromExpr[In, Out](expr: Expr[In, Out]) extends Action[In, Out]

    def fromExpr[In, Out](expr: Expr[In, Out]): Action[In, Out] = FromExpr(expr)

    def constant[Out](out: Out)(implicit tag: PrimitiveType[Out]): Action[Any, Out] = fromExpr(Expr(out))
  }

  object loyalty {
    import net.degoes.afd.examples.loyalty._
    import net.degoes.afd.examples.loyalty.LoyaltyTier._

    object FlightBooking {
        val id       = FactDefinition.string("id")
        val customer = FactDefinition.string("custoner") // FIXME: Support nested data
        val flight   = FactDefinition.string("flight")   // FIXME: Suppor nested data
        val price    = FactDefinition.double("price")
        val status   = FactDefinition.string("status")
    }

    object FlightBookingStatus {
        val Confirmed = Expr("Confirmed")
        val Cancelled = Expr("Cancelled")
        val Pending   = Expr("Pending")
    }

    (FlightBooking.price := FlightBooking.price.get + 1000.0) ++ (FlightBooking.status := FlightBookingStatus.Pending)

    val statusCondition = 
      Condition(FlightBooking.status.get === FlightBookingStatus.Confirmed)
    val priceCondition  = Condition(FlightBooking.price.get > 1000.0)

    val exampleCondition = statusCondition && priceCondition

  }

}
