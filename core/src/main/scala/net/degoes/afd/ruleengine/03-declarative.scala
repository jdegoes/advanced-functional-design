/**
 * Executable encodings are free of boilerplate and fast and flexible. However,
 * they tend to be less powerful and principled than declarative encodings,
 * which can be used with infinitely many different interpreters, each doing
 * something different (and useful!) with the model.
 *
 * In this section, you will refactor the rule engine you created to use the
 * declarative encoding. In the process, you will discover _expressions_, which
 * are recipes to describe the production of values from other values.
 *
 * To push this model to its logical conclusion, it will be necessary to
 * eliminate all Scala functions, which raises questions about how well-typed
 * you want to make the model.
 */
package net.degoes.afd.ruleengine

import zio._

import scala.annotation.implicitNotFound
import scala.language.implicitConversions

/**
 * Develop a fully declarative encoding of a rule engine. You are NOT allowed to
 * use any Scala functions in your model. Rather, your model must be purely
 * ADT-based. Attempt to make your model as type-safe as possible, but sacrifice
 * type-safety if necessary in order to avoid the embedding of Scala functions
 * in your model.
 *
 * You must develop an executor for the model which, given input and the rule
 * set, produces actions as output.
 */
object declarative {
  // Note: ensure the primitive types are in sync with ordering types
  @implicitNotFound("The type ${A} is not supported as a fact type and cannot be used for this method.")
  sealed trait PrimitiveType[A] { self =>
    def ordering: scala.math.Ordering[A] = PrimitiveType.orderingOf(self)
  }
  object PrimitiveType {
    implicit case object BooleanType extends PrimitiveType[Boolean]
    implicit case object ByteType    extends PrimitiveType[Byte]
    implicit case object CharType    extends PrimitiveType[Char]
    implicit case object IntType     extends PrimitiveType[Int]
    implicit case object LongType    extends PrimitiveType[Long]
    implicit case object FloatType   extends PrimitiveType[Float]
    implicit case object DoubleType  extends PrimitiveType[Double]
    implicit case object StringType  extends PrimitiveType[String]
    implicit case object InstantType extends PrimitiveType[java.time.Instant]

    def orderingOf[A](tag: PrimitiveType[A]): scala.math.Ordering[A] =
      tag match {
        case BooleanType => scala.math.Ordering[Boolean]
        case ByteType    => scala.math.Ordering[Byte]
        case CharType    => scala.math.Ordering[Char]
        case IntType     => scala.math.Ordering[Int]
        case LongType    => scala.math.Ordering[Long]
        case FloatType   => scala.math.Ordering[Float]
        case DoubleType  => scala.math.Ordering[Double]
        case StringType  => scala.math.Ordering[String]
        case InstantType => ??? // scala.math.Ordering[Long].on(java.time.Instant)(_.toEpochMilli)
      }
  }

  // Facts
  // FactDefinition
  // PrimitiveType = PrimitiveType

  // Its basicly a function
  // - its code not data so we do not need to serialize/deserialize
  // - next step goal likely to:update: Record[In] => Option[List[Out]] ...
  final case class RuleEngine[-In, +Out](update: Facts[In] => Option[List[Out]]) { self =>

    def contramap[In2](f: Facts[In2] => Facts[In]): RuleEngine[In2, Out] =
      RuleEngine(in => self.update(f(in)))

    def orElse[In1 <: In, Out1 >: Out](that: RuleEngine[In1, Out1]): RuleEngine[In1, Out1] =
      RuleEngine[In1, Out1](in => self.update(in).orElse(that.update(in)))

    def updateWith[Out1 >: Out](in: Facts[In])(defaultOut: Out1, combine: (Out1, Out1) => Out1): Out1 =
      self.update(in) match {
        case None       => defaultOut
        case Some(outs) => outs.reduceOption(combine).getOrElse(defaultOut)
      }

  }
  object RuleEngine {
    val empty: RuleEngine[Any, Nothing] = RuleEngine(_ => None)

    // Note:  not sense any longer cant pattern match on facts
    // def collect[In, Out](pf: PartialFunction[Facts[In], Out]): RuleEngine[In, Out] =
    //   RuleEngine(in => pf.lift(in).map(List(_)))

    def constant[Out](out: List[Out]): RuleEngine[Any, Out] = RuleEngine(_ => Some(out))

    def fromFunction[In, Out](f: Facts[In] => Out): RuleEngine[In, Out] = RuleEngine(in => Some(List(f(in))))

    def fromRuleSet[In, Out](ruleSet: RuleSet[In, Out]): RuleEngine[In, Out] = RuleEngine(???)
  }

  final case class RuleSet[-In, +Out](rules: Vector[Rule[In, Out]]) { self =>

    def ++[In1 <: In, Out1 >: Out](that: RuleSet[In1, Out1]): RuleSet[In1, Out1] = RuleSet(self.rules ++ that.rules)

    def +[In1 <: In, Out1 >: Out](that: Rule[In1, Out1]): RuleSet[In1, Out1] = RuleSet(self.rules :+ that)

    def addRule[In1 <: In, Out1 >: Out](that: Rule[In1, Out1]) = self + that

  }
  object RuleSet {
    def apply[In, Out](rule1: Rule[In, Out], rules: Rule[In, Out]*): RuleSet[In, Out] = RuleSet(rule1 +: rules.toVector)

    def empty[In, Out]: RuleSet[In, Out] = RuleSet(Vector.empty)
  }

  // Note: argumentation its not needed to be serializable becuase condition and action would be serialized
  final case class Rule[-In, +Out](condition: Condition[In], action: Action[In, Out])

  sealed trait Numeric[A]
  object Numeric {
    implicit case object ByteIsNumeric   extends Numeric[Byte]
    implicit case object CharIsNumeric   extends Numeric[Char]
    implicit case object IntIsNumeric    extends Numeric[Int]
    implicit case object LongIsNumeric   extends Numeric[Long]
    implicit case object FloatIsNumeric  extends Numeric[Float]
    implicit case object DoubleIsNumeric extends Numeric[Double]
  }

  /**
   * Contains a collection of facts, whose structure is described by a phantom
   * type parameter.
   */
  sealed abstract case class Facts[+Types] private (private val data: Map[FactDefinition[_], Any]) {

    def ++[Types2](that: Facts[Types2]): Facts[Types & Types2] =
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

  sealed trait FactDefinition[KeyValue] { self =>
    type Key <: Singleton with String
    type Value

    def name: Key

    def tag: PrimitiveType[Value]

    // Note: there is a way (self: FactDefinition[Key, Value] =>) to not use cast but cast is easiest...
    // A constructor: Reads a value in a expr out of a FactDefinition - it could also have been on Expr
    def get: Expr[(Key, Value), Value] = Expr.input(self.asInstanceOf[FactDefinition.KeyValue[Key, Value]])

    def set[In](value: Expr[In, Value]): Expr[In, Facts[(Key, Value)]] =
      Expr.fact(self.asInstanceOf[FactDefinition.KeyValue[Key, Value]], value)

    def :=[In](value: Expr[In, Value]): Expr[In, Facts[(Key, Value)]] = set(value)

    override final def toString(): String = s"FactDefinition($name, $tag)"
  }
  object FactDefinition {

    // Note Type  refinment
    type KeyValue[K <: Singleton with String, V] = FactDefinition[(K, V)] { type Key = K; type Value = V }

    def apply[N <: Singleton with String, T](name0: N)(implicit paramType0: PrimitiveType[T]): KeyValue[N, T] =
      new FactDefinition[(N, T)] {
        type Key   = N
        type Value = T
        def name: N               = name0
        def tag: PrimitiveType[T] = paramType0
      }

    def boolean[N <: Singleton with String](name0: N): KeyValue[N, Boolean] = FactDefinition[N, Boolean](name0)

    def double[N <: Singleton with String](name0: N): KeyValue[N, Double] = FactDefinition[N, Double](name0)

    def int[N <: Singleton with String](name0: N): KeyValue[N, Int] = FactDefinition[N, Int](name0)

    def instant[N <: Singleton with String](name0: N): KeyValue[N, java.time.Instant] =
      FactDefinition[N, java.time.Instant](name0)

    def string[N <: Singleton with String](name0: N): KeyValue[N, String] = FactDefinition[N, String](name0)
  }

  // Store int, etc and get type out of it
  // its like json api but more type safe
  //
  // This example is using the scala 2.13.* singlten type
  //
  // Example of the singleton type in scala 2.13.*
  // val x = "foo"
  // x.type
  final class DynamicRecord[Fields] private (private val map: Map[(String, PrimitiveType[_]), Any]) {
    def ++(that: DynamicRecord[Fields]): DynamicRecord[Fields] =
      new DynamicRecord(map ++ that.map)

    // Note: type out put (name.type, A) => ("fieldName", Int) ...
    def add[A](name: String, value: A)(implicit tag: PrimitiveType[A]): DynamicRecord[Fields with (name.type, A)] =
      new DynamicRecord(map.updated(name -> tag, value))

    def get[A](name: String)(implicit tag: PrimitiveType[A], ev: Fields <:< (name.type, A)): Option[A] =
      map.get(name -> tag).map(_.asInstanceOf[A])

  }
  object DynamicRecord {
    val empty: DynamicRecord[Any] = new DynamicRecord(Map())

    // Note: this record do not hold age and will fail with compile error:
    // Cannot prove that Any with (String("isMale"), Boolean) with (String("street"), String) <:< (String("age"), Int).
    // DynamicRecord.empty
    //   .add("isMale", true)
    //   .add("street", "123 Main st")
    //   .get[Int]("age")

    DynamicRecord.empty
      .add("age", 42)
      .add("isMale", true)
      .add("street", "123 Main st")
      .get[Int]("age")
  }

  // Note: some type level hands on
  // using path dependent types works on all scala (tm)
  object GenericTypeLevel101 {

    import PrimitiveType._

    sealed abstract case class FieldSpec[A](name: String, fieldType: PrimitiveType[A]) {
      type Type
    }
    object FieldSpec {
      def apply[A](name: String)(implicit tag: PrimitiveType[A]): FieldSpec[A] =
        new FieldSpec(name, tag) {}

      type WithFieldType[A, B] = FieldSpec[A] { type Type = B }
    }

    val age    = FieldSpec[Int]("age")
    val name   = FieldSpec[String]("name")
    val street = FieldSpec[String]("street")
    val isMale = FieldSpec[Boolean]("isMale")

    // Note:  value of fields exist, its a phantom type
    final class Record[+Fields] private (val map: Map[FieldSpec[_], Any]) { self =>

      def ++[Fields2](that: Record[Fields2]): Record[Fields with Fields2] =
        new Record(self.map ++ that.map)

      def add[A](fs: FieldSpec[A], value: A): Record[Fields with (fs.Type, A)] =
        new Record(self.map.updated(fs, value))

      def get[A](fs: FieldSpec[A])(implicit ev: Fields <:< (fs.Type, A)): A =
        self.map(fs).asInstanceOf[A]

      def updateWith[A](fs: FieldSpec[A], f: A => A)(implicit ev: Fields <:< (fs.Type, A)): Record[Fields] =
        add(fs, f(self.get(fs)))
    }
    object Record {
      val empty: Record[Any] = new Record(Map())
    }

    // Note: types are here for docs, would be infered...

    type TestRecord1 = Record[(age.Type, Int) with (name.Type, String) with (isMale.Type, Boolean)]

    val testRecord: TestRecord1 = Record.empty
      .add(age, 42)
      .add(name, "John")
      .add(isMale, true)

    // Do not compile as expected (tm)
    // testRecord.get(street)

    type TestRecord2 =
      Record[(age.Type, Int) with (name.Type, String) with (street.Type, String) with (isMale.Type, Boolean)]

    val testRecord2: TestRecord2 = testRecord.add(street, "Main Street")

    testRecord2.get(street)
  }

  object PhantomType101 {
    // Note: recap phantom type used in ZIO like
    // ZIO[Console & Logging & Database & Int & String, Throwable, Unit]
    // no values of this type... used as guard on compile time
    // will though require
    // ZEnvironment[Console & Logging & Database & Int & String]

    final case class Phantom[-In, +Out]()
    def foo[In, Out](t: Phantom[In, Out]): Phantom[In, Out] = ???
    // ^^ no requirement of producing In / Out
    // recall we have phantom to track types on compile types
    //
    // so we can pass around stuff on compile time without having the "value in hand"..
  }

  // Note: a recipe that when executed produces a value
  // 2 * 3
  // 2 * (3 + 123) / "foo".length
  // 2 * (3 +123) / scala.io.StdIn.readLine().length
  //
  // we can use that to to computation and manipulation inside action and condition
  // expression can be viewed as a very simple programming language (without for loops and stuff like that)
  // a `glorified` calculator
  //
  // () => Out
  // 1 + 2
  // 1 + Math.pow(2, 3)
  // scala.io.StdIn.readLine().length() + scala.util.Random.nextInt()
  // In => Out

  //
  // Note: john suggests to go away from sealed trait Expr[-In, +Out]
  // to any values of In in memory or when evaluated Out so it could be phantom types
  // ... he suggests using hlist (complicated) or more simply use phantom types to solve the structured data problem

  //
  // Not: Facts[("age", Int)] with Facts[("name", String)] with Facts[("isMale", Boolean)]
  // Goal: Facts[("age", Int) with ("name", String) with ("isMale", Boolean)]
  // Fact[In] => Out
  //
  // // Fact[In] => Facts[Out]
  sealed trait Expr[-In, +Out] { self =>

    final def ++[In1 <: In, Fields1, Fields2](that: Expr[In1, Facts[Fields2]])(implicit
      ev: Out <:< Facts[Fields1]
    ): Expr[In1, Facts[Fields1 & Fields2]] =
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

    def &&[In1 <: In](that: Expr[In1, Boolean])(implicit ev: Out <:< Boolean): Expr[In1, Boolean] =
      Expr.And(self.widen[Boolean], that)

    def ||[In1 <: In](that: Expr[In1, Boolean])(implicit ev: Out <:< Boolean): Expr[In1, Boolean] =
      Expr.Or(self.widen[Boolean], that)

    final def ===[In1 <: In, Out1 >: Out](that: Expr[In1, Out1]): Expr[In1, Boolean] = Expr.EqualTo(self.widen, that)

    final def <[In1 <: In, Out1 >: Out](that: Expr[In1, Out1]): Expr[In1, Boolean] = Expr.LessThan(self, that)

    final def <=[In1 <: In, Out1 >: Out](that: Expr[In1, Out1]): Expr[In1, Boolean] =
      (self < that) || (self === that)

    final def >[In1 <: In, Out1 >: Out](that: Expr[In1, Out1]): Expr[In1, Boolean] = !(self <= that)

    final def >=[In1 <: In, Out1 >: Out](that: Expr[In1, Out1]): Expr[In1, Boolean] = !(self < that)

    final def >>>[Out2](that: Expr[Out, Out2]): Expr[In, Out2] = Expr.Pipe(self, that)

    def ifTrue[In1 <: In, Out2](ifTrue: Expr[In1, Out2])(implicit ev: Out <:< Boolean): Expr.IfTrue[In1, Out2] =
      Expr.IfTrue(self.widen[Boolean], ifTrue)

    def unary_!(implicit ev: Out <:< Boolean): Expr[In, Boolean] = Expr.Not(self.widen[Boolean])

    // Note: casting because its safe (tm)
    // John uses this trick instead of going the more verbose Expr.`final case class Widen`
    def widen[Out2](implicit ev: Out <:< Out2): Expr[In, Out2] = self.asInstanceOf[Expr[In, Out2]]
  }

  object Expr {

    // Note: the current evidence approach could have been implemented with roughly
    // implicit class ExprBoolSyntax[In](self: Expr[In, Boolean])
    // con that one do not give good error messages when `and and` on types not boolean

    //
    // IfTrue is a helper so we can write
    Expr(true).ifTrue(42).otherwise(43)
    final case class IfTrue[In, Out](condition: Expr[In, Boolean], ifTrue: Expr[In, Out]) extends Expr[In, Out] {
      def otherwise(ifFalse: Expr[In, Out]): Expr[In, Out] = Expr.IfThenElse(condition)(ifTrue, ifFalse)
    }

    // Note: think of the following case classes constructors
    //
    // final case class Fact[K <: Singleton with String, V]()                      extends Expr[Any, Facts[(K, V)]]
    // `value: V` would not be flexible enough... so has to be Expr...
    // In used so we can have Input ... Any not enough
    final case class Fact[In, K <: Singleton with String, V](factDef: FactDefinition.KeyValue[K, V], value: Expr[In, V])
        extends Expr[In, Facts[(K, V)]]
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
    // Niecetohave: division
    final case class Input[K <: Singleton with String, V](factDef: FactDefinition.KeyValue[K, V])
        extends Expr[(K, V), V] // (K, V) - because the input to our rule enginge will be facts
    final case class Pipe[In, Out1, Out2](left: Expr[In, Out1], right: Expr[Out1, Out2]) extends Expr[In, Out2]
    final case class BinaryNumericOp[In, Out](
      lhs: Expr[In, Out],
      rhs: Expr[In, Out],
      op: NumericBinOpType,
      tag: Numeric[Out]
    ) extends Expr[In, Out]

    final case class IfThenElse[In, Out](condition: Expr[In, Boolean])(ifTrue: Expr[In, Out], ifFalse: Expr[In, Out])
        extends Expr[In, Out]

    sealed trait NumericBinOpType
    object NumericBinOpType {
      case object Add      extends NumericBinOpType
      case object Subtract extends NumericBinOpType
      case object Multiply extends NumericBinOpType
      case object Divide   extends NumericBinOpType
      case object Modulo   extends NumericBinOpType
    }

    implicit def apply[Out](out: Out)(implicit tag: PrimitiveType[Out]): Expr[Any, Out] = Constant(out, tag)

    def fact[In, K <: Singleton with String, V](
      factDef: FactDefinition.KeyValue[K, V],
      value: Expr[In, V]
    ): Expr[In, Facts[(K, V)]] =
      Fact(factDef, value)

    def ifThenElse[In, Out](
      condition: Expr[In, Boolean]
    )(ifTrue: Expr[In, Out], ifFalse: Expr[In, Out]): Expr[In, Out] =
      IfThenElse(condition)(ifTrue, ifFalse)

    def input[K <: Singleton with String, V](factDef: FactDefinition.KeyValue[K, V]): Expr[(K, V), V] = Input(factDef)

    // Note: we use the fact record  approach instead
    // def field[In, Out](name: String)(implicit tag: PrimitiveType[Out]): Expr[In, Out] = ???
  }

  // Note: proof constrain types
  def constrainedPolymorphicFunction[A](a: A)(implicit pt: PrimitiveType[A]): A = a

  constrainedPolymorphicFunction(1)
  constrainedPolymorphicFunction("1")
  constrainedPolymorphicFunction(true)

  // In => Boolean
  // Expr[Boolean]
  final case class Condition[-In](expr: Expr[In, Boolean]) { self =>

    def &&[In1 <: In](that: Condition[In1]): Condition[In1] = Condition(self.expr && that.expr)

    def ||[In1 <: In](that: Condition[In1]): Condition[In1] = Condition(
      self.expr || that.expr
    ) // TODO mention that there is maybe an error &&

    def unary_! : Condition[In] = Condition(!expr)

  }
  object Condition { self =>

    // Note: we can not use scala math ordering because its executable encoding
    // scala.math.Ordering
    // because we want declarative encoding

    val always: Condition[Any] = constant(true)

    val never: Condition[Any] = constant(false)

    def constant[In](value: Boolean): Condition[In] = Condition(Expr(value))

    // Note: not used for now likely let expr handle it

    // def isEqualTo[In](rhs: In)(implicit tag: PrimitiveType[In]): Condition[In] =
    //   Condition(Expr.input[In] === Expr(rhs))

    // def isLessThan[In](rhs: In)(implicit tag: PrimitiveType[In]): Condition[In] =
    //   Condition(Expr.input[In] < Expr(rhs))

    // def isGreaterThan[In](rhs: In)(implicit tag: PrimitiveType[In]): Condition[In] =
    //   Condition(Expr.input[In] > Expr(rhs))

    // Note: fromFunction can not be solved due to serialization
  }

  // Recap actions: LoyalyAction is pure data - think of Out as pure data do not need to be fancy like a function
  // think commands
  // - Add points
  // - Promote tier
  // - Send email
  // Outs are kind of instructions that tell us what to do.

  // In => NonEmptyList[Out]
  sealed trait Action[-In, +Out] { self =>

    def ++[In1 <: In, Out1 >: Out](that: Action[In1, Out1]): Action[In1, Out1] =
      Action.Concat(self, that)

    def >>>[Out2](that: Action[Out, Out2]): Action[In, Out2] = Action.Pipe(self, that)

    // Niecetohave: zip, at least it do not have scala functions in the executaable encoding imple
  }
  object Action { self =>
    final case class Concat[In, Out1, Out2](left: Action[In, Out1], right: Action[In, Out2]) extends Action[In, Out2]
    final case class Pipe[In, Out1, Out2](left: Action[In, Out1], right: Action[Out1, Out2]) extends Action[In, Out2]
    final case class FromExpr[In, Out](expr: Expr[In, Out])                                  extends Action[In, Out]

    def fromExpr[In, Out](expr: Expr[In, Out]): Action[In, Out] = FromExpr(expr)

    def constant[Out](out: Out)(implicit tag: PrimitiveType[Out]): Action[Any, Out] = fromExpr(Expr(out))
  }

  object loyalty {
    // Note:
    // FlightBookStatus is not a type in Our world ... it would need some adjustments of `object Ordering {`
    //    case class Ordering2[A, B](orderingA: Ordering[A], orderingB: Ordering[B]) extends Ordering[(A, B)]
    // John does not recommend going down this path now ...

    import net.degoes.afd.examples.loyalty._
    import net.degoes.afd.examples.loyalty.LoyaltyTier._

    // Rule engine model of FlightBooking and FlightBookingStatus that can be understood outside scala
    object FlightBooking {
      val id       = FactDefinition.string("id")
      val customer = FactDefinition.string("customer") // FIXME: Support nested data
      val flight   = FactDefinition.string("flight")   // FIXME: Support nested data
      val price    = FactDefinition.double("price")
      val status   = FactDefinition.string("status")
    }
    object FlightBookingStatus {
      val Confirmed = Expr("Confirmed")
      val Cancelled = Expr("Cancelled")
      val Pending   = Expr("Pending")
    }

    // FlightBooking.price.set(1000.0)
    // // same as
    // FlightBooking.price := 1000.0

    val foo: Expr[Any, Facts[("price", Double)]] = (FlightBooking.price := 1000.0)

    (FlightBooking.price    := 1000.0) ++
      (FlightBooking.status := FlightBookingStatus.Confirmed)

    val statusCondition = Condition(FlightBooking.status.get === FlightBookingStatus.Confirmed)

    val priceCondition = Condition(FlightBooking.price.get > Expr(1000.0))

    val both = statusCondition && priceCondition

    // goal make an action that produces
    // { "action_type": "add_points", "points": 100, "customer": "customer_id" }
    // { "action_type": "upgrade_tier", "customer": "customer_id" }
    // { "action_type": "downgrade_tier", "customer": "customer_id" }

  }
}
