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

import java.time.Instant
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

  sealed trait Numeric[A] { self =>
    type NumericType = A

    def primitiveType: PrimitiveType[A] =
      (self match {
        case _: Numeric.ByteNumeric.type   => PrimitiveType.ByteType
        case _: Numeric.CharNumeric.type   => PrimitiveType.CharType
        case _: Numeric.ShortNumeric.type  => PrimitiveType.ShortType
        case _: Numeric.IntNumeric.type    => PrimitiveType.IntType
        case _: Numeric.LongNumeric.type   => PrimitiveType.LongType
        case _: Numeric.FloatNumeric.type  => PrimitiveType.FloatType
        case _: Numeric.DoubleNumeric.type => PrimitiveType.DoubleType
      }).asInstanceOf[PrimitiveType[A]]

    def add(a: A, b: A): A
    def sub(a: A, b: A): A
    def mul(a: A, b: A): A
    def div(a: A, b: A): A
    def mod(a: A, b: A): A

    def apply(binOp: Expr.NumericBinOpType)(left: A, right: A): A =
      binOp match {
        case Expr.NumericBinOpType.Add      => add(left, right)
        case Expr.NumericBinOpType.Subtract => sub(left, right)
        case Expr.NumericBinOpType.Multiply => mul(left, right)
        case Expr.NumericBinOpType.Divide   => div(left, right)
        case Expr.NumericBinOpType.Modulo   => mod(left, right)
      }
  }

  object Numeric {
    implicit case object IntNumeric extends Numeric[Int] {
      def add(a: Int, b: Int): Int = a + b
      def sub(a: Int, b: Int): Int = a - b
      def mul(a: Int, b: Int): Int = a * b
      def div(a: Int, b: Int): Int = a / b
      def mod(a: Int, b: Int): Int = a % b
    }
    implicit case object LongNumeric extends Numeric[Long] {
      def add(a: Long, b: Long): Long = a + b
      def sub(a: Long, b: Long): Long = a - b
      def mul(a: Long, b: Long): Long = a * b
      def div(a: Long, b: Long): Long = a / b
      def mod(a: Long, b: Long): Long = a % b
    }
    implicit case object ShortNumeric extends Numeric[Short] {
      def add(a: Short, b: Short): Short = (a + b).toShort
      def sub(a: Short, b: Short): Short = (a - b).toShort
      def mul(a: Short, b: Short): Short = (a * b).toShort
      def div(a: Short, b: Short): Short = (a / b).toShort
      def mod(a: Short, b: Short): Short = (a % b).toShort
    }
    implicit case object ByteNumeric extends Numeric[Byte] {
      def add(a: Byte, b: Byte): Byte = (a + b).toByte
      def sub(a: Byte, b: Byte): Byte = (a - b).toByte
      def mul(a: Byte, b: Byte): Byte = (a * b).toByte
      def div(a: Byte, b: Byte): Byte = (a / b).toByte
      def mod(a: Byte, b: Byte): Byte = (a % b).toByte
    }
    implicit case object CharNumeric extends Numeric[Char] {
      def add(a: Char, b: Char): Char = (a + b).toChar
      def sub(a: Char, b: Char): Char = (a - b).toChar
      def mul(a: Char, b: Char): Char = (a * b).toChar
      def div(a: Char, b: Char): Char = (a / b).toChar
      def mod(a: Char, b: Char): Char = (a % b).toChar
    }
    implicit case object FloatNumeric extends Numeric[Float] {
      def add(a: Float, b: Float): Float = a + b
      def sub(a: Float, b: Float): Float = a - b
      def mul(a: Float, b: Float): Float = a * b
      def div(a: Float, b: Float): Float = a / b
      def mod(a: Float, b: Float): Float = a % b
    }
    implicit case object DoubleNumeric extends Numeric[Double] {
      def add(a: Double, b: Double): Double = a + b
      def sub(a: Double, b: Double): Double = a - b
      def mul(a: Double, b: Double): Double = a * b
      def div(a: Double, b: Double): Double = a / b
      def mod(a: Double, b: Double): Double = a % b
    }
  }

  @implicitNotFound("There is no PrimitiveType for ${A}")
  sealed trait PrimitiveType[A] {
    self =>
    def ordering: Ordering[A] = PrimitiveType.orderingOf(self)
  }

  object PrimitiveType {
    implicit case object BooleanType extends PrimitiveType[Boolean]
    implicit case object ByteType    extends PrimitiveType[Byte]
    implicit case object CharType    extends PrimitiveType[Char]
    implicit case object ShortType   extends PrimitiveType[Short]
    implicit case object IntType     extends PrimitiveType[Int]
    implicit case object LongType    extends PrimitiveType[Long]
    implicit case object FloatType   extends PrimitiveType[Float]
    implicit case object DoubleType  extends PrimitiveType[Double]
    implicit case object StringType  extends PrimitiveType[String]
    implicit case object InstantType extends PrimitiveType[Instant]

    def orderingOf[A](tag: PrimitiveType[A]): Ordering[A] =
      tag match {
        case BooleanType => Ordering[Boolean]
        case ByteType    => Ordering[Byte]
        case CharType    => Ordering[Char]
        case ShortType   => Ordering[Short]
        case IntType     => Ordering[Int]
        case LongType    => Ordering[Long]
        case FloatType   => Ordering[Float]
        case DoubleType  => Ordering[Double]
        case StringType  => Ordering[String]
        case InstantType => Ordering[Long].on[Instant](_.toEpochMilli)
      }
  }

  sealed trait EngineType[A] {
    def equals(a: A, b: A): Boolean
    def lessThan(a: A, b: A): Boolean
  }

  object EngineType {
    final case class Primitive[A](primitiveType: PrimitiveType[A]) extends EngineType[A] {
      override def equals(a: A, b: A): Boolean =
        primitiveType.ordering.equiv(a, b)
      override def lessThan(a: A, b: A): Boolean =
        primitiveType.ordering.lt(a, b)
    }

    final case class Composite[Fields](factsType: FactsType[Fields]) extends EngineType[Facts[Fields]] {
      override def equals(a: Facts[Fields], b: Facts[Fields]): Boolean = a == b

      override def lessThan(a: Facts[Fields], b: Facts[Fields]): Boolean = false
    }

    def fromPrimitive[A](implicit primType: PrimitiveType[A]): EngineType[A] =
      EngineType.Primitive(primType)

    def fromFacts[Types](facts: Facts[Types]): EngineType[Facts[Types]] =
      EngineType.Composite(FactsType.fromFacts(facts))
  }

  sealed trait FactDefinition[KeyValue] {
    type Key <: Singleton with String
    type Value

    def name: Key

    def tag: EngineType[Value]

    def self: FactDefinition.KeyValue[Key, Value] =
      self.asInstanceOf[FactDefinition.KeyValue[Key, Value]]

    val singletonType: EngineType[Facts[(Key, Value)]] =
      EngineType.Composite(FactsType.empty.add[(Key, Value)](self.asInstanceOf[FactDefinition[(Key, Value)]]))

    def get: Expr[Facts[(Key, Value)], Value] = {
      val fields: Expr[Facts[(Key, Value)], Facts[(Key, Value)]] =
        Expr.input(singletonType)
      Expr.Get(fields, self)
    }

    def set[In](value: Expr[In, Value]): Expr[In, Facts[(Key, Value)]] =
      Expr.fact(self.asInstanceOf[FactDefinition.KeyValue[Key, Value]], value)

    def :=[In](value: Expr[In, Value]): Expr[In, Facts[(Key, Value)]] = set(value)

    override final def toString(): String = s"FactDefinition($name, $tag)"
  }

  object FactDefinition {
    type KeyValue[K <: Singleton with String, V] = FactDefinition[(K, V)] { type Key = K; type Value = V }

    def apply[N <: Singleton with String, T](name0: N, tag0: EngineType[T]): KeyValue[N, T] =
      new FactDefinition[(N, T)] {
        type Key   = N
        type Value = T

        def name: N = name0

        def tag: EngineType[T] = tag0
      }

    def facts[N <: Singleton with String, Fields](name: N, factsType: FactsType[Fields]): KeyValue[N, Facts[Fields]] =
      FactDefinition[N, Facts[Fields]](name, EngineType.Composite(factsType))

    def prim[N <: Singleton with String, T](name0: N)(implicit tag0: PrimitiveType[T]): KeyValue[N, T] =
      new FactDefinition[(N, T)] {
        type Key   = N
        type Value = T

        def name: N = name0

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

  class FactsType[KeyValues] private (private val definitions: Chunk[FactDefinition[_]]) {
    def ++[KeyValues2](that: FactsType[KeyValues2]): FactsType[KeyValues & KeyValues2] =
      new FactsType(definitions ++ that.definitions)

    def add[KeyValue](definition: FactDefinition[KeyValue]): FactsType[KeyValue & KeyValues] =
      new FactsType(definitions :+ definition)
  }
  object FactsType {
    val empty: FactsType[Any] = new FactsType(Chunk.empty)

    def fromFacts[KeyValues](facts: Facts[KeyValues]): FactsType[KeyValues] =
      new FactsType(facts.definitions)
  }

  sealed abstract case class Facts[+Types] private (private val data: Map[FactDefinition[_], Any]) {
    self =>
    def ++[Types2](that: Facts[Types2]): Facts[Types & Types2] =
      new Facts[Types & Types2](data ++ that.data) {}

    def definitions: Chunk[FactDefinition[_]] = Chunk.fromIterable(data.keys)

    def get[Key <: Singleton with String, Value: PrimitiveType](pd: FactDefinition[(Key, Value)])(implicit
      subset: Types <:< (Key, Value)
    ): Value =
      data(pd).asInstanceOf[Value]

    override def equals(obj: Any): Boolean =
      obj match {
        case that: Facts[_] => data == that.data
        case _              => false
      }

    /**
     * Returns a new facts collection with the specified primitive fact added.
     */
    def add[Key <: Singleton with String, Value](
      pd: FactDefinition.KeyValue[Key, Value],
      value: Value
    ): Facts[Types & (Key, Value)] =
      new Facts[Types & (Key, Value)](data + (pd -> value)) {}

    /**
     * Returns a new facts collection with the specified composite fact added.
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
  }
  final case class RuleEngine[-In, +Out](update: In => Option[List[Out]]) {
    self =>
    def contramap[In2](f: In2 => In): RuleEngine[In2, Out] = RuleEngine(in2 => self.update(f(in2)))

    def orElse[In1 <: In, Out1 >: Out](that: RuleEngine[In1, Out1]): RuleEngine[In1, Out1] =
      RuleEngine(in => self.update(in) orElse that.update(in))

    def updateWith[Out1 >: Out](in: In)(defaultOut: Out1, combine: (Out1, Out1) => Out1): Out1 =
      self.update(in) match {
        case None => defaultOut

        case Some(outs) =>
          outs.reduceOption(combine).getOrElse(defaultOut)
      }
  }

  object RuleEngine {
    val empty: RuleEngine[Any, Nothing] = RuleEngine(_ => None)

    def constant[Out](out: Out): RuleEngine[Any, Out] = fromFunction(_ => out)

    def fromFunction[In, Out](f: In => Out): RuleEngine[In, Out] =
      RuleEngine(in => Some(List(f(in))))

    def fromRuleSet[In, Out](ruleSet: RuleSet[In, Out]): RuleEngine[In, Out] = ???
//      RuleEngine(ruleSet.update)

    private def execute[In, Out](ruleSet: RuleSet[In, Out], in: In): Option[List[Out]] =
      ruleSet.rules.find(_.condition.eval(in)).map { rule =>
        rule.action.eval(in)
      }
  }

  final case class RuleSet[-In, +Out](rules: Vector[Rule[In, Out]]) {
    self =>
    def +[In1 <: In, Out1 >: Out](that: Rule[In1, Out1]): RuleSet[In1, Out1] =
      RuleSet(self.rules :+ that)

    def ++[In1 <: In, Out1 >: Out](that: RuleSet[In1, Out1]): RuleSet[In1, Out1] =
      RuleSet(self.rules ++ that.rules)

    def addRule[In1 <: In, Out1 >: Out](that: Rule[In1, Out1]): RuleSet[In1, Out1] = self + that

  }

  object RuleSet {
    def apply[In, Out](rule1: Rule[In, Out], rules: Rule[In, Out]*): RuleSet[In, Out] =
      RuleSet(rule1 +: rules.toVector)

    val empty: RuleSet[Any, Nothing] = RuleSet(Vector.empty)
  }

  final case class Rule[-In, +Out](condition: Condition[In], action: Action[In, Out])

  sealed trait Expr[-In, +Out] { self =>
    def eval(in: In): Out = Expr.eval(in, self)
    final def ++[In1 <: In, Fields1, Fields2](that: Expr[In1, Facts[Fields2]])(implicit
      ev: Out <:< Facts[Fields1]
    ): Expr[In1, Facts[Fields1 & Fields2]] =
      Expr.CombineFacts(self.widenOut[Facts[Fields1]], that)
    def &&[In1 <: In, Fields1, Fields2](that: Expr[Facts[Fields2], Boolean])(implicit
      ev: In1 <:< Facts[Fields1],
      evOut: Out <:< Boolean
    ): Expr[Facts[Fields1 & Fields2], Boolean] =
      Expr.And(self.widenOut.widenIn, that)

    def ||[In1 <: In, In2 <: In, Fields1, Fields2](that: Expr[Facts[Fields2], Boolean])(implicit
      ev: In1 <:< Facts[Fields1],
      evOut: Out <:< Boolean
    ): Expr[Facts[Fields1 & Fields2], Boolean] =
      Expr.Or(self.widenOut.widenIn, that)

    def ===[In1 <: In, Out1 >: Out, Fields1, Fields2](that: Expr[Facts[Fields2], Out1])(implicit
      ev: In1 <:< Facts[Fields1]
    ): Expr[Facts[Fields1 & Fields2], Boolean] =
      Expr.EqualTo(self.widenIn, that)

    def !=[In1 <: In, Out1 >: Out, Fields1, Fields2](that: Expr[Facts[Fields2], Out1])(implicit
      ev: In1 <:< Facts[Fields1]
    ): Expr[Facts[Fields1 & Fields2], Boolean] =
      !(self === that)

    def <[In1 <: In, Out1 >: Out, Fields1, Fields2](that: Expr[Facts[Fields2], Out1])(implicit
      ev: In1 <:< Facts[Fields1]
    ): Expr[Facts[Fields1 & Fields2], Boolean] =
      Expr.LessThan(self.widenIn, that)

    def <=[In1 <: In, Out1 >: Out, Fields1, Fields2](that: Expr[Facts[Fields2], Out1])(implicit
      ev: In1 <:< Facts[Fields1]
    ): Expr[Facts[Fields1 & Fields2], Boolean] =
      (self.widenOut < that) || (self === that)

    def >[In1 <: In, Out1 >: Out, Fields1, Fields2](that: Expr[Facts[Fields2], Out1])(implicit
      ev: In1 <:< Facts[Fields1]
    ): Expr[Facts[Fields1 & Fields2], Boolean] =
      !(self <= that)
    def >=[In1 <: In, Out1 >: Out, Fields1, Fields2](that: Expr[Facts[Fields2], Out1])(implicit
      ev: In1 <:< Facts[Fields1]
    ): Expr[Facts[Fields1 & Fields2], Boolean] =
      !(self < that)

    def *[In1 <: In, Out1 >: Out, Fields1, Fields2](
      that: Expr[Facts[Fields2], Out1]
    )(implicit numeric: Numeric[Out1], ev: In1 <:< Facts[Fields1]): Expr[Facts[Fields1 & Fields2], Out1] =
      Expr.BinaryNumericOp(self.widenIn, that.widenOut, Expr.NumericBinOpType.Multiply, numeric)

    def /[In1 <: In, Out1 >: Out, Fields1, Fields2](
      that: Expr[Facts[Fields2], Out1]
    )(implicit numeric: Numeric[Out1], ev: In1 <:< Facts[Fields1]): Expr[Facts[Fields1 & Fields2], Out1] =
      Expr.BinaryNumericOp(self.widenIn, that.widenOut, Expr.NumericBinOpType.Divide, numeric)

    def +[In1 <: In, Out1 >: Out, Fields1, Fields2](
      that: Expr[Facts[Fields2], Out1]
    )(implicit numeric: Numeric[Out1], ev: In1 <:< Facts[Fields1]): Expr[Facts[Fields1 & Fields2], Out1] =
      Expr.BinaryNumericOp(self.widenIn, that.widenOut, Expr.NumericBinOpType.Add, numeric)

    def -[In1 <: In, Out1 >: Out, Fields1, Fields2](
      that: Expr[Facts[Fields2], Out1]
    )(implicit numeric: Numeric[Out1], ev: In1 <:< Facts[Fields1]): Expr[Facts[Fields1 & Fields2], Out1] =
      Expr.BinaryNumericOp(self.widenIn, that.widenOut, Expr.NumericBinOpType.Subtract, numeric)

    def unary_!(implicit ev: Out <:< Boolean): Expr[In, Boolean] = Expr.Not(self.widenOut)

    def ifTrue[In1 <: In, Out1 >: Out](thenExpr: Expr[In1, Out1])(implicit
      ev: Out <:< Boolean
    ): Expr.IfTrue[In1, Out1] =
      Expr.IfTrue(self.widenOut, thenExpr)

    def widenOut[Out2](implicit ev: Out <:< Out2): Expr[In, Out2] =
      self.asInstanceOf[Expr[In, Out2]]

    def widenIn[In1 <: In, In2, Fields1, Fields2](implicit
      ev: In1 <:< Facts[Fields1],
      ev2: In2 <:< Facts[Fields2]
    ): Expr[In2, Out] =
      self.asInstanceOf[Expr[In2, Out]]
  }

  object Generic {
    import zio.Chunk
    final case class FieldSpec[A](name: String, fieldType: PrimitiveType[A]) {
      type Type
    }

    object FieldSpec {
      def forField[A](name: String)(implicit pt: PrimitiveType[A]): FieldSpec[A] =
        FieldSpec(name, pt)
      // lifts the path dependent type to a type parameter
      type WithFieldType[A, B] = FieldSpec[A] { type Type = B }
    }

    val age    = FieldSpec("age", PrimitiveType.IntType)
    val name   = FieldSpec("name", PrimitiveType.StringType)
    val street = FieldSpec("street", PrimitiveType.StringType)
    val isMale = FieldSpec("isMale", PrimitiveType.BooleanType)

    def testEquality[A, B](l: FieldSpec.WithFieldType[A, B], r: FieldSpec.WithFieldType[A, B]): Unit =
      () //l.name == r.name && l.fieldType == r.fieldType
    final case class Field[A](fieldSpec: FieldSpec[A], value: A)

    final class DynamicRecord private (private val map: Map[(String, PrimitiveType[_]), Any]) {
      def ++[A](that: DynamicRecord): DynamicRecord =
        new DynamicRecord(map ++ that.map)

      def add[A](name: String, value: A)(implicit pt: PrimitiveType[A]): DynamicRecord =
        new DynamicRecord(map + ((name, pt) -> value))

      def get[A](name: String)(implicit pt: PrimitiveType[A]): Option[A] =
        map.get((name, pt)).asInstanceOf[Option[A]]
    }

    final class Record[+Fields] private (map: Map[FieldSpec[_], Any]) { self =>
      def values: Chunk[Any] = Chunk.fromIterable(map.values)

      def add[A](fs: FieldSpec[A], value: A): Record[Fields with (fs.Type, A)] =
        new Record[Fields with (fs.Type, A)](map.updated(fs, value))

      def get[A](fs: FieldSpec[A])(implicit ev: Fields <:< (A, fs.Type)): A =
        map(fs).asInstanceOf[A]

      def updateWith[A](fs: FieldSpec[A], f: A => A)(implicit ev: Fields <:< (A, fs.Type)): Record[Fields] =
        new Record[Fields](map.updated(fs, f(map(fs).asInstanceOf[A])))

    }
    object Record {
      def empty: Record[Any] = new Record[Any](Map.empty)
    }

    Record.empty.add(age, 42).add(name, "John").add(isMale, true)
  }

  object Expr {
    final case class IfTrue[In, Out](condition: Expr[In, Boolean], thenExpr: Expr[In, Out]) {
      def ifFalse(that: Expr[In, Out]): Expr[In, Out] = IfThenElse(condition, thenExpr, that)
    }
    final case class Fact[In, K <: Singleton with String, V](
      factDef: FactDefinition.KeyValue[K, V],
      value: Expr[In, V]
    ) extends Expr[In, Facts[(K, V)]]
    final case class CombineFacts[In, V1, V2](
      fact1: Expr[In, Facts[V1]],
      fact2: Expr[In, Facts[V2]]
    ) extends Expr[In, Facts[V1 & V2]]
    final case class Constant[Out](value: Out, pt: PrimitiveType[Out]) extends Expr[Any, Out]

    final case class And[In](left: Expr[In, Boolean], right: Expr[In, Boolean]) extends Expr[In, Boolean]

    final case class Or[In](left: Expr[In, Boolean], right: Expr[In, Boolean])    extends Expr[In, Boolean]
    final case class Not[In](condition: Expr[In, Boolean])                        extends Expr[In, Boolean]
    final case class EqualTo[In, Out](left: Expr[In, Out], right: Expr[In, Out])  extends Expr[In, Boolean]
    final case class LessThan[In, Out](left: Expr[In, Out], right: Expr[In, Out]) extends Expr[In, Boolean]
    final case class Input[In](tag: EngineType[In])                               extends Expr[In, In]

    final case class Get[In, K <: Singleton with String, V](
      expr: Expr[In, Facts[(K, V)]],
      factDef: FactDefinition.KeyValue[K, V]
    ) extends Expr[In, V]

    final case class Pipe[In, Out1, Out2](left: Expr[In, Out1], right: Expr[Out1, Out2]) extends Expr[In, Out2]
    final case class BinaryNumericOp[In, Out](
      left: Expr[In, Out],
      right: Expr[In, Out],
      op: NumericBinOpType,
      pt: Numeric[Out]
    ) extends Expr[In, Out]
    final case class IfThenElse[In, Out](
      condition: Expr[In, Boolean],
      thenExpr: Expr[In, Out],
      elseExpr: Expr[In, Out]
    ) extends Expr[In, Out]

    sealed trait NumericBinOpType
    object NumericBinOpType {
      case object Add      extends NumericBinOpType
      case object Subtract extends NumericBinOpType
      case object Multiply extends NumericBinOpType
      case object Divide   extends NumericBinOpType
      case object Modulo   extends NumericBinOpType
    }

    implicit def apply[Out](value: Out)(implicit pt: PrimitiveType[Out]): Expr[Any, Out] =
      Constant(value, pt)

    def eval[In, Out](in: In, expr: Expr[In, Out]): Out =
      evalWithType(in, expr)._2

    def evalWithType[In, Out](in: In, expr: Expr[In, Out]): (EngineType[Out], Out) =
      expr match {
        case Fact(factDef, value) =>
          implicit val pt = factDef.tag
          val result      = Facts.empty.add(factDef, eval(in, value))
          (EngineType.fromFacts(result).asInstanceOf[EngineType[Out]], result)
        case CombineFacts(fact1, fact2) =>
          val result = eval(in, fact1) ++ eval(in, fact2)
          (EngineType.fromFacts(result).asInstanceOf[EngineType[Out]], result)
        case Constant(value, pt) =>
          implicit val pt0: PrimitiveType[Out] = pt.asInstanceOf[PrimitiveType[Out]]
          (EngineType.fromPrimitive[Out], value)
        case Pipe(lhs, rhs) => evalWithType(eval(in, lhs), rhs)
        case IfThenElse(cond, thenExpr, elseExpr) =>
          if (eval(in, cond)) evalWithType(in, thenExpr) else evalWithType(in, elseExpr)
        case And(left, right) =>
          (EngineType.fromPrimitive[Boolean].asInstanceOf[EngineType[Out]], eval(in, left) && eval(in, right))
        case Or(left, right) =>
          (EngineType.fromPrimitive[Boolean].asInstanceOf[EngineType[Out]], eval(in, left) || eval(in, right))
        case Not(cond) => (EngineType.fromPrimitive[Boolean].asInstanceOf[EngineType[Out]], !eval(in, cond))
        case EqualTo(lhs, rhs) =>
          val (leftType, lft) = evalWithType(in, lhs)
          val right           = eval(in, rhs)

          val tag = EngineType.fromPrimitive[Boolean].asInstanceOf[EngineType[Out]]
          val out = leftType.equals(lft, right)

          tag -> out
        case LessThan(lhs, rhs) =>
          val (leftType, left) = evalWithType(in, lhs)
          val right            = eval(in, rhs)

          val tag = EngineType.fromPrimitive[Boolean].asInstanceOf[EngineType[Out]]
          val out = leftType.lessThan(left, right).asInstanceOf[Out]

          tag -> out

        case Input(engineType) => (engineType.asInstanceOf[EngineType[Out]], in.asInstanceOf[Out])
        case Get(expr, factDef) =>
          val facts = eval(in, expr)
          val value = Unsafe.unsafe(implicit u => facts.unsafe.get(factDef).asInstanceOf[Out])
          (factDef.tag.asInstanceOf[EngineType[Out]], value)
        case BinaryNumericOp(left, right, op, num0) =>
          val num = num0.asInstanceOf[Numeric[Out]]
          (EngineType.fromPrimitive(num.primitiveType), num(op)(eval(in, left), eval(in, right)))
      }

    def input[In](engineType: EngineType[In]): Expr[In, In] =
      Input(engineType)

    def get[In, K <: Singleton with String, V](
      expr: Expr[In, Facts[(K, V)]],
      factDef: FactDefinition.KeyValue[K, V]
    ): Expr[In, V] =
      Get(expr, factDef)

    def fact[In, K <: Singleton with String, V](
      factDef: FactDefinition.KeyValue[K, V],
      value: Expr[In, V]
    ): Expr[In, Facts[(K, V)]] =
      Fact(factDef, value)

    def ifThenElse[In, Out](
      condition: Expr[In, Boolean]
    )(thenExpr: Expr[In, Out], elseExpr: Expr[In, Out]): Expr[In, Out] =
      IfThenElse(condition, thenExpr, elseExpr)
  }

  final case class Condition[-In](expr: Expr[In, Boolean]) { self =>
    def &&[In1 <: In, Fields1, Fields2](that: Condition[Facts[Fields2]])(implicit
      ev: In1 <:< Facts[Fields1]
    ): Condition[Facts[Fields1 & Fields2]] =
      Condition(self.expr && that.expr)

    def ||[In1 <: In, Fields1, Fields2](that: Condition[Facts[Fields2]])(implicit
      ev: In1 <:< Facts[Fields1]
    ): Condition[Facts[Fields1 & Fields2]] =
      Condition(self.expr || that.expr)

    def unary_! : Condition[In] = Condition(!self.expr)

    def eval(input: In): Boolean = expr.eval(input)
  }
  object Condition {

    val always: Condition[Any] = Condition(Expr(value = true))
    val never: Condition[Any]  = Condition(Expr(value = false))

  }

  sealed trait Action[-In, +Out] {
    self =>
    def ++[In1 <: In, Out1 >: Out](that: Action[In1, Out1]): Action[In1, Out1] =
      Action.Concat(self, that)

    def >>>[Out2](that: Action[Out, Out2]): Action[In, Out2] =
      Action.Pipe(self, that)

    def eval(input: In): List[Out] =
      self match {
        case Action.Concat(left, right) => left.eval(input) ++ right.eval(input)
        case Action.Pipe(left, right)   => right.eval(left.eval(input))
        case Action.FromExpr(expr)      => List(expr.eval(input))
      }

  }

  object Action {
    final case class Concat[In, Out](left: Action[In, Out], right: Action[In, Out])       extends Action[In, Out]
    final case class Pipe[In, Out, Out2](left: Action[In, Out], right: Action[Out, Out2]) extends Action[In, Out2]
    final case class FromExpr[In, Out](expr: Expr[In, Out])                               extends Action[In, Out]

    def fromExpr[In, Out](expr: Expr[In, Out]): Action[In, Out] =
      FromExpr(expr)
  }

  object loyalty {
    object Flight {
      val id     = FactDefinition.string("id")
      val number = FactDefinition.string("number")

      val factsType =
        FactsType.empty.add(id).add(number)
    }

    object Customer {
      val id    = FactDefinition.string("id")
      val name  = FactDefinition.string("name")
      val email = FactDefinition.string("email")
      val phone = FactDefinition.string("phone")

      val factsType =
        FactsType.empty.add(id).add(name).add(email).add(phone)
    }

    object FlightBooking {
      val id       = FactDefinition.string("id")
      val customer = FactDefinition.facts("customer", Customer.factsType)
      val flight   = FactDefinition.facts("flight", Flight.factsType)
      val price    = FactDefinition.double("price")
      val status   = FactDefinition.string("status")

      val factsType =
        FactsType.empty.add(id).add(customer).add(flight).add(price).add(status)
    }

    object FlightBookingStatus {
      val Confirmed = FactDefinition.string("Confirmed")
      val Cancelled = FactDefinition.string("Cancelled")
      val Pending   = FactDefinition.string("Pending")
    }

    val statusCondition: Condition[Facts[("status", String) & ("Confirmed", String)]] =
      Condition(FlightBooking.status.get === FlightBookingStatus.Confirmed.get)

    val priceCondition: Condition[Facts[("price", Double)]] =
      Condition(FlightBooking.price.get < 1000.0)

    val both = statusCondition && priceCondition

    object LoyaltyAction {
      val actionType = FactDefinition.string("action_type")
      val points     = FactDefinition.int("points")
      val customer   = FactDefinition.string("customer")
    }

    object ActionType {
      val AddPoints     = Expr("add_points")
      val UpgradeTier   = Expr("upgrade_tier")
      val DowngradeTier = Expr("downgrade_tier")
    }

    val rule =
      Rule(
        both,
        Action.fromExpr((LoyaltyAction.actionType     := ActionType.AddPoints) ++ (LoyaltyAction.points := 100))
          ++ Action.fromExpr(LoyaltyAction.actionType := ActionType.UpgradeTier)
      )
  }
}
