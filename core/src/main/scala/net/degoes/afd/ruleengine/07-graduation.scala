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

  sealed trait Numeric[A] {

    type NumericType = A

    import Expr.NumericBinOpType
    import Expr.NumericBinOpType._

    def add(left: A, right: A): A

    def subtract(left: A, right: A): A 

    def multiply(left: A, right: A): A

    def divide(left: A, right: A): A

    def modulo(left: A, right: A): A

    import PrimitiveType._
    def primitiveType: PrimitiveType[A] =
      (this match {
        case _: Numeric.ByteIsNumeric.type => PrimitiveType.ByteType
        case _: Numeric.CharIsNumeric.type => PrimitiveType.CharType
        case _: Numeric.IntIsNumeric.type => PrimitiveType.IntType
        case _: Numeric.LongIsNumeric.type => PrimitiveType.LongType
        case _: Numeric.FloatIsNumeric.type => PrimitiveType.FloatType
        case _: Numeric.DoubleIsNumeric.type => PrimitiveType.DoubleType
      }).asInstanceOf[PrimitiveType[A]]

    def apply(binOp: NumericBinOpType)(left: A, right: A): A =
      binOp match {
        case Add      => add(left, right)
        case Subtract => subtract(left, right)
        case Multiply => multiply(left, right)
        case Divide   => divide(left, right)
        case Modulo   => modulo(left, right)
      }

  }

  object Numeric {
    implicit case object ByteIsNumeric extends Numeric[Byte] {
      def add(left: Byte, right: Byte): Byte = (left + right).toByte

      def subtract(left: Byte, right: Byte): Byte = (left - right).toByte

      def multiply(left: Byte, right: Byte): Byte = (left * right).toByte

      def divide(left: Byte, right: Byte): Byte = (left / right).toByte

      def modulo(left: Byte, right: Byte): Byte = (left % right).toByte

    }
    implicit case object CharIsNumeric extends Numeric[Char] {
      def add(left: Char, right: Char): Char = (left + right).toChar

      def subtract(left: Char, right: Char): Char = (left - right).toChar

      def multiply(left: Char, right: Char): Char = (left * right).toChar

      def divide(left: Char, right: Char): Char = (left / right).toChar

      def modulo(left: Char, right: Char): Char = (left % right).toChar

    }
    implicit case object IntIsNumeric extends Numeric[Int] {
      def add(left: Int, right: Int): Int = (left + right)

      def subtract(left: Int, right: Int): Int = (left - right)

      def multiply(left: Int, right: Int): Int = (left * right)

      def divide(left: Int, right: Int): Int = (left / right)

      def modulo(left: Int, right: Int): Int = (left % right)
  
    }
    implicit case object LongIsNumeric extends Numeric[Long] {
      def add(left: Long, right: Long): Long = (left + right)

      def subtract(left: Long, right: Long): Long = (left - right)

      def multiply(left: Long, right: Long): Long = (left * right)

      def divide(left: Long, right: Long): Long = (left / right)

      def modulo(left: Long, right: Long): Long = (left % right)
  
    }
    implicit case object FloatIsNumeric extends Numeric[Float] {
      def add(left: Float, right: Float): Float = (left + right)

      def subtract(left: Float, right: Float): Float = (left - right)

      def multiply(left: Float, right: Float): Float = (left * right)

      def divide(left: Float, right: Float): Float = (left / right)

      def modulo(left: Float, right: Float): Float = (left % right)
    
    }
    implicit case object DoubleIsNumeric extends Numeric[Double] {
      def add(left: Double, right: Double): Double = (left + right)

      def subtract(left: Double, right: Double): Double = (left - right)

      def multiply(left: Double, right: Double): Double = (left * right)

      def divide(left: Double, right: Double): Double = (left / right)

      def modulo(left: Double, right: Double): Double = (left % right)
   
    }

  }

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

    def eval(in: In): Out = Expr.eval(in, self)

    def ifTrue[In1 <: In, Out2](ifTrue: Expr[In1, Out2])(implicit ev: Out <:< Boolean): Expr.IfTrue[In1, Out2] =
      Expr.IfTrue(self.widen[Boolean], ifTrue)

    final def unary_!(implicit ev: Out <:< Boolean): Expr[In, Boolean] =
      Expr.Not(self.widen)

    final def widen[Out2](implicit ev: Out <:< Out2): Expr[In, Out2] = self.asInstanceOf[Expr[In, Out2]]

  }

  object Expr {

    final case class IfTrue[In, Out](condition: Expr[In, Boolean], ifTrue: Expr[In, Out]) {
      def otherwise(ifFalse: Expr[In, Out]) = IfThenElse(condition, ifTrue, ifFalse)
    }

    final case class Fact[In, K <: Singleton with String, V](factDef: FactDefinition.KeyValue[K, V], value: Expr[In, V])
        extends Expr[In, Facts[(K, V)]]
    final case class CombineFacts[In, V1, V2](
      left: Expr[In, Facts[V1]],
      right: Expr[In, Facts[V2]]
    ) extends Expr[In, Facts[V1 & V2]]
    final case class Constant[Out](value: Out, tag: EngineType[Out])         extends Expr[Any, Out]
    final case class And[In](left: Expr[In, Boolean], right: Expr[In, Boolean]) extends Expr[In, Boolean]
    final case class Or[In](left: Expr[In, Boolean], right: Expr[In, Boolean])  extends Expr[In, Boolean]
    final case class Not[In](condition: Expr[In, Boolean])                      extends Expr[In, Boolean]
    final case class EqualTo[In, Out](lhs: Expr[In, Out], rhs: Expr[In, Out])   extends Expr[In, Boolean]
    final case class LessThan[In, Out](lhs: Expr[In, Out], rhs: Expr[In, Out])  extends Expr[In, Boolean]
    final case class Input[In](engineType: EngineType[In]) extends Expr[In, In]
    final case class Get[In, K <: Singleton with String, V](expr: Expr[In, Facts[(K, V)]], factDef: FactDefinition.KeyValue[K, V]) extends Expr[In, V]
    final case class Pipe[In, Out1, Out2](left: Expr[In, Out1], right: Expr[Out1, Out2]) extends Expr[In, Out2]
    final case class BinaryNumericOp[In, Out](
      lhs: Expr[In, Out],
      rhs: Expr[In, Out],
      op: NumericBinOpType,
      tag: Numeric[Out]
    ) extends Expr[In, Out]
    final case class IfThenElse[In, Out](condition: Expr[In, Boolean], ifTrue: Expr[In, Out], ifFalse: Expr[In, Out])
        extends Expr[In, Out]
    sealed trait NumericBinOpType
    object NumericBinOpType {
      case object Add      extends NumericBinOpType
      case object Subtract extends NumericBinOpType
      case object Multiply extends NumericBinOpType
      case object Divide   extends NumericBinOpType
      case object Modulo   extends NumericBinOpType
    }

    implicit def apply[Out](out: Out)(implicit tag: PrimitiveType[Out]): Expr[Any, Out] =
      Constant(out, EngineType.Primitive(tag))

    implicit def apply[Out](out: Facts[Out]): Expr[Any, Facts[Out]] = 
      Constant(out, EngineType.fromFacts(out))

      
    private def eval[In, Out](in: In, expr: Expr[In, Out]): Out = evalWithType(in, expr)._2

    private def evalWithType[In, Out](in: In, expr: Expr[In, Out]): (EngineType[Out], Out) = 
      expr match {
        case Fact(factDef, value) => 
          implicit val tag = factDef.tag

          val result = Facts.empty.add(factDef, value)
          (EngineType.fromFacts(result).asInstanceOf[EngineType[Out]], result)

        case CombineFacts(lhs, rhs) => 
          val left = eval(in, lhs)
          val right = eval(in, rhs)
          val results = left ++ right
          (EngineType.fromFacts(results).asInstanceOf[EngineType[Out]], results)
        
        case Constant(value, tag) => 
          (tag.asInstanceOf[EngineType[Out]], value)

        case And(lhs, rhs) => 
          val left = eval(in, lhs)
          val right = eval(in, rhs)
          (EngineType.fromPrimitive[Boolean].asInstanceOf[EngineType[Out]], left && right)

        case Or(lhs, rhs) =>
          val left = eval(in, lhs)
          val right = eval(in, rhs)
          (EngineType.fromPrimitive[Boolean].asInstanceOf[EngineType[Out]], left || right)

        case Not(condition) => 
          (EngineType.fromPrimitive[Boolean].asInstanceOf[EngineType[Out]], !eval(in, condition))

        case EqualTo(lhs, rhs) =>
          val (leftType, left) = evalWithType(in, lhs)
          val (rightType, right) = evalWithType(in, rhs)
          
          import PrimitiveType._

          (EngineType.fromPrimitive(PrimitiveType[Boolean]).asInstanceOf[EngineType[Out]],
          leftType.equals(left, right))

        case LessThan(lhs, rhs) =>
          val (leftType, left) = evalWithType(in, lhs)
          val (rightType, right) = evalWithType(in, rhs)
          
          import PrimitiveType._

          (EngineType.fromPrimitive(PrimitiveType[Boolean]).asInstanceOf[EngineType[Out]],
          leftType.lessThan(left, right))

        case Input(engineType) => 
          engineType.asInstanceOf[EngineType[Out]] -> in.asInstanceOf[Out]
          
        case Get(expr, fd) =>
          val facts = eval(in, expr)

          val value = Unsafe.unsafe { implicit u =>
            facts.unsafe.get(fd).asInstanceOf[Out]
          }
          (fd.tag.asInstanceOf[EngineType[Out]],value)
        
        case Pipe(lhs, rhs) => evalWithType(eval(in, lhs), rhs)          
          
        case BinaryNumericOp(lhs, rhs, op, tag0) => 
          val tag = tag0.asInstanceOf[Numeric[Out]]
          val left: Out  = eval(in, lhs)
          val right: Out = eval(in, rhs) 
          (EngineType.fromPrimitive(tag.primitiveType), tag(op)(left, right))

        case IfThenElse(condition, ifTrue, ifFalse) =>
          val bool = eval(in, condition)
          if (bool) evalWithType(in, ifTrue)
          else evalWithType(in, ifFalse)

      }

    def fact[In, K <: Singleton with String, V](factDef: FactDefinition.KeyValue[K, V], value: Expr[In, V]) =
      Fact(factDef, value)

    def ifThenElse[In, Out](
      condition: Expr[In, Boolean]
    )(ifTrue: Expr[In, Out], ifFalse: Expr[In, Out]): Expr[In, Out] =
      Expr.IfThenElse(condition, ifTrue, ifFalse)

    def input[A](engineType: EngineType[A]): Expr[A, A] = Input(engineType)

  }

  class FactsType[KeyValues] private (private val definitions: Chunk[FactDefinition[_]]) { self =>
    def ++ [KeyValues2](that: FactsType[KeyValues2]): FactsType[KeyValues & KeyValues2] =
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
        def name: N               = name0
        def tag: EngineType[T] = fact0
      }

    def facts[N <: Singleton with String, Fields](name: N, factsType: FactsType[Fields]): KeyValue[N, Facts[Fields]] =
      FactDefinition[N, Facts[Fields]](name, EngineType.Composite(factsType))

   def prim[N <: Singleton with String, T](name0: N)(implicit tag0: PrimitiveType[T]): KeyValue[N, T] =
      new FactDefinition[(N, T)] {
        type Key   = N
        type Value = T
        def name: N               = name0
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

  final case class RuleEngine[-In, +Out](update: In => Option[List[Out]]) { self =>
    def contramap[In2](f: In2 => In): RuleEngine[In2, Out] =
      RuleEngine(in => self.update(f(in)))

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

    def fromFunction[In, Out](f: In => Out): RuleEngine[In, Out] = RuleEngine(in => Some(List(f(in))))

    def fromRuleSet[In, Out](ruleSet: RuleSet[In, Out]): RuleEngine[In, Out] = {
      val update: In => Option[List[Out]] = execute(ruleSet, _)

      RuleEngine(update)
    }

    private def execute[In, Out](ruleSet: RuleSet[In, Out], in: In): Option[List[Out]] =
      ruleSet.rules.find(_.condition.eval(in)).map { rule =>
        rule.action.eval(in)
      }
  }

  final case class Rule[-In, +Out](condition: Condition[In], action: Action[In, Out])

  final case class RuleSet[-In, +Out](rules: Vector[Rule[In, Out]]) { self =>

    def +[In1 <: In, Out1 >: Out](that: Rule[In1, Out1]): RuleSet[In1, Out1] =
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
  sealed abstract case class Facts[+Types] private (private val data: Map[FactDefinition[_], Any]) { self =>
    def ++[Types2](that: Facts[Types2]): Facts[Types & Types2] =
      new Facts[Types & Types2](data ++ that.data) {}

    def definitions: Chunk[FactDefinition[_]] = Chunk.fromIterable(data.keys)
    
    override def equals(that: Any): Boolean = 
      that match {
        case that: Facts[_] => self.data == that.data
        case _ => false
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

  final case class Condition[-In](expr: Expr[In, Boolean]) { self =>

    def eval(in: In) = expr.eval(in)

    def &&[In1 <: In](that: Condition[In1]): Condition[In1] =
      Condition(self.expr && that.expr)

    def ||[In1 <: In](that: Condition[In1]): Condition[In1] =
      Condition(self.expr && that.expr)

    def unary_! : Condition[In] = Condition(!expr)
  }

  object Condition {
    val always: Condition[Any] = constant(true)
    val never: Condition[Any]  = constant(false)

    def constant[In](value: Boolean): Condition[In] = Condition(Expr(value))
  }

  sealed trait Action[-In, +Out] { self =>
    def ++[In1 <: In, Out1 >: Out](that: Action[In1, Out1]): Action[In1, Out1] =
      Action.Concat(self, that)

    def >>>[Out2](that: Action[Out, Out2]): Action[In, Out2] =
      Action.Pipe(self, that)

    def eval(in: In): List[Out] = 
        self match {
          case Action.Concat(left, right) =>
            left.eval(in) ++ right.eval(in)

          case Action.Pipe(lhs, rhs) => lhs.eval(in).flatMap(rhs.eval(_))
          
          case Action.FromExpr(expr) =>
            List(expr.eval(in))
        }
  }
  object Action {
    final case class Concat[In, Out](left: Action[In, Out], right: Action[In, Out])          extends Action[In, Out]
    final case class Pipe[In, Out1, Out2](left: Action[In, Out1], right: Action[Out1, Out2]) extends Action[In, Out2]
    final case class FromExpr[In, Out](expr: Expr[In, Out])                                  extends Action[In, Out]

    def fromExpr[In, Out](expr: Expr[In, Out]): Action[In, Out] = FromExpr(expr)
  
  }

  object loyalty {
    import net.degoes.afd.examples.loyalty._
    import net.degoes.afd.examples.loyalty.LoyaltyTier._

    object Flights {
      val id = FactDefinition.string("id")
      val number = FactDefinition.string("number")

      val factsType =
        FactsType.empty.add(id).add(number)
    }

    object Customer {
      val id = FactDefinition.string("id")
      val name = FactDefinition.string("name")
      val email = FactDefinition.string("email")
      val phone = FactDefinition.string("phone")

      val factsType =
        FactsType.empty.add(id).add(name).add(email).add(phone)
    }

    object FlightBooking {
      val id       = FactDefinition.string("id")
      val customer = FactDefinition.facts("customer", Customer.factsType) // FIXME: Support nested data
      val flight   = FactDefinition.facts("fligh", Flights.factsType) // FactDefinition.string("flight")   // FIXME: Suppor nested data
      val price    = FactDefinition.double("price")
      val status   = FactDefinition.string("status")

      val factsType = 
        FactsType.empty.add(id).add(customer).add(flight).add(price).add(status)
    }

    object FlightBookingStatus {
      val Confirmed = Expr("Confirmed")
      val Cancelled = Expr("Cancelled")
      val Pending   = Expr("Pending")
    }

    object LoyaltyProgram {
      val tier   = FactDefinition.string("tier")
      val points = FactDefinition.int("points")
    }

    object LoyaltyAction {
      val actionType = FactDefinition.string("action_type")
      val points = FactDefinition.int("points")
      val customer = FactDefinition.string("customer")

      def update(program: LoyaltyProgram, action: Facts[_]): LoyaltyProgram =
         Unsafe.unsafe { implicit u =>
          action.unsafe.get(actionType) match {
            case ActionType.DowngradeTier => program.copy(tier = LoyaltyTier.Bronze)
            case ActionType.UpgradeTier => program.copy(tier = LoyaltyTier.Gold)
            case ActionType.AddPoints => action.unsafe.get(points) match {
              case Expr.Constant(value: Int, _) => program.copy(points = value)
              case _ => program
            }
            case _ => program
          } 
        }
      
      def update(program: LoyaltyProgram, actions: List[Facts[_]]) : LoyaltyProgram =
        actions.foldLeft(program)( (program, action) => update(program, action))
    }
    
    object ActionType {
      val AddPoints = Expr("add_points")
      val UpgradeTier = Expr("upgrade_tier")
      val DowngradeTier = Expr("downgrade_tier")
    }

    val statusCondition = Condition(FlightBooking.status.get === FlightBookingStatus.Confirmed)
    
    val priceCondition = Condition(FlightBooking.price.get > 1000.0)

    val both = statusCondition && priceCondition

    val addPointsExpr = (LoyaltyAction.actionType := ActionType.AddPoints) ++ (LoyaltyAction.points := 100)

    val upgradeTierExpr = LoyaltyAction.actionType := ActionType.UpgradeTier
    
    val actions = Action.fromExpr(addPointsExpr) ++ Action.fromExpr(upgradeTierExpr)

    val rule =  Rule(both, actions)

    val engine = RuleEngine.fromRuleSet(RuleSet(Vector(rule)))

    val facts = Facts.empty.add(FlightBooking.price, 100.0).add(FlightBooking.status, "Gold")

  }

}

