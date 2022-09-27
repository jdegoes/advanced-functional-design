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

    def collect[In, Out](pf: PartialFunction[In, Out]): RuleEngine[In, Out] =
      RuleEngine(in => pf.lift(in).map(List(_)))

    def constant[Out](out: Out): RuleEngine[Any, Out] = fromFunction(_ => out)

    def fromFunction[In, Out](f: In => Out): RuleEngine[In, Out] = RuleEngine(in => Some(List(f(in))))

    def fromRuleSet[In, Out](ruleSet: RuleSet[In, Out]): RuleEngine[In, Out] = ???
//      RuleEngine(ruleSet.update)
  }

  final case class RuleSet[-In, +Out](rules: Vector[Rule[In, Out]]) {
    self =>
    def +[In1 <: In, Out1 >: Out](that: Rule[In1, Out1]): RuleSet[In1, Out1] =
      RuleSet(self.rules :+ that)

    def ++[In1 <: In, Out1 >: Out](that: RuleSet[In1, Out1]): RuleSet[In1, Out1] =
      RuleSet(self.rules ++ that.rules)

    def addRule[In1 <: In, Out1 >: Out](that: Rule[In1, Out1]): RuleSet[In1, Out1] = self + that

//    def update(in: In): Option[List[Out]] =
//      self.rules.find(_.condition.eval(in)).map { rule =>
//        rule.action.update(in)
//      }
  }

  object RuleSet {
    def apply[In, Out](rule1: Rule[In, Out], rules: Rule[In, Out]*): RuleSet[In, Out] =
      RuleSet(rule1 +: rules.toVector)

    val empty: RuleSet[Any, Nothing] = RuleSet(Vector.empty)
  }

  final case class Rule[-In, +Out](condition: Condition[In], action: Action[In, Out])

  sealed trait Numeric[+A]

  object Numeric {
    case object IntNumeric    extends Numeric[Int]
    case object LongNumeric   extends Numeric[Long]
    case object ShortNumeric  extends Numeric[Short]
    case object ByteNumeric   extends Numeric[Byte]
    case object CharNumeric   extends Numeric[Char]
    case object FloatNumeric  extends Numeric[Float]
    case object DoubleNumeric extends Numeric[Double]
  }

  sealed trait Expr[-In, +Out] { self =>
    def &&[In1 <: In](that: Expr[In1, Boolean])(implicit ev: Out <:< Boolean): Expr[In1, Boolean] =
      Expr.And(self.widen, that)

    def ||[In1 <: In](that: Expr[In1, Boolean])(implicit ev: Out <:< Boolean): Expr[In1, Boolean] =
      Expr.Or(self.widen, that)

    def ===[In1 <: In, Out1 >: Out](that: Expr[In1, Out1]): Expr[In1, Boolean] =
      Expr.EqualTo(self.widen, that)

    def !=[In1 <: In, Out1 >: Out](that: Expr[In1, Out1]): Expr[In1, Boolean] =
      !(self === that)

    def <[In1 <: In, Out1 >: Out](that: Expr[In1, Out1]): Expr[In1, Boolean] =
      Expr.LessThan(self.widen, that)

    def <=[In1 <: In, Out1 >: Out](that: Expr[In1, Out1]): Expr[In1, Boolean] =
      (self.widen < that) || (self === that)

    def >[In1 <: In, Out1 >: Out](that: Expr[In1, Out1]): Expr[In1, Boolean] =
      !(self <= that)
    def >=[In1 <: In, Out1 >: Out](that: Expr[In1, Out1]): Expr[In1, Boolean] =
      !(self < that)

    def *[In1 <: In, Out1 >: Out](that: Expr[In1, Out1])(implicit numeric: Numeric[Out1]): Expr[In1, Out1] =
      Expr.BinaryNumericOp(self.widen, that.widen, Expr.NumericBinOpType.Multiply, numeric)

    def /[In1 <: In, Out1 >: Out](that: Expr[In1, Out1])(implicit numeric: Numeric[Out1]): Expr[In1, Out1] =
      Expr.BinaryNumericOp(self.widen, that.widen, Expr.NumericBinOpType.Divide, numeric)

    def +[In1 <: In, Out1 >: Out](that: Expr[In1, Out1])(implicit numeric: Numeric[Out1]): Expr[In1, Out1] =
      Expr.BinaryNumericOp(self.widen, that.widen, Expr.NumericBinOpType.Add, numeric)

    def -[In1 <: In, Out1 >: Out](that: Expr[In1, Out1])(implicit numeric: Numeric[Out1]): Expr[In1, Out1] =
      Expr.BinaryNumericOp(self.widen, that.widen, Expr.NumericBinOpType.Subtract, numeric)

    def unary_!(implicit ev: Out <:< Boolean): Expr[In, Boolean] = Expr.Not(self.widen)

    def widen[Out2](implicit ev: Out <:< Out2): Expr[In, Out2] =
      self.asInstanceOf[Expr[In, Out2]]
  }

  object Generic {
    import zio.Chunk
    final case class FieldSpec[A](name: String, fieldType: PrimitiveType[A]) {
      type Type
    }

    object FieldSpec {
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

    sealed trait Record[Fields] { self =>
      def values: Chunk[Any]

      def add[A](fs: FieldSpec[A], value: A): Record[Fields with A] =
        new Record[Fields with A] {
          def values: Chunk[Any] = self.values :+ value
        }

      def getInt(implicit ev: Fields <:< Int): Int =
        values.collect { case i: Int => i }.head
    }
    object Record {
      def empty: Record[Any] = new Record[Any] {
        def values: Chunk[Any] = Chunk.empty
      }
    }

    Record.empty.add(age, 42).add(name, "John").add(isMale, true)
  }

  object Expr {
    final case class Constant[Out](value: Out, pt: PrimitiveType[Out]) extends Expr[Any, Out]

    final case class And[In](left: Expr[In, Boolean], right: Expr[In, Boolean]) extends Expr[In, Boolean]

    final case class Or[In](left: Expr[In, Boolean], right: Expr[In, Boolean])    extends Expr[In, Boolean]
    final case class Not[In](condition: Expr[In, Boolean])                        extends Expr[In, Boolean]
    final case class EqualTo[In, Out](left: Expr[In, Out], right: Expr[In, Out])  extends Expr[In, Boolean]
    final case class LessThan[In, Out](left: Expr[In, Out], right: Expr[In, Out]) extends Expr[In, Boolean]
    final case class Identity[In](pt: PrimitiveType[In])                          extends Expr[In, In]

    final case class Pipe[In, Out1, Out2](left: Expr[In, Out1], right: Expr[Out1, Out2]) extends Expr[In, Out2]
    final case class BinaryNumericOp[In, Out](
      left: Expr[In, Out],
      right: Expr[In, Out],
      op: NumericBinOpType,
      pt: Numeric[Out]
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

    def input[In](implicit pt: PrimitiveType[In]): Expr[In, In] = Identity(pt)
  }

  sealed trait PrimitiveType[A] {
    def ordering: Ordering[A]
  }
  object PrimitiveType {
    implicit case object BooleanType extends PrimitiveType[Boolean] {
      override def ordering: Ordering[Boolean] = Ordering.Boolean
    }

    implicit case object IntType extends PrimitiveType[Int] {
      override def ordering: Ordering[Int] = Ordering.Int
    }

    implicit case object LongType extends PrimitiveType[Long] {
      override def ordering: Ordering[Long] = Ordering.Long
    }

    implicit case object FloatType extends PrimitiveType[Float] {
      override def ordering: Ordering[Float] = Ordering[Float]
    }

    implicit case object DoubleType extends PrimitiveType[Double] {
      override def ordering: Ordering[Double] = Ordering[Double]
    }

    implicit case object ShortType extends PrimitiveType[Short] {
      override def ordering: Ordering[Short] = Ordering.Short
    }

    implicit case object ByteType extends PrimitiveType[Byte] {
      override def ordering: Ordering[Byte] = Ordering.Byte
    }

    implicit case object CharType extends PrimitiveType[Char] {
      override def ordering: Ordering[Char] = Ordering.Char
    }

    implicit case object StringType extends PrimitiveType[String] {
      override def ordering: Ordering[String] = Ordering.String
    }

  }

  final case class Condition[-In](expr: Expr[In, Boolean]) { self =>
    def &&[In1 <: In](that: Condition[In1]): Condition[In1] =
      Condition(self.expr && that.expr)

    def ||[In1 <: In](that: Condition[In1]): Condition[In1] =
      Condition(self.expr || that.expr)
  }
  object Condition {

    val always: Condition[Any] = Condition(Expr(value = true))
    val never: Condition[Any]  = Condition(Expr(value = false))
    def isEqualTo[In](value: In)(implicit pt: PrimitiveType[In]): Condition[In] =
      Condition(Expr(value) === Expr.input[In])

    def isLessThan[In](value: In)(implicit pt: PrimitiveType[In]): Condition[In] =
      Condition(Expr.input[In] < Expr(value))

    def isGreaterThan[In](value: In)(implicit pt: PrimitiveType[In]): Condition[In] =
      Condition(Expr.input[In] > Expr(value))

  }

  sealed trait Action[-In, +Out] {
    self =>
    def ++[In1 <: In, Out1 >: Out](that: Action[In1, Out1]): Action[In1, Out1] =
      Action.Concat(self, that)

    def >>>[Out2](that: Action[Out, Out2]): Action[In, Out2] =
      Action.Then(self, that)

  }

  object Action {
    final case class Concat[In, Out](left: Action[In, Out], right: Action[In, Out])       extends Action[In, Out]
    final case class Then[In, Out, Out2](left: Action[In, Out], right: Action[Out, Out2]) extends Action[In, Out2]
    final case class Constant[Out](value: Out, pt: PrimitiveType[Out])                    extends Action[Any, Out]

    def constant[Out](value: Out)(implicit pt: PrimitiveType[Out]): Action[Any, Out] =
      Constant(value, pt)
  }

  object loyalty {

    val statusCondition: Condition[String] =
      Condition.isEqualTo("confirmed")

    val priceCondition: Condition[Int] =
      Condition.isLessThan(100)

    statusCondition && priceCondition
  }
}
