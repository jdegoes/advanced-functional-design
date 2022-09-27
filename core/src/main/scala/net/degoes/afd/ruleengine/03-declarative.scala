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

import net.degoes.afd.examples.loyalty._
import net.degoes.afd.examples.loyalty.LoyaltyTier._

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

  sealed trait Json
  object Json {
    final case class String(value: String)            extends Json
    final case class Number(value: Double)            extends Json
    final case class Boolean(value: Boolean)          extends Json
    final case class Array(value: List[Json])         extends Json
    final case class Object(value: Map[String, Json]) extends Json
    case object Null                                  extends Json
  }

  // Its bacily a function
  // its code not data so we do not need to serialize/deserialize
  final case class RuleEngine[-In, +Out](update: In => Option[List[Out]]) { self =>
    def contramap[In2](f: In2 => In): RuleEngine[In2, Out] = RuleEngine(in2 => self.update(f(in2)))

    def orElse[In1 <: In, Out1 >: Out](that: RuleEngine[In1, Out1]): RuleEngine[In1, Out1] =
      RuleEngine[In1, Out1](in => self.update(in).orElse(that.update(in)))

    def updateWith[Out1 >: Out](in: In)(defaultOut: Out1, combine: (Out1, Out1) => Out1): Out1 = self.update(in) match {
      case None       => defaultOut
      case Some(outs) => outs.reduceOption(combine).getOrElse(defaultOut)
    }

  }
  object RuleEngine {
    val empty: RuleEngine[Any, Nothing] = RuleEngine(_ => None)

    def collect[In, Out](pf: PartialFunction[In, Out]): RuleEngine[In, Out] =
      RuleEngine(in => pf.lift(in).map(List(_)))

    def constant[Out](out: List[Out]): RuleEngine[Any, Out] = RuleEngine(_ => Some(out))

    def fromFunction[In, Out](f: In => Out): RuleEngine[In, Out] = RuleEngine(in => Some(List(f(in))))

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

  // Note: argumentation itsnot needed to be serializable becuase condition and action would be serialized
  final case class Rule[-In, +Out](condition: Condition[In], action: Action[In, Out])

  // Note: a recipe that when executed produces a value
  // 2 * 3
  // 2 * (3 + 123) / "foo".length
  // 2 * (3 +123) / scala.io.StdIn.readLine().length
  //
  // we can use that to to computation and manipulation inside action and condition
  // expression can be viewed as a very simple programming language (with out for loops and stuff like that)
  // a gloryfied calculater
  //
  // () => Out
  // 1 + 2
  // 1 + Math.pow(2, 3)
  // scala.io.StdIn.readLine().length() + scala.util.Random.nextInt()
  // In => Out
  sealed trait Expr[-In, +Out] { self =>

    // TODO slide 50 and so forth check palce
    final def +[In1 <: In, Out1 >: Out](that: Expr[In1, Out1])(implicit tag: Numeric[Out1]): Expr[In1, Out1] =
      Expr.BinaryNumericOp(self.widen, that, Expr.NumericBinOpType.Add, tag)

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

    def unary_!(implicit ev: Out <:< Boolean): Expr[In, Boolean] = Expr.Not(self.widen[Boolean])

    // Note: casting becasue its safe (tm)
    // John uses this trick instead of going the more verbose Expr.`final case class Widen`
    def widen[Out2](implicit ev: Out <:< Out2): Expr[In, Out2] = self.asInstanceOf[Expr[In, Out2]]

    // TODO input
  }

  sealed trait Numeric[A]
  object Numeric {
    implicit case object ByteIsNumeric extends Numeric[Byte]
  }

  object Expr {

    // Note: we can use this to be able  ...
    // implicit class ExprBoolSyntax[In](self: Expr[In, Boolean])
    // con and and on types not boolean will not give good error messages

    final case class Constant[Out](value: Out, tag: PrimitiveType[Out])         extends Expr[Any, Out]
    final case class And[In](left: Expr[In, Boolean], right: Expr[In, Boolean]) extends Expr[In, Boolean]
    final case class Or[In](left: Expr[In, Boolean], right: Expr[In, Boolean])  extends Expr[In, Boolean]
    final case class Not[In](condition: Expr[In, Boolean])                      extends Expr[In, Boolean]
    final case class EqualTo[In, Out](lhs: Expr[In, Out], rhs: Expr[In, Out])   extends Expr[In, Boolean]
    final case class LessThan[In, Out](lhs: Expr[In, Out], rhs: Expr[In, Out])  extends Expr[In, Boolean]
    // Niecetohave: division
    final case class Input[In, Out](tag: PrimitiveType[Out]) extends Expr[In, Out]
    // TODO 40
    final case class Pipe[In, Out1, Out2](left: Expr[In, Out1], right: Expr[Out1, Out2]) extends Expr[In, Out2]

    // TODO slide 45 and so forth
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

    def input[A](implicit tag: PrimitiveType[A]): Expr[A, A] = Input(tag)
    // Expr(123)
    // Expr("sdf")
    // Expr(<something that will no compilole>)
  }

  // Note: ensure the primitive types are in sync with ordering types
  sealed trait PrimitiveType[A] {
    def ordering: scala.math.Ordering[A]
  }
  object PrimitiveType {
    implicit case object BooleanType extends PrimitiveType[Boolean] {
      def ordering: scala.math.Ordering[Boolean] = scala.math.Ordering[Boolean]
    }
    implicit case object ByteType extends PrimitiveType[Byte] {
      def ordering: scala.math.Ordering[Byte] = scala.math.Ordering[Byte]
    }
    implicit case object CharType extends PrimitiveType[Char] {
      def ordering: scala.math.Ordering[Char] = scala.math.Ordering[Char]
    }
    implicit case object IntType extends PrimitiveType[Int] {
      def ordering: scala.math.Ordering[Int] = scala.math.Ordering[Int]
    }
    implicit case object LongType extends PrimitiveType[Long] {
      def ordering: scala.math.Ordering[Long] = scala.math.Ordering[Long]
    }
    implicit case object FloatType extends PrimitiveType[Float] {
      def ordering: scala.math.Ordering[Float] = scala.math.Ordering[Float]
    }
    implicit case object DoubleType extends PrimitiveType[Double] {
      def ordering: scala.math.Ordering[Double] = scala.math.Ordering[Double]
    }
    implicit case object StringType extends PrimitiveType[String] {
      def ordering: scala.math.Ordering[String] = scala.math.Ordering[String]
    }
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

    def ||[In1 <: In](that: Condition[In1]): Condition[In1] = Condition(self.expr || that.expr)

    def unary_! : Condition[In] = Condition(!expr)

  }
  object Condition { self =>

    // Note: we can not use scala math ordering because its executable encoding
    // scala.math.Ordering
    // becasue we want declarative encoding

    val always: Condition[Any] = constant(true)

    val never: Condition[Any] = constant(false)

    def constant[In](value: Boolean): Condition[In] = Condition(Expr(value))

    def isEqualTo[In](rhs: In)(implicit tag: PrimitiveType[In]): Condition[In] =
      Condition(Expr.input[In] === Expr(rhs))

    def isLessThan[In](rhs: In)(implicit tag: PrimitiveType[In]): Condition[In] =
      Condition(Expr.input[In] < Expr(rhs))

    def isGreaterThan[In](rhs: In)(implicit tag: PrimitiveType[In]): Condition[In] =
      Condition(Expr.input[In] > Expr(rhs))

    // Note: fromFunction can not be solved due to serialization
  }

  // Recap actions: LoyalyAction is pure data - think of Out as pure data do not need to be fancy liek a function
  // think commands
  // - Add points
  // - Promote tier
  // - Send email
  // Outs are kind of instructons that tells us what to do.

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

    val statusCondition: Condition[String] = Condition.isEqualTo("confirmed")

    val priceCondition: Condition[Double] = Condition.isLessThan(1000.0)

    // val statusCondition: Condition[String] =
    //   Condition.isEqualTo("confirmed").contramap[FlightBooking](_.status.toString)

    // val priceCondition: Condition[Double] = Condition.isLessThan(1000.0)

    // Note: problem
    val foo: Condition[String with Double] = statusCondition && priceCondition

    // FlightBookStatus is not a type in our world ... it would need soe adjustments of `object Ordering {`
    //    case class Ordering2[A, B](orderingA: Ordering[A], orderingB: Ordering[B]) extends Ordering[(A, B)]
    // John do not recommend to go down this path now ...

  }
}
