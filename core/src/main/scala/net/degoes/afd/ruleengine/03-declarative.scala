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

  trait Expression

  sealed trait PrimitiveType[A]
  object PrimitiveType {
    implicit case object BooleanType extends PrimitiveType[Boolean]
    implicit case object ByteType    extends PrimitiveType[Byte]
    implicit case object CharType    extends PrimitiveType[Char]
    implicit case object IntType     extends PrimitiveType[Int]
    implicit case object LongType    extends PrimitiveType[Long]
    implicit case object FloatType   extends PrimitiveType[Float]
    implicit case object DoubleType  extends PrimitiveType[Double]
    implicit case object StringType  extends PrimitiveType[String]
  }

  // Note: poof constrain types
  def constrainedPolymorphicFunction[A](a: A)(implicit pt: PrimitiveType[A]): A = a

  constrainedPolymorphicFunction(1)
  constrainedPolymorphicFunction("1")
  constrainedPolymorphicFunction(true)

  // TODO spelling
  // Declarative encoding of the concept of orderability
  sealed trait Ordering[A] extends scala.math.Ordering[A] {
    private val delegate: scala.math.Ordering[A] = Ordering.toScalaOrdering(this)

    def compare(x: A, y: A): Int = delegate.compare(x, y)
  }
  object Ordering {
    implicit case object IntOrdering     extends Ordering[Int]
    implicit case object LongOrdering    extends Ordering[Long]
    implicit case object DoubleOrdering  extends Ordering[Double]
    implicit case object FloatOrdering   extends Ordering[Float]
    implicit case object StringOrdering  extends Ordering[String]
    implicit case object BooleanOrdering extends Ordering[Boolean]
    implicit case object ByteOrdering    extends Ordering[Byte]
    implicit case object CharOrdering    extends Ordering[Char]

    def toScalaOrdering[A](ord: Ordering[A]): scala.math.Ordering[A] = ord match {
      case IntOrdering     => scala.math.Ordering[Int]
      case LongOrdering    => scala.math.Ordering[Long]
      case DoubleOrdering  => scala.math.Ordering[Double]
      case FloatOrdering   => scala.math.Ordering[Float]
      case StringOrdering  => scala.math.Ordering[String]
      case BooleanOrdering => scala.math.Ordering[Boolean]
      case ByteOrdering    => scala.math.Ordering[Byte]
      case CharOrdering    => scala.math.Ordering[Char]
    }
  }

  sealed trait Condition[-In] { self =>

    def &&[In1 <: In](that: Condition[In1]): Condition[In1] = Condition.And(self, that)

    def ||[In1 <: In](that: Condition[In1]): Condition[In1] = Condition.Or(self, that)

    def eval(in: In): Boolean = Condition.eval(self)(in)

    def unary_! : Condition[In] = Condition.Not(self)

  }
  object Condition { self =>
    final case class Constant(value: Boolean)                           extends Condition[Any]
    final case class And[In](left: Condition[In], right: Condition[In]) extends Condition[In]
    final case class Or[In](left: Condition[In], right: Condition[In])  extends Condition[In]
    final case class Not[In](condition: Condition[In])                  extends Condition[In]
    final case class IsEqualTo[In](rhs: In, ordering: Ordering[In])     extends Condition[In]
    final case class LessThan[In](rhs: In, ordering: Ordering[In])      extends Condition[In]

    // Note: we can not use scala math ordering because its executable encoding
    // scala.math.Ordering
    // becasue we want declarative encoding

    val always: Condition[Any] = constant(true)

    val never: Condition[Any] = constant(false)

    def constant[In](value: Boolean): Condition[In] = Constant(value)

    def eval[In](condition: Condition[In])(in: In): Boolean = condition match {
      case Constant(value)     => value
      case And(left, right)    => eval(left)(in) && eval(right)(in)
      case Or(left, right)     => eval(left)(in) || eval(right)(in)
      case Not(condition)      => !eval(condition)(in)
      case IsEqualTo(rhs, ord) => ord.compare(in, rhs) == 0
      case LessThan(rhs, ord)  => ord.compare(in, rhs) < 0
    }

    def isEqualTo[In](in: In)(implicit ord: Ordering[In]): Condition[In] = IsEqualTo(in, ord)

    def isLessThan[In](in: In)(implicit ord: Ordering[In]): Condition[In] = LessThan(in, ord)

    def isGreaterThan[In](in: In)(implicit ord: Ordering[In]): Condition[In] = ??? // TODO LessThan(in, ord.reverse)

    // Note: fromFunction can not be solved due to serialization
  }

  // Recap actions: LoyalyAction is pure data - think of Out as pure data do not need to be fancy liek a function
  // think commands
  // - Add points
  // - Promote tier
  // - Send email
  // Outs are kind of instructons that tells us what to do.
  sealed trait Action[-In, +Out] { self =>

    def ++[In1 <: In, Out1 >: Out](that: Action[In1, Out1]): Action[In1, Out1] =
      Action.Concat(self, that)

    def >>>[Out2](that: Action[Out, Out2]): Action[In, Out2] = Action.Pipe(self, that)

    // Niecetohave: zip, at least it do not have scala functions in the executaable encoding imple
  }
  object Action { self =>
    final case class Concat[In, Out1, Out2](left: Action[In, Out1], right: Action[In, Out2]) extends Action[In, Out2]
    final case class Pipe[In, Out1, Out2](left: Action[In, Out1], right: Action[Out1, Out2]) extends Action[In, Out2]
    final case class Constant[Out](value: Out, pt: PrimitiveType[Out])                       extends Action[Any, Out]

    def constant[Out](out: Out)(implicit pt: PrimitiveType[Out]): Action[Any, Out] = Constant(out, pt)
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
