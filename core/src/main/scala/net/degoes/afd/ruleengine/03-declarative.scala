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

import net.degoes.afd.ruleengine.declarative.PrimitiveType.StringOrdering

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
    def apply[In, Out](rule1: Rule[In, Out], rules: Rule[In, Out]*): RuleSet[In, Out] = RuleSet(rule1 +: rules.toVector)

    val empty: RuleSet[Any, Nothing] = RuleSet(Vector.empty)
  }

  final case class Rule[-In, +Out](condition: Condition[In], action: Action[In, Out])

  sealed trait Expression[-In, +Out]

  object Expression {
    final case class Constant[Out](value: Out, pt: PrimitiveType[Out]) extends Expression[Any, Out]

    implicit def apply[Out](value: Out)(implicit pt: PrimitiveType[Out]): Expression[Any, Out] =
      Constant(value, pt)
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

    implicit case object StringOrdering extends PrimitiveType[String] {
      override def ordering: Ordering[String] = Ordering.String
    }

  }

  sealed trait Condition[-In] { self =>
    def &&[In1 <: In](that: Condition[In1]): Condition[In1] = Condition.And(self, that)
    def ||[In1 <: In](that: Condition[In1]): Condition[In1] = Condition.Or(self, that)
    def unary_! : Condition[In]                             = Condition.Not(self)

    def eval(in: In): Boolean =
      Condition.eval(self)(in)
  }
  object Condition {
    final case class Constant(value: Boolean)                           extends Condition[Any]
    final case class And[In](left: Condition[In], right: Condition[In]) extends Condition[In]
    final case class Or[In](left: Condition[In], right: Condition[In])  extends Condition[In]
    final case class Not[In](condition: Condition[In])                  extends Condition[In]
    final case class EqualTo[In](rhs: In, ordering: PrimitiveType[In])  extends Condition[In]
    final case class LessThan[In](rhs: In, ordering: PrimitiveType[In]) extends Condition[In]

    def constant(value: Boolean): Condition[Any] = Constant(value)
    val always: Condition[Any]                   = Constant(true)
    val never: Condition[Any]                    = Constant(false)
    def isEqualTo[In](value: In)(implicit ordering: PrimitiveType[In]): Condition[In] =
      EqualTo(value, ordering)

    def isLessThan[In](value: In)(implicit ordering: PrimitiveType[In]): Condition[In] =
      LessThan(value, ordering)

    def isGreaterThan[In](value: In)(implicit ordering: PrimitiveType[In]): Condition[In] =
      Not(isLessThan(value) || isEqualTo(value))

    def eval[In](condition: Condition[In])(in: In): Boolean = condition match {
      case Constant(value)         => value
      case And(left, right)        => eval(left)(in) && eval(right)(in)
      case Or(left, right)         => eval(left)(in) || eval(right)(in)
      case Not(condition)          => !eval(condition)(in)
      case EqualTo(rhs, ordering)  => ordering.ordering.compare(in, rhs) == 0
      case LessThan(rhs, ordering) => ordering.ordering.compare(in, rhs) < 0
    }
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
