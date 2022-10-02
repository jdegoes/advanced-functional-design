package net.degoes.afd.ruleengine.graduation

import zio._
import scala.annotation._
import scala.language.implicitConversions

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
