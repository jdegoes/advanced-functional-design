/**
 * The next step in the evolution of a rule engine is to make it generic, across
 * all domains (not just an airline loyalty program).
 *
 * In this section, you will explore different ways to encode a generic rule
 * engine, and discover more powerful functional domains in the process.
 */
package net.degoes.afd.ruleengine

import net.degoes.afd.ruleengine.basics.LoyaltyCondition

/**
 * Create a functional domain to express how rules translate into actions, which
 * can be leveraged across multiple business domains (loyalty points,
 * recommendations, customer onboarding, and event processing).
 *
 * For simplicity and ease-of-construction, use the executable encoding, but be
 * sure to leverage parametric polymorphism in your efforts to make the rule
 * engine generic.
 */
object generic {
  final case class RuleEngine[-In, +Out](update: In => Option[Out]) { self =>
    def >>>[Out2](that: RuleEngine[Out, Out2]): RuleEngine[In, Out2] =
      RuleEngine[In, Out2](in => self.update(in).flatMap(that.update))

    def contramap[In2](f: In2 => In): RuleEngine[In2, Out] =
      RuleEngine[In2, Out](in2 => self.update(f(in2)))

    def map[Out2](f: Out => Out2): RuleEngine[In, Out2] =
      RuleEngine[In, Out2](in => self.update(in).map(f))

    def zip[In2, Out2](that: RuleEngine[In2, Out2]): RuleEngine[(In, In2), (Out, Out2)] =
      RuleEngine[(In, In2), (Out, Out2)] { case (in1, in2) =>
        (self.update(in1), that.update(in2)) match {
          case (Some(out1), Some(out2)) => Some((out1, out2))
          case _                        => None
        }
      }

  }

  object RuleEngine {
    def fromRuleSet[In, Out](rules: RuleSet[In, Out]): RuleEngine[In, Out] =
      RuleEngine[In, Out](rules.update)
  }

  final case class RuleSet[-In, +Out](rules: Vector[Rule[In, Out]]) { self =>

    def +[In2 <: In, Out2 >: Out](rule: Rule[In2, Out2]): RuleSet[In2, Out2] =
      RuleSet[In2, Out2](rules :+ rule)
    def ++[In2 <: In, Out2 >: Out](that: RuleSet[In2, Out2]): RuleSet[In2, Out2] =
      RuleSet[In2, Out2](rules ++ that.rules)
    def addRule[In2 <: In, Out2 >: Out](rule: Rule[In2, Out2]): RuleSet[In2, Out2] =
      RuleSet[In2, Out2](rules :+ rule)
    def addRules[In2 <: In, Out2 >: Out](rules: Vector[Rule[In2, Out2]]): RuleSet[In2, Out2] =
      RuleSet[In2, Out2](this.rules ++ rules)

    def update(in: In): Option[Out] =
      self.rules.find(_.condition.eval(in)).map(_.action.eval(in))
  }

  object RuleSet {
    def empty[In, Out]: RuleSet[In, Out] = RuleSet(Vector.empty)
  }

  final case class Rule[-In, +Out](condition: Condition[In], action: Action[In, Out])

  final case class Condition[-In](eval: In => Boolean) { self =>
    def &&[In1 <: In](that: Condition[In1]): Condition[In1] =
      Condition[In1](in => self.eval(in) && that.eval(in))

    def ||[In1 <: In](that: Condition[In1]): Condition[In1] =
      Condition[In1](in => self.eval(in) || that.eval(in))

    def contramap[In1](f: In1 => In): Condition[In1] =
      Condition(in => self.eval(f(in)))

    def unary_! : Condition[In] = Condition[In](in => !self.eval(in))

  }

  object Condition {
    val always: Condition[Any] = constant(true)
    val never: Condition[Any]  = constant(false)

    def constant[In](value: Boolean): Condition[In] = Condition(_ => value)
  }

  final case class Action[-In, +Out](eval: In => Out) { self =>
    def map[Out2](f: Out => Out2): Action[In, Out2]    = Action(in => f(self.eval(in)))
    def contramap[In2](f: In2 => In): Action[In2, Out] = Action(in2 => self.eval(f(in2)))

    def flatMap[In2 <: In, Out2](f: Out => Action[In2, Out2]): Action[In2, Out2] =
      Action(in2 => f(self.eval(in2)).eval(in2))

    def zip[In2 <: In, Out2](that: Action[In2, Out2]): Action[In2, (Out, Out2)] =
      Action(in2 => (self.eval(in2), that.eval(in2)))
  }

  /**
   * As proof of genericity, implement the same API for the loyalty engine that
   * you developed in the previous section, but implement it atop the generic
   * rule engine you created in this section.
   */
  object loyalty {
    import net.degoes.afd.examples.loyalty._

    type Patch[A] = A => A

    type LoyaltyEngine = RuleEngine[FlightBooking, Patch[LoyaltyProgram]]

    type LoyaltyRule = Rule[FlightBooking, Patch[LoyaltyProgram]]

    object LoyaltyRule {
      def apply(condition: LoyaltyCondition, action: LoyaltyAction): LoyaltyRule =
        Rule(condition, action)
    }

    type LoyaltyRuleSet = RuleSet[FlightBooking, Patch[LoyaltyProgram]]

    type LoyaltyCondition = Condition[FlightBooking]

    object LoyaltyCondition {
      def status(f: FlightBookingStatus => Boolean): LoyaltyCondition =
        Condition(booking => f(booking.status))

      def price(f: Double => Boolean): LoyaltyCondition =
        Condition(booking => f(booking.price))
    }

    type LoyaltyAction = Action[Any, Patch[LoyaltyProgram]]

    object LoyaltyAction {
      def adjustPoints(points: Int): LoyaltyAction =
        Action(_ => (loyalty: LoyaltyProgram) => loyalty.copy(points = loyalty.points + points))

      def upgradeTier: LoyaltyAction =
        Action(_ =>
          (loyalty: LoyaltyProgram) =>
            loyalty.copy(tier = loyalty.tier match {
              case LoyaltyTier.Bronze => LoyaltyTier.Silver
              case LoyaltyTier.Silver => LoyaltyTier.Gold
              case LoyaltyTier.Gold   => LoyaltyTier.Gold
            })
        )

      def downgradeTier: LoyaltyAction =
        Action(_ =>
          (loyalty: LoyaltyProgram) =>
            loyalty.copy(tier = loyalty.tier match {
              case LoyaltyTier.Bronze => LoyaltyTier.Bronze
              case LoyaltyTier.Silver => LoyaltyTier.Bronze
              case LoyaltyTier.Gold   => LoyaltyTier.Silver
            })
        )
    }

    object example {
      val exampleCondition =
        LoyaltyCondition.status(_ == FlightBookingStatus.Confirmed) &&
          LoyaltyCondition.price(_ > 1000)

      val exampleAction  = LoyaltyAction.upgradeTier //++ LoyaltyAction.adjustPoints(100)
      val exampleRule    = Rule(exampleCondition, exampleAction)
      val exampleRuleSet = RuleSet.empty + exampleRule

      val engine = RuleEngine.fromRuleSet(exampleRuleSet)

      def updateLoyaltyProgram(booking: FlightBooking, program: LoyaltyProgram): LoyaltyProgram =
        engine.update(booking).map(_(program)).getOrElse(program)
    }
  }
}
