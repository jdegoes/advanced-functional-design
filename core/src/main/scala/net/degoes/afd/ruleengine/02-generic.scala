/**
 * The next step in the evolution of a rule engine is to make it generic, across
 * all domains (not just an airline loyalty program).
 *
 * In this section, you will explore different ways to encode a generic rule
 * engine, and discover more powerful functional domains in the process.
 */
package net.degoes.afd.ruleengine

import net.degoes.afd.examples.loyalty.LoyaltyProgram
import net.degoes.afd.examples.loyalty.FlightBooking
import net.degoes.afd.examples.loyalty.LoyaltyTier
import net.degoes.afd.examples.loyalty.FlightBookingStatus

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
  final case class RuleEngine[In, Out](update: In => Option[Out])
  object RuleEngine {
    def fromRuleSet[In, Out](ruleSet: RuleSet[In, Out]): RuleEngine[In, Out] =
      RuleEngine(ruleSet.update)
  }

  final case class RuleSet[-In, +Out](rules: Vector[Rule[In, Out]]) { self =>
    def +[In1 <: In, Out1 >: Out](rule: Rule[In1, Out1]): RuleSet[In1, Out1] =
      RuleSet(self.rules :+ rule)

    def ++[In1 <: In, Out1 >: Out](that: RuleSet[In1, Out1]): RuleSet[In1, Out1] =
      RuleSet(self.rules ++ that.rules)

    def update(in: In): Option[Out] =
      self.rules.find(_.condition.eval(in)).map { rule =>
        rule.action.update(in)
      }
  }
  object RuleSet {
    val empty: RuleSet[Any, Nothing] = RuleSet(Vector.empty)
  }

  final case class Rule[-In, +Out](condition: Condition[In], action: Action[In, Out])

  final case class Condition[-In](eval: In => Boolean) { self =>
    def &&[In1 <: In](that: Condition[In1]): Condition[In1] =
      Condition(in => self.eval(in) && that.eval(in))

    def ||[In1 <: In](that: Condition[In1]): Condition[In1] =
      Condition(in => self.eval(in) || that.eval(in))

    def contramap[In2](f: In2 => In): Condition[In2] = Condition(in => self.eval(f(in)))

    def unary_! : Condition[In] = Condition(in => !self.eval(in))

  }
  object Condition {
    val always: Condition[Any] = constant(true)
    val never: Condition[Any]  = constant(false)

    def constant(value: Boolean): Condition[Any] = Condition(_ => value)
  }

  final case class Action[-In, +Out](update: In => Out) { self =>
    def >>>[Out2](that: Action[Out, Out2]): Action[In, Out2] =
      Action(self.update andThen that.update)

    def contramap[In2](f: In2 => In): Action[In2, Out] =
      Action.fromFunction(f) >>> self

    def flatMap[In1 <: In, Out2](f: Out => Action[In1, Out2]): Action[In1, Out2] =
      Action(in => f(self.update(in)).update(in))

    def map[Out2](f: Out => Out2): Action[In, Out2] =
      // flatMap(out => Action.constant(f(out)))
      self >>> Action.fromFunction(f)
  }
  object Action {
    def constant[Out](out: Out): Action[Any, Out]            = Action(_ => out)
    def fromFunction[In, Out](f: In => Out): Action[In, Out] = Action(f)
  }

  /**
   * As proof of genericity, implement the same API for the loyalty engine that
   * you developed in the previous section, but implement it atop the generic
   * rule engine you created in this section.
   */
  object loyalty {
    type LoyaltyEngine = RuleEngine[(FlightBooking, LoyaltyProgram), LoyaltyProgram]

    type LoyaltyRuleSet = RuleSet[(FlightBooking, LoyaltyProgram), LoyaltyProgram]
    
    type LoyaltyRule = Rule[(FlightBooking, LoyaltyProgram), LoyaltyProgram]
    object LoyaltyRule {
      def apply(condition: LoyaltyCondition, action: LoyaltyAction): LoyaltyRule =
        Rule(condition.contramap(_._1), action.contramap(_._2))
    }

    type LoyaltyCondition = Condition[FlightBooking]
    object LoyaltyCondition {
      def status(p: FlightBookingStatus => Boolean): LoyaltyCondition = 
        Condition(booking => p(booking.status))

      def price(p: Double => Boolean): LoyaltyCondition = 
        Condition(booking => p(booking.price))
    }

    type LoyaltyAction = Action[LoyaltyProgram, LoyaltyProgram]
    object LoyaltyAction {
      def apply(f: LoyaltyProgram => LoyaltyProgram): LoyaltyAction = Action(f)
      def adjustPoints(value: Int): LoyaltyAction =
        LoyaltyAction(program => program.copy(points = program.points + value))

      val downgradeTier: LoyaltyAction =
        LoyaltyAction(program =>
          program.copy(tier = program.tier match {
            case LoyaltyTier.Gold   => LoyaltyTier.Silver
            case LoyaltyTier.Silver => LoyaltyTier.Bronze
            case LoyaltyTier.Bronze => LoyaltyTier.Bronze
          })
        )

      val none: LoyaltyAction = LoyaltyAction(identity[LoyaltyProgram])

      val upgradeTier: LoyaltyAction =
        LoyaltyAction(program =>
          program.copy(tier = program.tier match {
            case LoyaltyTier.Bronze => LoyaltyTier.Silver
            case LoyaltyTier.Silver => LoyaltyTier.Gold
            case LoyaltyTier.Gold   => LoyaltyTier.Gold
          })
        )
    }
  }

  object example {
    import loyalty._
    
    val exampleAction: LoyaltyAction = 
      LoyaltyAction.upgradeTier >>> LoyaltyAction.adjustPoints(100)

    val exampleCondition =
      LoyaltyCondition.status(_ == FlightBookingStatus.Confirmed) &&
        LoyaltyCondition.price(_ > 1000)

    val exampleRule = LoyaltyRule(exampleCondition, exampleAction)

    val exampleRuleSet: LoyaltyRuleSet = RuleSet(Vector(exampleRule))

    val exampleEngine: LoyaltyEngine = RuleEngine.fromRuleSet(exampleRuleSet)

    def updateLoyaltyProgram(booking: FlightBooking, program: LoyaltyProgram): LoyaltyProgram =
      exampleEngine.update((booking, program)).getOrElse(program)
  }
}
