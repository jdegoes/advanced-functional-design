/**
 * The next step in the evolution of a rule engine is to make it generic, across
 * all domains (not just an airline loyalty program).
 *
 * In this section, you will explore different ways to encode a generic rule
 * engine, and discover more powerful functional domains in the process.
 */
package net.degoes.afd.ruleengine

import net.degoes.afd.examples.loyalty.LoyaltyProgram
import net.degoes.afd.examples.loyalty.LoyaltyTier.Bronze
import net.degoes.afd.examples.loyalty.LoyaltyTier.Gold
import net.degoes.afd.examples.loyalty.LoyaltyTier.Silver

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

  // TODO try to rename Out to Action
  // prestep rename already exsiting Action to something

  // TODO: change In => Option[Out]
  // to In => Option[List[Out]]
  // `Option[List[Out]]` used for orElse
  final case class RuleEngine[In, Out](update: In => Option[Out]) { self =>

    // TODO: check screen
    def >>>[Out2](that: RuleEngine[Out, Out2]): RuleEngine[In, Out2] =
      RuleEngine[In, Out2](in => self.update(in).flatMap(that.update))

    def orElse[In1 <: In, Out1 >: Out](that: RuleEngine[In1, Out1]): RuleEngine[In1, Out1] =
      RuleEngine[In1, Out1](in => self.update(in).orElse(that.update(in)))

    // add +/- types
    // def updateWith(defaultOut: Out, combine: (Out, Out) => Out): Out = self.update(in) match {
    //   case None => defaultOut
    //   case Some(outs) => ???
    // }

  }

  object RuleEngine {

    val empty: RuleEngine[Any, Nothing] = RuleEngine(_ => None)

    def constant[Out](out: Out): RuleEngine[Any, Out] = RuleEngine(_ => Some(out))

    def fromRuleSet[In, Out](ruleSet: RuleSet[In, Out]): RuleEngine[In, Out] =
      RuleEngine(ruleSet.update)

  }

  final case class RuleSet[-In, +Out](rules: Vector[Rule[In, Out]]) { self =>

    def ++[In1 <: In, Out1 >: Out](that: RuleSet[In1, Out1]): RuleSet[In1, Out1] = RuleSet(self.rules ++ that.rules)

    def +[In1 <: In, Out1 >: Out](that: Rule[In1, Out1]): RuleSet[In1, Out1] = RuleSet(self.rules :+ that)

    def addRule[In1 <: In, Out1 >: Out](that: Rule[In1, Out1]) = self + that

    // Note: has to be option because we might not produce anything
    def update(in: In): Option[Out] = self.rules.find(_.condition.eval(in)).map(rule => rule.action.update(in))

  }
  object RuleSet {
    def empty[In, Out]: RuleSet[In, Out] = RuleSet(Vector.empty)
  }

  final case class Rule[-In, +Out](condition: Condition[In], action: Action[In, Out])

  final case class Condition[-In](eval: In => Boolean) { self =>

    // TODO check with screen its copilot stuff
    // In1 related to In
    def &&[In1 <: In](that: Condition[In1]): Condition[In1] =
      Condition[In1](in => self.eval(in) && that.eval(in))

    // TODO check with screen its copilot stuff
    def ||[In1 <: In](that: Condition[In1]): Condition[In1] =
      Condition[In1](in => self.eval(in) || that.eval(in))

    // TODO check with screen its copilot stuff

    // In2 is used a convention not related to In
    def contramap[In2](f: In2 => In): Condition[In2] =
      Condition(in2 => self.eval(f(in2)))

    def unary_! : Condition[In] = Condition(in => !self.eval(in))

  }

  // we can not write status and price because they are domain specific
  object Condition {
    val always: Condition[Any]                      = constant(true)
    val never: Condition[Any]                       = constant(false)
    def constant[In](value: Boolean): Condition[In] = Condition(_ => value)
  }

// Note: recall
// only in -
// only out +
// to improve type inference
  final case class Action[-In, +Out](update: In => Out) { self =>

    def ++[In1 <: In, Out1 >: Out](that: Action[In1, Out1]): Action[In1, Out1] = ???
//
//      Action(in =? update(in))

    def >>>[Out2](that: Action[Out, Out2]): Action[In, Out2] =
      Action(in => that.update(update(in)))

    def contramap[In2](f: In2 => In): Action[In2, Out] =
      Action.fromFunction(f) >>> self

    // needed to have the actions in a for comprehension
    def flatMap[In1 <: In, Out2](f: Out => Action[In1, Out2]): Action[In1, Out2] =
      Action(in => f(self.update(in)).update(in))

    def map[Out2](f: Out => Out2): Action[In, Out2] =
      self.flatMap(out => Action.constant(f(out)))

    def zip[In1 <: In, Out2](that: Action[In1, Out2]): Action[In1, (Out, Out2)] =
      self.flatMap(out => that.map(out2 => (out, out2)))

  }
  object Action {
    def constant[Out](out: Out): Action[Any, Out] = Action(_ => out)

    def fromFunction[In, Out](f: In => Out): Action[In, Out] = Action(f)

    constant(42): Action[String, Int]

    // contra variance ... co variance
    // variant versus invariant
    // variance enables subtyping on generic data types
    // more on variance https://www.youtube.com/watch?v=aUmj7jnXet4

    trait Animal
    trait Dog extends Animal

    def bar(action: Action[Dog, _])    = ???
    def foo(action: Action[Animal, _]) = bar(action)

  }

  /**
   * As proof of genericity, implement the same API for the loyalty engine that
   * you developed in the previous section, but implement it atop the generic
   * rule engine you created in this section.
   */
  object loyalty {

    import net.degoes.afd.examples.loyalty._
    import net.degoes.afd.examples._

    // Note: its an updater that simply updates a
    type Patch[A] = A => A

    type LoyaltyEngine = RuleEngine[(FlightBooking, LoyaltyProgram), Patch[LoyaltyProgram]]

    type LoyaltyRuleSet = RuleSet[(FlightBooking, LoyaltyProgram), LoyaltyProgram]

    type LoyaltyRule = Rule[(FlightBooking, LoyaltyProgram), LoyaltyProgram]
    object LoyaltyRule {
      def apply(condition: LoyaltyCondition, action: LoyaltyAction): LoyaltyRule = ???

      // Rule(condition, action)
      // TODO get to work ???
//        Rule(condition.contramap(_._1), action.contramap(_._2))
    }

    type LoyaltyCondition = Condition[FlightBooking]
    object LoyaltyCondition {
      def status(f: FlightBookingStatus => Boolean): LoyaltyCondition = Condition(booking => f(booking.status))
      def price(f: Double => Boolean): LoyaltyCondition               = Condition(booking => f(booking.price))
    }
    // type LoyaltyAction = Action[LoyaltyProgram, LoyaltyProgram]
    type LoyaltyAction = Action[Any, Patch[LoyaltyProgram]]
    object LoyaltyAction {
      // def apply(f: LoyaltyProgram => LoyaltyProgram): LoyaltyAction = Action(f)
      def apply(f: LoyaltyProgram => LoyaltyProgram): LoyaltyAction = Action(_ => f)

      def adjustPoint(value: Int): LoyaltyAction =
        LoyaltyAction(program => program.copy(points = program.points + value))

      val downgradeTier: LoyaltyAction = LoyaltyAction(program =>
        program.copy(tier = program.tier match {
          case Bronze => Bronze
          case Silver => Bronze
          case Gold   => Silver
        })
      )

      val none: LoyaltyAction = LoyaltyAction(identity[LoyaltyProgram])

      val upgradeTier: LoyaltyAction = LoyaltyAction(program =>
        program.copy(tier = program.tier match {
          case Bronze => Silver
          case Silver => Gold
          case Gold   => Gold
        })
      )

    }

    object example {

      val exampleCondition = LoyaltyCondition.status(_ == FlightBookingStatus.Confirmed) &&
        LoyaltyCondition.price(_ > 1000)

      // note: debate >>> versus ++, >>> compiles but is incorrect because of ...
      val exampleAction = LoyaltyAction.upgradeTier // ++ LoyaltyAction.adjustPoints(100)

      val exampleRule = LoyaltyRule(exampleCondition, exampleAction)

      val exampleRuleSet = RuleSet.empty.addRule(exampleRule)

      val engine = RuleEngine.fromRuleSet(exampleRuleSet)

      def updateLoyaltyProgram(booking: FlightBooking, program: LoyaltyProgram): LoyaltyProgram =
        engine.update(booking -> program).getOrElse(program)

      // val identityPatch = identity[LoyaltyProgram](_)
      // engine.update(booking).getOrElse

    }

  }

}
