/**
 * The next step in the evolution of a rule engine is to make it generic, across
 * all domains (not just an airline loyalty program).
 *
 * In this section, you will explore different ways to encode a generic rule
 * engine, and discover more powerful functional domains in the process.
 */
package net.degoes.afd.ruleengine

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
  final case class RuleEngine[-In, +Out](update: In => Option[List[Out]]) { self =>
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

    def fromRuleSet[In, Out](ruleSet: RuleSet[In, Out]): RuleEngine[In, Out] =
      RuleEngine(ruleSet.update)
  }

  final case class RuleSet[-In, +Out](rules: Vector[Rule[In, Out]]) { self =>
    def + [In1 <: In, Out1 >: Out](that: Rule[In1, Out1]): RuleSet[In1, Out1] =
      RuleSet(self.rules :+ that)

    def ++ [In1 <: In, Out1 >: Out](that: RuleSet[In1, Out1]): RuleSet[In1, Out1] =
      RuleSet(self.rules ++ that.rules)

    def addRule[In1 <: In, Out1 >: Out](that: Rule[In1, Out1]): RuleSet[In1, Out1] = self + that

    def update(in: In): Option[List[Out]] =
      self.rules.find(_.condition.eval(in)).map { rule =>
        rule.action.update(in)
      }
  }
  object RuleSet {
    def apply[In, Out](rule1: Rule[In, Out], rules: Rule[In, Out]*): RuleSet[In, Out]
    = RuleSet(rule1 +: rules.toVector)

    val empty: RuleSet[Any, Nothing] = RuleSet(Vector.empty)
  }

  final case class Rule[-In, +Out](condition: Condition[In], action: Action[In, Out])

  final case class Condition[-In](eval: In => Boolean) { self =>
    def &&[In1 <: In](that: Condition[In1]): Condition[In1] =
      Condition(in => self.eval(in) && that.eval(in))

    def ||[In1 <: In](that: Condition[In1]): Condition[In1] =
      Condition(in => self.eval(in) || that.eval(in))

    def contramap[In2](f: In2 => In): Condition[In2] = Condition(f.andThen(self.eval))

    def unary_! : Condition[In] = Condition(in => !self.eval(in))
  }
  object Condition {
    val always: Condition[Any] = constant(true)

    val never: Condition[Any] = constant(false)

    def constant[In](value: Boolean): Condition[In] = Condition(_ => value)

    def fromFunction[In](f: In => Boolean): Condition[In] = Condition(f)
  }

  final case class Action[-In, +Out](update: In => List[Out]) { self =>
    def ++[In1 <: In, Out1 >: Out](that: Action[In1, Out1]): Action[In1, Out1] =
      Action(in => self.update(in) ++ that.update(in))

    def >>>[Out2](that: Action[Out, Out2]): Action[In, Out2] =
      Action(in => update(in).flatMap(that.update))

    def contramap[In2](f: In2 => In): Action[In2, Out] =
      Action.fromFunction(f) >>> self

    def flatMap[In1 <: In, Out2](f: Out => Action[In1, Out2]): Action[In1, Out2] =
      Action(in => self.update(in).flatMap(out => f(out).update(in)))

    def map[Out2](f: Out => Out2): Action[In, Out2] =
      self >>> Action.fromFunction(f)

    def zip[In1 <: In, Out2](that: Action[In1, Out2]): Action[In1, (Out, Out2)] =
      self.flatMap(out => that.map(out2 => (out, out2)))
  }
  object Action {
    def constant[Out](out: Out): Action[Any, Out] = fromFunction(_ => out)

    def fromFunction[In, Out](f: In => Out): Action[In, Out] = Action(in => List(f(in)))
  }

  /**
   * As proof of genericity, implement the same API for the loyalty engine that
   * you developed in the previous section, but implement it atop the generic
   * rule engine you created in this section.
   */
  object loyalty {
    import net.degoes.afd.examples.loyalty.LoyaltyTier._
    import net.degoes.afd.examples.loyalty._

    type Patch[A] = A => A

    type LoyaltyEngine = RuleEngine[FlightBooking, Patch[LoyaltyProgram]]

    type LoyaltyRuleSet = RuleSet[FlightBooking, Patch[LoyaltyProgram]]

    type LoyaltyRule = Rule[FlightBooking, Patch[LoyaltyProgram]]
    object LoyaltyRule {
      def apply(condition: LoyaltyCondition, action: LoyaltyAction): LoyaltyRule =
        Rule(condition, action)
    }

    type LoyaltyCondition = Condition[FlightBooking]
    object LoyaltyCondition {
      def status(f: FlightBookingStatus => Boolean): LoyaltyCondition =
        Condition(booking => f(booking.status))

      def price(f: Double => Boolean): LoyaltyCondition =
        Condition(booking => f(booking.price))
    }

    type LoyaltyAction = Action[Any, Patch[LoyaltyProgram]]
    object LoyaltyAction {
      def apply(patch: LoyaltyProgram => LoyaltyProgram): LoyaltyAction = Action.constant(patch)

      def adjustPoints(value: Int): LoyaltyAction =
        LoyaltyAction(program => program.copy(points = program.points + value))

      val downgradeTier: LoyaltyAction =
        LoyaltyAction(program => program.copy(tier = program.tier match {
          case Bronze => Bronze
          case Silver => Bronze
          case Gold => Silver
        }))

      val none: LoyaltyAction = LoyaltyAction(identity[LoyaltyProgram])

      val upgradeTier: LoyaltyAction =
        LoyaltyAction(program => program.copy(tier = program.tier match {
          case Bronze => Silver
          case Silver => Gold
          case Gold => Gold
        }))
    }

    object example {
      val exampleCondition =
        LoyaltyCondition.status(_ == FlightBookingStatus.Confirmed) &&
          LoyaltyCondition.price(_ > 1000)

      val exampleAction  = LoyaltyAction.upgradeTier ++ LoyaltyAction.adjustPoints(100)
      val exampleRule    = LoyaltyRule(exampleCondition, exampleAction)
      val exampleRuleSet = RuleSet.empty.addRule(exampleRule)

      val engine = RuleEngine.fromRuleSet(exampleRuleSet)

      def updateLoyaltyProgram(booking: FlightBooking, program: LoyaltyProgram): LoyaltyProgram = {
        val empty = identity[LoyaltyProgram](_)
        val patch = engine.updateWith(booking)(empty, _ andThen _)

        patch(program)
      }
    }
  }
}