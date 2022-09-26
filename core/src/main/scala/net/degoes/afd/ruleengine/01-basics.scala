/**
 * The simplest of all functional domains uses untyped models that are highly
 * specialized, encoded using the executable encoding.
 *
 * In this section, you will explore the domain of rule engines by making the
 * simplest possible, domain-specialized, untyped functional model, equipped
 * with constructors and operators.
 */
package net.degoes.afd.ruleengine

import net.degoes.afd.examples.loyalty._

/**
 * Create a functional domain to express how customer actions translate into
 * loyalty points and tier changes.
 *
 * For simplicity and ease-of-construction, use the executable encoding and an
 * untyped functional model.
 *
 * Attempt to make each subdomain as compositional as possible, with
 * constructors, binary operators, and unary operators.
 */
object basics {

  // recap so keep it simple..

  // need to update based on flight booking
  // responsibel for computaion in response to a booking that result sin loyalti program changes
  final case class LoyaltyEngine(
    update: (FlightBooking, LoyaltyProgram) => LoyaltyProgram
  )
  object LoyaltyEngine {
    def fromRuleSet(ruleSet: LoyaltyRuleSet): LoyaltyEngine =
      LoyaltyEngine((booking, program) =>
        ruleSet.rules.find(_.condition.eval(booking)) match {
          case Some(rule) => rule.action.update(program)
          case None       => program
        }
      )
    // note: we could have foldLeft here but thats i correct because we have to break on first match

  }

  // a bunch of rules
  // not a set because order matters on the match reward booking match {
  // note: we have it in a seperate case class to give it its own rules
  final case class LoyaltyRuleSet(rules: Vector[LoyaltyRule]) { self =>
    def ++(that: LoyaltyRuleSet): LoyaltyRuleSet = LoyaltyRuleSet(self.rules ++ that.rules)
    def +(that: LoyaltyRule): LoyaltyRuleSet     = LoyaltyRuleSet(self.rules :+ that)
  }
  // constructors added to the companion object
  object LoyaltyRuleSet {
    val empty: LoyaltyRuleSet                  = LoyaltyRuleSet(Vector.empty)
    def fromFile(file: String): LoyaltyRuleSet = ???
  }

  // composition of other domain
  final case class LoyaltyRule(condition: LoyaltyCondition, action: LoyaltyAction) { self =>
    def &&(that: LoyaltyRule): LoyaltyRule = LoyaltyRule(self.condition && that.condition, self.action ++ that.action)
    // def ||(that: LoyaltyRule): LoyaltyRule = LoyaltyRule.Or(self, that)
  }

  final case class LoyaltyRule2(f: LoyaltyCondition => LoyaltyAction) { self =>
    def &&(that: LoyaltyRule2): LoyaltyRule2 = LoyaltyRule2(c => self.f(c) ++ that.f(c))
  }

  // if (price > 1000)
  // not functional domain yet because no operatores and or constructors
  // final case class LoyaltyCondition(eval: (FlightBooking, LoyaltyProgram) => Boolean)
  final case class LoyaltyCondition(eval: FlightBooking => Boolean) { self =>
    def &&(that: LoyaltyCondition): LoyaltyCondition =
      LoyaltyCondition(booking => self.eval(booking) && that.eval(booking))

    def ||(that: LoyaltyCondition): LoyaltyCondition =
      LoyaltyCondition(booking => self.eval(booking) || that.eval(booking))

    def unary_! : LoyaltyCondition =
      LoyaltyCondition(booking => !self.eval(booking))
  }
  object LoyaltyCondition {
    val always: LoyaltyCondition = constant(true)

    def status(p: FlightBookingStatus => Boolean): LoyaltyCondition =
      LoyaltyCondition(booking => p(booking.status))

    def price(p: Double => Boolean): LoyaltyCondition =
      LoyaltyCondition(booking => p(booking.price))

    val never: LoyaltyCondition = constant(false)

    def constant(value: Boolean): LoyaltyCondition =
      LoyaltyCondition(_ => value)
  }

  // similar to loyalty rewards
  LoyaltyCondition.status(_ == FlightBookingStatus.Confirmed) &&
    LoyaltyCondition.price(_ > 1000)

//  LoyaltyAction.upgradeTier  ++ TODO

  // alternative executable encoding instead use trait   or case class (not sealed treat)
  // con: we can swicth, set customer id not desirable
  // => (points, tier)
  final case class LoyaltyAction(update: LoyaltyProgram => LoyaltyProgram) { self =>
    def ++(that: LoyaltyAction): LoyaltyAction =
      LoyaltyAction(self.update.andThen(that.update))
  }
  object LoyaltyAction {

    def adjustPoints(value: Int): LoyaltyAction =
      LoyaltyAction(program => program.copy(points = program.points + value))

    val downgradeTier: LoyaltyAction = LoyaltyAction(program =>
      program.copy(tier = program.tier match {
        case LoyaltyTier.Bronze => LoyaltyTier.Bronze
        case LoyaltyTier.Silver => LoyaltyTier.Bronze
        case LoyaltyTier.Gold   => LoyaltyTier.Silver
      })
    )

    val none: LoyaltyAction = LoyaltyAction(identity[LoyaltyProgram])

    val upgradeTier: LoyaltyAction = LoyaltyAction(program =>
      program.copy(tier = program.tier match {
        case LoyaltyTier.Bronze => LoyaltyTier.Silver
        case LoyaltyTier.Silver => LoyaltyTier.Gold
        case LoyaltyTier.Gold   => LoyaltyTier.Gold
      })
    )

  }

  // learning go back and see if use cases can be solved / expressed
  object example {
    val exampleCondition = LoyaltyCondition.status(_ == FlightBookingStatus.Confirmed) &&
      LoyaltyCondition.price(_ > 1000)

    val exampleAction = LoyaltyAction.upgradeTier ++ LoyaltyAction.adjustPoints(100)

    val exampleRule = LoyaltyRule(exampleCondition, exampleAction)

    val exampleRuleSet = LoyaltyRuleSet(Vector(exampleRule))

    val engine = LoyaltyEngine.fromRuleSet(exampleRuleSet)

    def updateLoyaltyProgram(booking: FlightBooking, program: LoyaltyProgram): LoyaltyProgram =
      engine.update(booking, program)

  }

}
