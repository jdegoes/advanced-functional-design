/**
 * The simplest of all functional domains uses untyped models that are highly
 * specialized, encoded using the executable encoding.
 *
 * In this section, you will explore the domain of rule engines by making the
 * simplest possible, domain-specialized, untyped functional model, equipped
 * with constructors and operators.
 */
package net.degoes.afd.ruleengine

import net.degoes.afd.examples.loyalty.LoyaltyProgram
import net.degoes.afd.examples.loyalty.LoyaltyTier
import net.degoes.afd.examples.loyalty.FlightBooking
import net.degoes.afd.examples.loyalty.FlightBookingStatus

object example {
  import basics._
  val exampleAction = LoyaltyAction.upgradeTier ++ LoyaltyAction.adjustPoints(100)

  val exampleCondition = 
    LoyaltyCondition.status(_ == FlightBookingStatus.Confirmed) &&
    LoyaltyCondition(_.price > 1000)

  val exampleRule = LoyaltyRule(exampleCondition, exampleAction)

  val exampleRuleSet = LoyaltyRuleSet(Vector(exampleRule))

  val exampleEngine = LoyaltyEngine.fromRuleSet(exampleRuleSet)
}

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


  // Responsible for computation in response to a that results in Loyalty program changes
  final case class LoyaltyEngine(update: (FlightBooking, LoyaltyProgram) => LoyaltyProgram)
  object LoyaltyEngine {
    def fromRuleSet(ruleSet: LoyaltyRuleSet): LoyaltyEngine =
      LoyaltyEngine( (booking, program) =>
        ruleSet.rules.find(_.condition.eval(booking)).map{ rule => 
          rule.action.update(program)
        }.getOrElse(program)
      )
  }

  final case class LoyaltyRuleSet(rules: Vector[LoyaltyRule]) { self =>
    def + (rule: LoyaltyRule): LoyaltyRuleSet =
      LoyaltyRuleSet(self.rules :+ rule)

    def ++ (that: LoyaltyRuleSet): LoyaltyRuleSet =
      LoyaltyRuleSet(self.rules ++ that.rules)
  }
  object LoyaltyRuleSet {
    val empty: LoyaltyRuleSet = LoyaltyRuleSet(Vector.empty)
  }

  final case class LoyaltyRule(condition: LoyaltyCondition, action: LoyaltyAction)

  final case class LoyaltyCondition(eval: FlightBooking => Boolean) { self =>
    def && (that: LoyaltyCondition): LoyaltyCondition = 
      LoyaltyCondition(booking => self.eval(booking) && that.eval(booking))

    def || (that: LoyaltyCondition): LoyaltyCondition = 
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

  // alternate executable encoding
  final case class LoyaltyAction(update: LoyaltyProgram => LoyaltyProgram) {
    def ++ (that: LoyaltyAction): LoyaltyAction = 
      LoyaltyAction(update andThen that.update)
  }

  // Downside, this as written could update the customer id which is not desireable
  // we could make it less powerful by changing update: LoyaltyProgram => LoyaltyProgram
  object LoyaltyAction {
    def adjustPoints(value: Int): LoyaltyAction = 
      LoyaltyAction(program => program.copy(points = program.points + value))

    val downgradeTier: LoyaltyAction = 
      LoyaltyAction(program => program.copy(tier = program.tier match {
        case LoyaltyTier.Gold => LoyaltyTier.Silver
        case LoyaltyTier.Silver => LoyaltyTier.Bronze
        case LoyaltyTier.Bronze => LoyaltyTier.Bronze
      }))

    val none: LoyaltyAction = LoyaltyAction(identity[LoyaltyProgram])

    val upgradeTier: LoyaltyAction = 
      LoyaltyAction(program => program.copy(tier = program.tier match {
        case LoyaltyTier.Bronze => LoyaltyTier.Silver
        case LoyaltyTier.Silver => LoyaltyTier.Gold
        case LoyaltyTier.Gold => LoyaltyTier.Gold
      }))
  }

}
