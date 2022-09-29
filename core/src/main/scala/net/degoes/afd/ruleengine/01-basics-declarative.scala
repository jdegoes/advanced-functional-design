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
import net.degoes.afd.examples.loyalty._
import net.degoes.afd.examples.loyalty.LoyaltyTier.Bronze
import net.degoes.afd.examples.loyalty.LoyaltyTier.Gold
import net.degoes.afd.examples.loyalty.LoyaltyTier.Silver
import net.degoes.afd.ruleengine.basicsdeclarative.LoyaltyAction.AdjustPoints
import net.degoes.afd.ruleengine.basicsdeclarative.LoyaltyAction.Unchanged
import net.degoes.afd.ruleengine.basicsdeclarative.LoyaltyAction.UpgradeTier
import net.degoes.afd.ruleengine.basicsdeclarative.LoyaltyAction.DowngradeTier
import net.degoes.afd.ruleengine.basicsdeclarative.LoyaltyAction.Both

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
object basicsdeclarative {

  // TODO: use 01-basics to implement the rest in declartive fashion

  // recap so keep it simple..

  // need to update based on flight booking
  // responsibel for computaion in response to a booking that result sin loyalti program changes
  trait LoyaltyEngine

  // a bunch of rules
  trait LoyaltyRuleSet

  trait LoyaltyRule

  sealed trait LoyaltyCondition { self =>

    // TODO: how do I get booking and do the evaluation ?
    def &&(that: LoyaltyCondition): LoyaltyCondition = ???
    def ||(that: LoyaltyCondition): LoyaltyCondition = ???
    def unary_! : LoyaltyCondition                   = ???
  }
  object LoyaltyCondition {
    // final case class And(left: LoyaltyCondition, right: LoyaltyCondition) extends LoyaltyCondition
  }

  sealed trait LoyaltyAction { self =>
    def ++(that: LoyaltyAction): LoyaltyAction = LoyaltyAction.Both(self, that)

    final def update(program: LoyaltyProgram): LoyaltyProgram = self match {
      case AdjustPoints(points)    => program.copy(points = program.points + points)
      case LoyaltyAction.Unchanged => program
      case LoyaltyAction.UpgradeTier =>
        program.copy(tier = program.tier match {
          case LoyaltyTier.Bronze => LoyaltyTier.Silver
          case LoyaltyTier.Silver => LoyaltyTier.Gold
          case LoyaltyTier.Gold   => LoyaltyTier.Gold
        })
      case LoyaltyAction.DowngradeTier =>
        program.copy(tier = program.tier match {
          case Bronze => Bronze
          case Gold   => Silver
          case Silver => Bronze
        })
      case Both(left, right) =>
        // associtivity ?
        // left.update(right.update(program))
        // or
        right.update(left.update(program))
    }
  }
  object LoyaltyAction {

    def adjustPoints(value: Int): LoyaltyAction = AdjustPoints(value)
    val downgradeTier: LoyaltyAction            = DowngradeTier
    val none: LoyaltyAction                     = Unchanged
    val upgradeTier: LoyaltyAction              = UpgradeTier

    final case class AdjustPoints(points: Int)                       extends LoyaltyAction
    case object Unchanged                                            extends LoyaltyAction
    case object UpgradeTier                                          extends LoyaltyAction
    case object DowngradeTier                                        extends LoyaltyAction
    final case class Both(left: LoyaltyAction, right: LoyaltyAction) extends LoyaltyAction
  }

  object example {
    // val exampleCondition = LoyaltyCondition.status(_ == FlightBookingStatus.Confirmed) &&
    //   LoyaltyCondition.price(_ > 1000)

    // val exampleAction = LoyaltyAction.upgradeTier ++ LoyaltyAction.adjustPoints(100)

    // val exampleRule = LoyaltyRule(exampleCondition, exampleAction)

    // val exampleRuleSet = LoyaltyRuleSet(Vector(exampleRule))

    // val engine = LoyaltyEngine.fromRuleSet(exampleRuleSet)

    // def updateLoyaltyProgram(booking: FlightBooking, program: LoyaltyProgram): LoyaltyProgram =
    //   engine.update(booking, program)

  }

}
