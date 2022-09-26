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
  final case class LoyaltyEngine(update: (FlightBooking, LoyaltyProgram) => LoyaltyProgram)

  object LoyaltyEngine {
    def fromRuleSet(ruleSet: LoyaltyRuleSet): LoyaltyEngine =
      LoyaltyEngine((booking, program) =>
        ruleSet.rules.find(_.condition.eval(booking)) match {
          case Some(rule) => rule.action.update(program)
          case None       => program
        }
      )
  }

  final case class LoyaltyRuleSet(rules: Vector[LoyaltyRule]) {
    self =>
    def +(that: LoyaltyRule): LoyaltyRuleSet = LoyaltyRuleSet(self.rules :+ that)

    def ++(that: LoyaltyRuleSet): LoyaltyRuleSet = LoyaltyRuleSet(self.rules ++ that.rules)
  }

  object LoyaltyRuleSet {
    def empty: LoyaltyRuleSet = LoyaltyRuleSet(Vector.empty)
  }

  final case class LoyaltyRule(condition: LoyaltyCondition, action: LoyaltyAction) {
    self =>
    def ++(that: LoyaltyRule): LoyaltyRule =
      LoyaltyRule(self.condition && that.condition, self.action ++ that.action)
  }

  final case class LoyaltyCondition(eval: FlightBooking => Boolean) {
    self =>
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

  final case class LoyaltyAction(update: LoyaltyProgram => LoyaltyProgram) {
    def ++(that: LoyaltyAction): LoyaltyAction =
      LoyaltyAction(this.update.andThen(that.update))
  }

  object LoyaltyAction {
    def adjustPoints(value: Int): LoyaltyAction =
      LoyaltyAction(program => program.copy(points = program.points + value))

    val downgradeTier: LoyaltyAction =
      LoyaltyAction(program =>
        program.copy(tier = program.tier match {
          case LoyaltyTier.Bronze => LoyaltyTier.Bronze
          case LoyaltyTier.Silver => LoyaltyTier.Bronze
          case LoyaltyTier.Gold   => LoyaltyTier.Silver
        })
      )

    val none: LoyaltyAction = LoyaltyAction(identity)

    val upgradeTier: LoyaltyAction =
      LoyaltyAction(program =>
        program.copy(tier = program.tier match {
          case LoyaltyTier.Bronze => LoyaltyTier.Silver
          case LoyaltyTier.Silver => LoyaltyTier.Gold
          case LoyaltyTier.Gold   => LoyaltyTier.Gold
        })
      )
  }

  object example {
    val exampleCondition =
      LoyaltyCondition.status(_ == FlightBookingStatus.Confirmed) &&
        LoyaltyCondition.price(_ > 1000)

    val exampleAction  = LoyaltyAction.upgradeTier ++ LoyaltyAction.adjustPoints(100)
    val exampleRule    = LoyaltyRule(exampleCondition, exampleAction)
    val exampleRuleSet = LoyaltyRuleSet.empty + exampleRule

    val engine = LoyaltyEngine.fromRuleSet(exampleRuleSet)

    def updateLoyaltyProgram(booking: FlightBooking, program: LoyaltyProgram): LoyaltyProgram =
      engine.update(booking, program)
  }

  //object LoyaltyAction {
  //
  //  private[ruleengine] case object Unchanged extends LoyaltyAction
  //  private[ruleengine] case object DowngradeTier extends LoyaltyAction
  //  private[ruleengine] case object UpgradeTier   extends LoyaltyAction
  //  private[ruleengine] case class AdjustPoints(value: Int) extends LoyaltyAction
  //  private[ruleengine] case class Both(left: LoyaltyAction, right: LoyaltyAction) extends LoyaltyAction
  //}

}
