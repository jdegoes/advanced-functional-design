package net.degoes.afd.ruleengine

import net.degoes.afd.examples.loyalty.LoyaltyProgram
import net.degoes.afd.examples.loyalty.LoyaltyTier

//   declarative
object test {
  sealed trait LoyaltyAction { self =>
    def ++(that: LoyaltyAction): LoyaltyAction = LoyaltyAction.Both(self, that)

    final def update(program: LoyaltyProgram): LoyaltyProgram = self match {
      case LoyaltyAction.Unchanged => program
      case LoyaltyAction.UpgradeTier =>
        program.copy(tier = program.tier match {
          case LoyaltyTier.Bronze => LoyaltyTier.Silver
          case LoyaltyTier.Silver => LoyaltyTier.Gold
          case LoyaltyTier.Gold   => LoyaltyTier.Gold
        })
      case LoyaltyAction.DowngradeTier =>
        program.copy(tier = program.tier match {
          case LoyaltyTier.Bronze => LoyaltyTier.Bronze
          case LoyaltyTier.Gold   => LoyaltyTier.Silver
          case LoyaltyTier.Silver => LoyaltyTier.Bronze
        })
      case LoyaltyAction.AdjustPoints(points) => ???
    }
  }

  object LoyaltyAction {
    val none: LoyaltyAction                     = Unchanged
    val upgradeTier: LoyaltyAction              = UpgradeTier
    val downgradeTier: LoyaltyAction            = DowngradeTier
    def adjustPoints(value: Int): LoyaltyAction = AdjustPoints(value)

    private[ruleengine] case object Unchanged                                            extends LoyaltyAction
    private[ruleengine] case object UpgradeTier                                          extends LoyaltyAction
    private[ruleengine] case object DowngradeTier                                        extends LoyaltyAction
    private[ruleengine] final case class AdjustPoints(points: Int)                       extends LoyaltyAction
    private[ruleengine] final case class Both(left: LoyaltyAction, right: LoyaltyAction) extends LoyaltyAction
  }

}

// executable as a trait
// trait LoyaltyAction3 {
//   def update: LoyaltyProgram => LoyaltyProgram
// }
