package net.degoes.afd.ruleengine



  // declarative
  // sealed trait LoyaltyAction2 { self => 
  //   def ++ (that: LoyaltyAction2): LoyaltyAction = LoyaltyAction.Both(self, that)

    // final def update(program: LoyaltyProgram): LoyaltyProgram = self match {
    //   case LoyaltyAction.Unchanged => program
    //   case LoyaltyAction.UpgradeTier =>
    //     program.copy(tier = program.tier match {
    //       case LoyaltyTier.Bronze => LoyaltyTier.Silver
    //       case LoyaltyTier.Silver => LoyaltyTier.Gold
    //       case LoyaltyTier.Gold   => LoyaltyTier.Gold
    //     })
    //   case LoyaltyAction.DowngradeTier =>
    //     program.copy(tier = program.tier match {
    //       case Bronze => Bronze
    //       case Gold   => Silver
    //       case Silver => Bronze
    //     })
    //   case LoyaltyAction.AdjustPoints(points) => ???
    // }
  // }

  // object LoyaltyAction {
  //   val none: LoyaltyAction = Unchanged
  //   val upgradeTier: LoyaltyAction = UpgradeTier
  //   val downgradeTier: LoyaltyAction = DowngradeTier
  //   def adjustPoints(value: Int): LoyaltyAction = AdjustPoints(value)

  //   private[basics] case object Unchanged extends LoyaltyAction
  //   private[basics] case object UpgradeTier extends LoyaltyAction
  //   private[basics] case object DowngradeTier extends LoyaltyAction
  //   private[basics] final case class AdjustPoints(points: Int) extends LoyaltyAction
  //   private[basics] final case class Both(left: LoyaltyAction, right: LoyaltyAction) extends LoyaltyAction
  // }


  // executable as a trait
  // trait LoyaltyAction3 {
  //   def update: LoyaltyProgram => LoyaltyProgram
  // }
