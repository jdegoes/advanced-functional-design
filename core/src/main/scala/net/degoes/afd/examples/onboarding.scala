package net.degoes.afd.examples.onboarding

import java.time.Instant

sealed trait Subscription
object Subscription {
  case object Freemium extends Subscription
  case object Premium  extends Subscription
}

final case class ScreenId(value: String)
object ScreenId {
  val Home     = ScreenId("home")
  val Profile  = ScreenId("profile")
  val Settings = ScreenId("settings")
  val Help     = ScreenId("help")
  val About    = ScreenId("about")
  val Export   = ScreenId("export")
}

final case class Tip(description: String, screenToOpen: Option[ScreenId], highlightLocation: Option[(Int, Int)])

final case class UserProfile(accountCreation: Instant, subscription: Subscription, visitedScreens: Set[ScreenId]) {
  def isNewAccount(): Boolean = {
    val OneWeek    = 60 * 60 * 24 * 7
    val OneWeekAgo = Instant.now().minusSeconds(OneWeek)
    accountCreation.isAfter(OneWeekAgo)
  }
}

object tips {
  def generateTip(profile: UserProfile): Option[Tip] =
    profile.subscription match {
      case Subscription.Freemium =>
        if (!profile.visitedScreens.contains(ScreenId.Export) && profile.isNewAccount()) {
          Some(
            Tip(
              "Did you know that you can export your plan to PDF by accessing the File menu?",
              Some(ScreenId.Export),
              Some((50, 100))
            )
          )
        } else None

      case _ => None
    }
}
