package net.degoes.afd.examples.events

import zio._
import net.degoes.afd.examples.events.Event.AccountRegistered
import net.degoes.afd.examples.events.Event.AppInstalled
import net.degoes.afd.examples.events.Event.AdWatched

final case class AdId(value: String)

final case class UserId(value: String)

final case class UserProfile(adsWatched: Int, registered: Boolean)

sealed trait Event
object Event {
  final case class AccountRegistered(user: UserId)     extends Event
  final case class AppInstalled(user: UserId)          extends Event
  final case class AdWatched(adId: AdId, user: UserId) extends Event
}

trait Email {
  def sendEmail(userId: UserId, subject: String, body: String): Task[Unit]
}

trait UserProfileRepo {
  def getUserProfile(userId: UserId): Task[UserProfile]

  def updateUserProfile(userId: UserId, profile: UserProfile): Task[Unit]
}

object processor {
  def process(event: Event, email: Email, repo: UserProfileRepo): Task[Unit] =
    event match {
      case AccountRegistered(user) =>
        for {
          profile <- repo.getUserProfile(user)
          _       <- repo.updateUserProfile(user, profile.copy(registered = true))
          _       <- email.sendEmail(user, "Welcome to our app!", "Thank you for registering!")
        } yield ()

      case AppInstalled(user) =>
        for {
          profile <- repo.getUserProfile(user)
          _       <- repo.updateUserProfile(user, profile.copy(registered = true))
          _       <- email.sendEmail(user, "Welcome to our app!", "Thank you for installing our app!")
        } yield ()

      case AdWatched(adId, user) =>
        for {
          profile   <- repo.getUserProfile(user)
          newProfile = profile.copy(adsWatched = profile.adsWatched + 1)
          _         <- repo.updateUserProfile(user, newProfile)
          _ <- if (newProfile.adsWatched > 100)
                 email.sendEmail(
                   user,
                   "Here's a 20% off coupone to upgrade to Premium!",
                   "We noticed you enjoy using our app. That's why we've decided to give you a 20% off coupon!"
                 )
               else ZIO.unit
        } yield ()
    }
}
