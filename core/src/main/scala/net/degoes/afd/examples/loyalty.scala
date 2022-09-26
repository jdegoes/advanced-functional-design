package net.degoes.afd.examples.loyalty

final case class Customer(id: String, name: String, email: String, phone: String)

final case class Flight(id: String, number: String)

final case class FlightBooking(
  id: String,
  customer: Customer,
  flight: Flight,
  price: Double,
  status: FlightBookingStatus
)

sealed trait FlightBookingStatus
object FlightBookingStatus {
  case object Pending   extends FlightBookingStatus
  case object Confirmed extends FlightBookingStatus
  case object Cancelled extends FlightBookingStatus
}

final case class LoyaltyProgram(customerId: String, points: Int, tier: LoyaltyTier)

sealed trait LoyaltyTier
object LoyaltyTier {
  case object Bronze extends LoyaltyTier
  case object Silver extends LoyaltyTier
  case object Gold   extends LoyaltyTier
}

object rewards {

  // Note: update loyalty program based on flight booking status
  def updateLoyaltyProgram(booking: FlightBooking, program: LoyaltyProgram): LoyaltyProgram =
    booking match {
      case FlightBooking(_, Customer(_, _, _, _), _, price, FlightBookingStatus.Confirmed) =>
        if (price > 1000) program.copy(tier = LoyaltyTier.Gold, points = program.points + 100)
        else program

      case _ => program
    }
}
