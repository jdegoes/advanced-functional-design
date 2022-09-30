/**
 * After much work, you have a rule engine you can be proud of: generic,
 * compositional, type-safe, declarative, persistable, efficient, and even
 * suitable for streaming applications.
 *
 * Now, it's time you put that rule engine to use. In this section, you will use
 * your rule engine to build out an application of your choosing, possibly
 * chosen from among the motivating examples.
 */
package net.degoes.afd.ruleengine

import zio._

/**
 * Use your rule engine to build out an application of your choosing. You should
 * have a main function that executes a sample rule set on some sample data, and
 * prints out or executes the actions produced.
 */
object graduation extends App {
  import declarative._
  object loyalty {
    object Flight {
      val id     = FactDefinition.string("id")
      val number = FactDefinition.string("number")

      val factsType =
        FactsType.empty.add(id).add(number)
    }

    object Customer {
      val id    = FactDefinition.string("id")
      val name  = FactDefinition.string("name")
      val email = FactDefinition.string("email")
      val phone = FactDefinition.string("phone")

      val factsType =
        FactsType.empty.add(id).add(name).add(email).add(phone)
    }

    object FlightBooking {
      val id       = FactDefinition.string("id")
      val customer = FactDefinition.facts("customer", Customer.factsType)
      val flight   = FactDefinition.facts("flight", Flight.factsType)
      val price    = FactDefinition.double("price")
      val status   = FactDefinition.string("status")

      val factsType =
        FactsType.empty.add(id).add(customer).add(flight).add(price).add(status)
    }

    object FlightBookingStatus {
      val Confirmed = FactDefinition.string("Confirmed")
      val Cancelled = FactDefinition.string("Cancelled")
      val Pending   = FactDefinition.string("Pending")
    }

    object LoyaltyAction {
      val actionType = FactDefinition.string("action_type")
      val points     = FactDefinition.int("points")
      val customer   = FactDefinition.string("customer")
    }

    object ActionType {
      val AddPoints     = Expr("add_points")
      val UpgradeTier   = Expr("upgrade_tier")
      val DowngradeTier = Expr("downgrade_tier")
    }
  }

  object example1 {
    object rules {

      import loyalty._

      val statusCondition: Condition[Facts[("status", String)]] =
        Condition(FlightBooking.status.get === "Confirmed")

      val priceCondition: Condition[Facts[("price", Double)]] =
        Condition(FlightBooking.price.get > 1000.0)

      val both = statusCondition && priceCondition

      val rule =
        Rule(
          both,
          Action.fromExpr((LoyaltyAction.actionType     := ActionType.AddPoints) ++ (LoyaltyAction.points := 100))
            ++ Action.fromExpr(LoyaltyAction.actionType := ActionType.UpgradeTier)
        )

      val ruleSet = RuleSet(rule)

    }

    import net.degoes.afd.examples.loyalty._

    val customer       = Customer("id-1", "John Doe", "me@my.self", "+49123-456-7890")
    val flight         = Flight("id-1-flight", "LH-123")
    val booking        = FlightBooking("id-1-booking", customer, flight, 1500.0, FlightBookingStatus.Confirmed)
    val loyaltyProgram = LoyaltyProgram(customer.id, 0, LoyaltyTier.Bronze)

    val engine = RuleEngine.fromRuleSet(rules.ruleSet)

  }

  object convert {
    import net.degoes.afd.examples.loyalty._
    def bookingToFacts(booking: FlightBooking) =
      Facts.empty
        .add(loyalty.FlightBooking.id, booking.id)
        .add(loyalty.FlightBooking.customer, customerToFacts(booking.customer))
        .add(loyalty.FlightBooking.flight, flightToFacts(booking.flight))
        .add(loyalty.FlightBooking.price, booking.price)
        .add(loyalty.FlightBooking.status, booking.status.productPrefix)

    def customerToFacts(customer: Customer) =
      Facts.empty
        .add(loyalty.Customer.id, customer.id)
        .add(loyalty.Customer.name, customer.name)
        .add(loyalty.Customer.email, customer.email)
        .add(loyalty.Customer.phone, customer.phone)

    def flightToFacts(flight: Flight) =
      Facts.empty
        .add(loyalty.Flight.id, flight.id)
        .add(loyalty.Flight.number, flight.number)

    def applyActionsToProgram(
      program: LoyaltyProgram,
      actions: List[Facts[("action_type", String) & ("points", Int)]]
    ): LoyaltyProgram =
      actions.foldLeft(program) { (program, action) =>
        val actionType = action.get(loyalty.LoyaltyAction.actionType)
        actionType match {
          case "add_points" =>
            val points = action.get(loyalty.LoyaltyAction.points)
            program.copy(points = program.points + points)
          case "upgrade_tier"   => program.copy(tier = LoyaltyTier.next(program.tier))
          case "downgrade_tier" => program.copy(tier = LoyaltyTier.previous(program.tier))
        }
      }
  }

  private val engineOutput: Option[List[Facts[("action_type", String) & ("points", Int)]]] =
    example1.engine.update(convert.bookingToFacts(example1.booking))

  print("Calculated actions: ")
  println(engineOutput)
  println("")

  print("Original Loyalty Program: ")
  println(example1.loyaltyProgram)

  engineOutput.foreach { actions =>
    print("Updated loyalty program: ")
    println(convert.applyActionsToProgram(example1.loyaltyProgram, actions))
  }

}
