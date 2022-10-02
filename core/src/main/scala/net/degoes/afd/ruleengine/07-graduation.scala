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
import scala.annotation._
import scala.language.implicitConversions

/**
 * Use your rule engine to build out an application of your choosing. You should
 * have a main function that executes a sample rule set on some sample data, and
 * prints out or executes the actions produced.
 */
object graduation2 {

  import graduation._
  import net.degoes.afd.examples.loyalty._
  import net.degoes.afd.examples.loyalty.LoyaltyTier._

  object loyalty_model {

    object Flights {
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
      val customer = FactDefinition.facts("customer", Customer.factsType) // FIXME: Support nested data
      val flight = FactDefinition.facts(
        "fligh",
        Flights.factsType
      ) // FactDefinition.string("flight")   // FIXME: Suppor nested data
      val price  = FactDefinition.double("price")
      val status = FactDefinition.string("status")

      val factsType =
        FactsType.empty.add(id).add(customer).add(flight).add(price).add(status)
    }

    object FlightBookingStatus {
      val Confirmed = Expr("Confirmed")
      val Cancelled = Expr("Cancelled")
      val Pending   = Expr("Pending")
    }

    object LoyaltyProgram {
      val tier   = FactDefinition.string("tier")
      val points = FactDefinition.int("points")
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

  object ActionExecutor {

    import loyalty_model._

    def update(program: LoyaltyProgram, action: Facts[_]): LoyaltyProgram =
      Unsafe.unsafe { implicit u =>
        action.unsafe.get(LoyaltyAction.actionType) match {
          case ActionType.DowngradeTier => program.copy(tier = LoyaltyTier.Bronze)
          case ActionType.UpgradeTier   => program.copy(tier = LoyaltyTier.Gold)
          case ActionType.AddPoints =>
            action.unsafe.get(LoyaltyAction.points) match {
              case Expr.Constant(value: Int, _) => program.copy(points = value)
              case _                            => program
            }
          case _ => program
        }
      }

    def update(program: LoyaltyProgram, actions: List[Facts[_]]): LoyaltyProgram =
      actions.foldLeft(program)((program, action) => update(program, action))
  }

  object fixture {

    import loyalty_model._

    val statusCondition = Condition(FlightBooking.status.get === FlightBookingStatus.Confirmed)
  
    val priceCondition = Condition(FlightBooking.price.get > 1000.0)
  
    val both = statusCondition && priceCondition
  
    val addPointsExpr = (LoyaltyAction.actionType := ActionType.AddPoints) ++ (LoyaltyAction.points := 100)
  
    val upgradeTierExpr = LoyaltyAction.actionType := ActionType.UpgradeTier
  
    val actions = Action.fromExpr(addPointsExpr) ++ Action.fromExpr(upgradeTierExpr)
  
    val rule = Rule(both, actions)
    
    val facts = Facts.empty.add(FlightBooking.price, 2000.0).add(FlightBooking.status, "Confirmed")

  }

}

object LoyaltyExample {

  def main(args: Array[String]) = {

    import graduation._
    import graduation2._
    import fixture._
    import loyalty_model._
    
    val engine = RuleEngine.fromRuleSet(RuleSet(Vector(rule)))  
    val actions = engine.update(facts)
    
    val loyaltyProgram = {
      import net.degoes.afd.examples.loyalty._
      import net.degoes.afd.examples.loyalty.LoyaltyTier._
      
      LoyaltyProgram("id", 0, LoyaltyTier.Bronze)
    }
    
    val updated = actions.map(ActionExecutor.update(loyaltyProgram, _)).getOrElse(loyaltyProgram)
    println(s"loyalty program:\n$loyaltyProgram\n\nupdated:\n$updated")
  }

}
