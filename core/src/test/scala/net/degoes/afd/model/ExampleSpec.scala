package net.degoes.afd.ruleengine

import zio._
import zio.test._ 
import zio.test.TestAspect._

object ExampleSpec extends ZIOSpecDefault {
  
  import graduation._
  import graduation2._ 
  import loyalty_model._
  import fixture._

  val priceCondition = Condition(FlightBooking.price.get > 1000.0)
  val upgradeTier = Action.fromExpr {
      LoyaltyAction.actionType := ActionType.UpgradeTier
    }
  val upgradeTierRule = Rule(priceCondition, upgradeTier)     

  def emptyFacts[In, Out](action: Action[In, Out]): List[Out] = action.eval(Facts.empty.asInstanceOf[In])

  def spec = 
    suite("EngineSpec") ( 
      test("empty ruleset produce no action"){
        val engine = RuleEngine.fromRuleSet(RuleSet(Vector()))
        val facts = Facts.empty
        assertTrue( engine.update(facts).isEmpty)
      },
      test("insatisfied conditions produce no action"){
        val engine = RuleEngine.fromRuleSet(RuleSet(Vector(upgradeTierRule)))
        val facts = Facts.empty.add(FlightBooking.price, 0.0)
        val actions = engine.update(facts)
        assertTrue(actions.isEmpty)
      },
      test("satisfied conditions produce single action"){
        val engine = RuleEngine.fromRuleSet(RuleSet(Vector(upgradeTierRule)))
        val facts = Facts.empty.add(FlightBooking.price, 2000.0)
        val expectedAction = emptyFacts(upgradeTier)
        val actions = engine.update(facts)
        assertTrue(actions == Some(expectedAction)) 
      },
      test("satisfied composite conditions produce two actions"){
        val engine = RuleEngine.fromRuleSet(RuleSet(Vector(rule)))
        val facts = Facts.empty.add(FlightBooking.price, 2000.0).add(FlightBooking.status, "Confirmed")
        val actions = engine.update(facts)
        assertTrue(actions.map(_.size).getOrElse(0) == 2)
      },
      test("update loyalty program"){
        val engine = RuleEngine.fromRuleSet(RuleSet(Vector(rule)))
        val facts = Facts.empty.add(FlightBooking.price, 2000.0).add(FlightBooking.status, "Confirmed")
        val actions = engine.update(facts)

        import net.degoes.afd.examples.loyalty._
        import net.degoes.afd.examples.loyalty.LoyaltyTier._

        val program = LoyaltyProgram("id", 0, LoyaltyTier.Bronze)

        val expectedProgram = LoyaltyProgram("id", 100, LoyaltyTier.Gold)
        val updated = ActionExecutor.update(program, actions.get)

        assertTrue(updated == expectedProgram)
      })      

}