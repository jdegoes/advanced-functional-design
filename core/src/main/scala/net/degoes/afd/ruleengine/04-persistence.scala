/**
 * Persistence of declarative models opens up new toolboxes: suddenly, you can
 * create command-line, API, or visual tooling around functional models, which
 * can open up business logic to third-party stakeholders and possibly even
 * business users, depending on the level of abstraction.
 *
 * In this section, you will explore the persistence of declarative models, and
 * wrestle with the implications of type erasure.
 */
package net.degoes.afd.ruleengine

object persistence {
  import net.degoes.afd.ruleengine.declarative._

  def toJson(ruleSet: RuleSet): String = ???

  def fromJson(json: String): RuleSet = ???
}
