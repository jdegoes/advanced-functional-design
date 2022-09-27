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

/**
 * Create a persistence layer for your declarative rule set, using JSON as the
 * format. You may use any JSON library that you like.
 *
 * Note: You may have to make persistable any data types used by rule set.
 */
object persistence {
  import net.degoes.afd.ruleengine.declarative._

  def toJson[A, B](ruleSet: RuleSet[A, B]): String = ???

  def fromJson[A, B](json: String): RuleSet[A, B] = ???
}
