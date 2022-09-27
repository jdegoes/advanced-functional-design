/**
 * Declarative models are open to "fantastic interpretation". New ways of
 * execution can be introduced later, without them having to be explicitly
 * designed for.
 *
 * In this section, you'll explore an advanced way of interpreting the
 * declarative functional model you have been working on.
 */
package net.degoes.afd.ruleengine

import zio._

/**
 * Explore the space of incremental computation by creating an incremental rule
 * engine, which produces changes in actions, based on changes in input.
 */
object incremental {
  import net.degoes.afd.ruleengine.declarative._

  trait InputChange[Input]
  trait OutputChange[Output]

  trait IncrementalRuleEngine[Input, Output] {
    def update(input: InputChange[Input]): UIO[OutputChange[Output]]
  }
  object IncrementalRuleEngine {
    def apply[Input, Output, A, B](ruleSet: RuleSet[A, B]): UIO[IncrementalRuleEngine[Input, Output]] = ???
  }
}
