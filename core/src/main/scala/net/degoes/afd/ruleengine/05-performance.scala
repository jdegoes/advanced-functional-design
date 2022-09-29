/**
 * Declarative encodings offer additional opportunities for high-performance
 * execution, because the models can be inspected, transformed, and optimized.
 *
 * In this section, you will explore some techniques for writing
 * high-performance executors of declarative functional models.
 */
package net.degoes.afd.ruleengine

/**
 * Rewrite the executor you developed for the rule engine, but this time,
 * focusing on performance, using such techniques as partial evaluation, and
 * ensuring that the executor is as efficient as possible, minimizing
 * allocations and invocations of virtual methods.
 */
object performance {
  import net.degoes.afd.ruleengine.declarative._

  def execute[In, Out, Input](input: Input, ruleSet: RuleSet[In, Out]) = ???
}
