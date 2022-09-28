/**
 * Executable encodings are free of boilerplate and fast and flexible. However,
 * they tend to be less powerful and principled than declarative encodings,
 * which can be used with infinitely many different interpreters, each doing
 * something different (and useful!) with the model.
 *
 * In this section, you will refactor the rule engine you created to use the
 * declarative encoding. In the process, you will discover _expressions_, which
 * are recipes to describe the production of values from other values.
 *
 * To push this model to its logical conclusion, it will be necessary to
 * eliminate all Scala functions, which raises questions about how well-typed
 * you want to make the model.
 */
package net.degoes.afd.ruleengine

/**
 * Develop a fully declarative encoding of a rule engine. You are NOT allowed to
 * use any Scala functions in your model. Rather, your model must be purely
 * ADT-based. Attempt to make your model as type-safe as possible, but sacrifice
 * type-safety if necessary in order to avoid the embedding of Scala functions
 * in your model.
 *
 * You must develop an executor for the model which, given input and the rule
 * set, produces actions as output.
 */
object declarative {
  final case class RuleEngine[-In, +Out](update: In => Option[List[Out]]) { self =>
    def contramap[In2](f: In2 => In): RuleEngine[In2, Out] = RuleEngine(in2 => self.update(f(in2)))

    def orElse[In1 <: In, Out1 >: Out](that: RuleEngine[In1, Out1]): RuleEngine[In1, Out1] =
      RuleEngine(in => self.update(in) orElse that.update(in))

    def updateWith[Out1 >: Out](in: In)(defaultOut: Out1, combine: (Out1, Out1) => Out1): Out1 =
      self.update(in) match {
        case None => defaultOut

        case Some(outs) =>
          outs.reduceOption(combine).getOrElse(defaultOut)
      }
  }
  object RuleEngine {
    val empty: RuleEngine[Any, Nothing] = RuleEngine(_ => None)

    def collect[In, Out](pf: PartialFunction[In, Out]): RuleEngine[In, Out] =
      RuleEngine(in => pf.lift(in).map(List(_)))

    def constant[Out](out: Out): RuleEngine[Any, Out] = fromFunction(_ => out)

    def fromFunction[In, Out](f: In => Out): RuleEngine[In, Out] = RuleEngine(in => Some(List(f(in))))

    def fromRuleSet[In, Out](ruleSet: RuleSet[In, Out]): RuleEngine[In, Out] =
      RuleEngine(???)
  }

  final case class RuleSet[-In, +Out](rules: Vector[Rule[In, Out]]) { self =>
    def + [In1 <: In, Out1 >: Out](that: Rule[In1, Out1]): RuleSet[In1, Out1] =
      RuleSet(self.rules :+ that)

    def ++ [In1 <: In, Out1 >: Out](that: RuleSet[In1, Out1]): RuleSet[In1, Out1] =
      RuleSet(self.rules ++ that.rules)

    def addRule[In1 <: In, Out1 >: Out](that: Rule[In1, Out1]): RuleSet[In1, Out1] =
      self + that

  }
  object RuleSet {
    def apply[In, Out](rule1: Rule[In, Out], rules: Rule[In, Out]*): RuleSet[In, Out] =
      RuleSet(rule1 +: rules.toVector)

    val empty: RuleSet[Any, Nothing] = RuleSet(Vector.empty)

    def fromFile(file: String): RuleSet[_, _] = ???
  }

  /**
   * Use a case class to represent rule. If condition and action can be serialized, then
   * the rule can be serialized.
   *
   * Are we using execution context because it is easier?
   */
  trait Rule[-In, +Out]

  /**
   * Declarative encoding of the concept of orderability
   *
   * Type A has an ordering and we can only order it based on types we
   * know about. Essentially this is a proof that we can order the defined types.
   * @tparam A
   */
  sealed trait Ordering[A] extends scala.math.Ordering[A] {
    private val delegate: scala.math.Ordering[A] = Ordering.toScalaOrdering(this)
    def compare(x: A, y: A): Int = delegate.compare(x, y)
  }
  object Ordering {
   implicit case object ByteOrdering extends Ordering[Byte]
   implicit case object CharOrdering extends Ordering[Char]
   implicit case object IntOrdering extends Ordering[Int]
   implicit case object LongOrdering extends Ordering[Long]
   implicit case object FloatOrdering extends Ordering[Float]
   implicit case object DoubleOrdering extends Ordering[Double]
   implicit case object StringOrdering extends Ordering[String]

    private def toScalaOrdering[A](ord: Ordering[A]): scala.math.Ordering[A] =
      ord match {
        case ByteOrdering => scala.math.Ordering[Byte]
        case CharOrdering => scala.math.Ordering[Char]
        case IntOrdering => scala.math.Ordering[Int]
        case LongOrdering => scala.math.Ordering[Long]
        case FloatOrdering => scala.math.Ordering[Float]
        case DoubleOrdering => scala.math.Ordering[Double]
        case StringOrdering => scala.math.Ordering[String]
      }
  }

  trait Expression

  trait Condition

  /**
   *
   * @tparam In Is a generic representation of data. Does not work on Scala
   *            types like Int, String, etc. Expecting generic forma of data
   *            such as JSON, XML, YML, etc.
   * @tparam Out Pure data (no functions)
   */
  sealed trait Action[-In, +Out]
  object Action {

  }

  object loyalty {

  }

}
