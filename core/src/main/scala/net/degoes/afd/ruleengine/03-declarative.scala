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
import net.degoes.afd.ruleengine.declarative.PrimitiveType.ByteType
import net.degoes.afd.ruleengine.declarative.PrimitiveType.IntType
import net.degoes.afd.ruleengine.declarative.PrimitiveType.BooleanType
import zio.Chunk

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
  final case class RuleEngine[-In, +Out](update: In => Option[Out]) 
  object RuleEngine {
    // def fromRuleSet[In, Out](ruleSet: RuleSet[In, Out]): RuleEngine[In, Out] =
    //   RuleEngine(ruleSet.update)
  }

  final case class RuleSet[-In, +Out](rules: Vector[Rule[In, Out]]) { self =>
    def +[In1 <: In, Out1 >: Out](rule: Rule[In1, Out1]): RuleSet[In1, Out1] =
      RuleSet(self.rules :+ rule)

    def ++[In1 <: In, Out1 >: Out](that: RuleSet[In1, Out1]): RuleSet[In1, Out1] =
      RuleSet(self.rules ++ that.rules)

    // def update(in: In): Option[Out] =
    //   self.rules.find(_.condition.eval(in)).map { rule =>
    //     rule.action.update(in)
    //   }
  }
  object RuleSet {
    val empty: RuleSet[Any, Nothing] = RuleSet(Vector.empty)
  }

  final case class Rule[-In, +Out](condition: Condition[In], action: Action[In, Out])
  
  sealed trait PrimitiveType[A] { self => 
    def ordering: scala.math.Ordering[A]
  }
  object PrimitiveType {
    implicit case object ByteType extends PrimitiveType[Byte] {
      override def ordering: scala.math.Ordering[Byte] = scala.math.Ordering[Byte]
    }
    implicit case object IntType extends PrimitiveType[Int] {
      override def ordering: scala.math.Ordering[Int] = scala.math.Ordering[Int]
    }
    implicit case object BooleanType extends PrimitiveType[Boolean] {
      override def ordering: scala.math.Ordering[Boolean] = scala.math.Ordering[Boolean]
    }
  }

  sealed trait Numeric[A]
  object Numeric {
    implicit case object IntIsNumeric extends Numeric[Int]
  }

  object Generic {
    sealed abstract case class FieldSpec[A](name: String, fieldType: PrimitiveType[A]) {
      // or literal types in 2.13
      type Type 
    }
    object FieldSpec {
      def apply[A](name: String)(implicit typ: PrimitiveType[A]): FieldSpec[A] =
        new FieldSpec(name, typ) {}
      type WithFieldType[A, B] = FieldSpec[A] { type FieldType = B }
    }

    val age = FieldSpec[Int]("age")
    implicitly[age.Type =:= age.Type]

    class Record[+Fields] private (private val map: Map[FieldSpec[_], Any]) { self => 
      def ++ [Fields2](that: Record[Fields2]): Record[Fields with Fields2] = 
        new Record(self.map ++ that.map) {}

      def get[A](fs: FieldSpec[A])(implicit ev: Fields <:< (fs.Type, A)): A = 
        map(fs).asInstanceOf[A]

      def add[A](fs: FieldSpec[A], value: A): Record[Fields with (fs.Type, A)] = 
        new Record(map.updated(fs, value)) {}

      def update[A](fs: FieldSpec[A], f: A => A)(implicit ev: Fields <:< (fs.Type, A)): Record[Fields] = 
        add(fs, f(self.get(fs)))
    }

    object Record {
      val empty: Record[Any] = new Record(Map())
    }
  }

  sealed trait Expr[-In, +Out] { self => 
    final def >>>[Out2](that: Expr[Out, Out2]): Expr[In, Out2] =
      Expr.Pipe(self, that)

    final def +[In1 <: In, Out1 >: Out](that: Expr[In1, Out1])(implicit tag: Numeric[Out1]): Expr[In1, Out1] = 
      Expr.BinaryNumericOp(self.widen, that, Expr.NumericBinOpType.Add, tag)
      
    final def && [In1 <: In] (that: Expr[In1, Boolean])(implicit ev: Out <:< Boolean): Expr[In1, Boolean] = Expr.And(self.widen, that)
    final def || [In1 <: In] (that: Expr[In1, Boolean])(implicit ev: Out <:< Boolean): Expr[In1, Boolean] = Expr.Or(self.widen, that)

    final def === [In1 <: In, Out1 >: Out](that: Expr[In1, Out1]): Expr[In1, Boolean] = 
      Expr.EqualTo(self, that)

    final def != [In1 <: In, Out1 >: Out](that: Expr[In1, Out1]): Expr[In1, Boolean] = 
      !(self === that)

    final def < [In1 <: In, Out1 >: Out](that: Expr[In1, Out1]): Expr[In1, Boolean] =
      Expr.LessThan(self, that)

    final def <= [In1 <: In, Out1 >: Out](that: Expr[In1, Out1]): Expr[In1, Boolean] =
      Expr.LessThan(self, that) || Expr.EqualTo(self, that)

    final def > [In1 <: In, Out1 >: Out](that: Expr[In1, Out1]): Expr[In1, Boolean] =
      Expr.GreaterThan(self, that)

    final def >= [In1 <: In, Out1 >: Out](that: Expr[In1, Out1]): Expr[In1, Boolean] =
      Expr.GreaterThan(self, that) || Expr.EqualTo(self, that)

    final def unary_! [In1 <: In](implicit ev: Out <:< Boolean): Expr[In1, Boolean] = Expr.Not(self.widen)

    def widen[Out2](implicit ev: Out <:< Out2): Expr[In, Out2] = self.asInstanceOf[Expr[In, Out2]]
  }
  object Expr {
    // implicit class ExprBoolSyntax[In](self: Expr[In, Boolean]) {
    //   final def &&[In1 <: In] (that: Expr[In1, Boolean]): Expr[In1, Boolean] = Expr.And(self.widen, that)
    // }
    
    final case class Constant[Out](value: Out, typ: PrimitiveType[Out]) extends Expr[Any, Out]
    final case class Not[In](value: Expr[In, Boolean]) extends Expr[In, Boolean]
    final case class And[In](left: Expr[In, Boolean], right: Expr[In, Boolean]) extends Expr[In, Boolean]
    final case class Or[In](left: Expr[In, Boolean], right: Expr[In, Boolean]) extends Expr[In, Boolean]

    final case class EqualTo[In, Out](left: Expr[In, Out], right: Expr[In, Out]) extends Expr[In, Boolean]
    final case class GreaterThan[In, Out](left: Expr[In, Out], right: Expr[In, Out]) extends Expr[In, Boolean]
    final case class LessThan[In, Out](left: Expr[In, Out], right: Expr[In, Out]) extends Expr[In, Boolean]
    final case class Input[In](typ: PrimitiveType[In]) extends Expr[In, In]
    final case class Pipe[In, Out1, Out2](left: Expr[In, Out1], right: Expr[Out1, Out2]) extends Expr[In, Out2]

    final case class BinaryNumericOp[In, Out](
      left: Expr[In, Out], right: Expr[In, Out], op: NumericBinOpType, tag: Numeric[Out]) extends Expr[In, Out]
    

    sealed trait NumericBinOpType
    object NumericBinOpType {
      final case object Add extends NumericBinOpType
    }
    
    implicit def apply[Out](out: Out)(implicit typ: PrimitiveType[Out]): Expr[Any, Out] = Constant(out, typ) 

    def input[A](implicit typ: PrimitiveType[A]): Expr[A, A] = Input(typ)
  }

  // type Condition[-In] = Expr[In, Boolean]

  final case class Condition[-In](expr: Expr[In, Boolean]) { self => 
    def isEqualTo[In](right: In)(implicit typ: PrimitiveType[In]): Condition[In] =
      Condition(Expr.input[In] === Expr(right))
  }


  // sealed trait Condition[-In] { self =>
  //   final def eval(in: In): Boolean = Condition.eval(self)(in)
  // }
  // object Condition {
  //   final case class Constant(value: Boolean) extends Condition[Any]
  //   final case class And[A](left: Condition[A], right: Condition[A]) extends Condition[A]
  //   final case class Or[A](left: Condition[A], right: Condition[A]) extends Condition[A]
  //   final case class EqualTo[A](value: A, typ: PrimitiveType[A]) extends Condition[A]
  //   final case class GreaterThan[A](value: A, typ: PrimitiveType[A]) extends Condition[A]
  //   final case class LessThan[A](value: A, typ: PrimitiveType[A]) extends Condition[A]

  //   val always: Condition[Any] = Constant(true)
  //   val never: Condition[Any] = Constant(false)

  //   def eval[In](condition: Condition[In])(in: In): Boolean =
  //     condition match {
  //       case Constant(value) => value
  //       case And(left, right) => eval(left)(in) && eval(right)(in)
  //       case Or(left, right) => eval(left)(in) || eval(right)(in)
  //       case EqualTo(value, typ) => typ.ordering.compare(value, in) == 0
  //       case LessThan(value, typ) => typ.ordering.compare(value, in) < 0
  //       case GreaterThan(value, typ) => typ.ordering.compare(value, in) > 0
  //     }

  //   def isEqualTo[In: Ordering](in: In)(implicit typ: PrimitiveType[In]): Condition[In] =
  //     EqualTo(in, typ)

  //   def isGreaterThan[In: Ordering](in: In)(implicit typ: PrimitiveType[In]): Condition[In] =
  //     GreaterThan(in, typ)

  // }

  type Action[-In, +Out] = Expr[In, Out]

  // sealed trait Action[-In, +Out] { self => 
  //   def >>>[Out2](that: Action[Out, Out2]): Action[In, Out2] =
  //     Action.Pipe(self, that)
  // }
  // object Action {
  //   final case class Pipe[In, Out1, Out2](left: Action[In, Out1], right: Action[Out1, Out2]) extends Action[In, Out2]
  //   final case class Constant[Out](value: Out, pr: PrimitiveType[Out]) extends Action[Any, Out]

  //   def constant[Out](out: Out)(implicit pt: PrimitiveType[Out]): Action[Any, Out] = Constant(out, pt)
  // }

  object loyalty {
    import net.degoes.afd.examples.loyalty._

    type LoyaltyEngine = RuleEngine[(FlightBooking, LoyaltyProgram), LoyaltyProgram]

    type LoyaltyRuleSet = RuleSet[(FlightBooking, LoyaltyProgram), LoyaltyProgram]
    
    // type LoyaltyRule = Rule[(FlightBooking, LoyaltyProgram), LoyaltyProgram]
    // object LoyaltyRule {
    //   def apply(condition: LoyaltyCondition, action: LoyaltyAction): LoyaltyRule =
    //     Rule(condition.contramap(_._1), action.contramap(_._2))
    // }

    type LoyaltyCondition = Condition[FlightBooking]
    object LoyaltyCondition {
      // def status(p: FlightBookingStatus => Boolean): LoyaltyCondition = 
      //   FlightBookingStatus.Cancelled.toString()
      //   Condition(booking => p(booking.status))

      // val priceCondition = Condition.isGreaterThan(1000)

      // def price(p: Double => Boolean): LoyaltyCondition = 
      //   Condition(booking => p(booking.price))
    }

    // type LoyaltyAction = Action[LoyaltyProgram, LoyaltyProgram]
    // object LoyaltyAction {
    //   def apply(f: LoyaltyProgram => LoyaltyProgram): LoyaltyAction = Action(f)
    //   def adjustPoints(value: Int): LoyaltyAction =
    //     LoyaltyAction(program => program.copy(points = program.points + value))

    //   val downgradeTier: LoyaltyAction =
    //     LoyaltyAction(program =>
    //       program.copy(tier = program.tier match {
    //         case LoyaltyTier.Gold   => LoyaltyTier.Silver
    //         case LoyaltyTier.Silver => LoyaltyTier.Bronze
    //         case LoyaltyTier.Bronze => LoyaltyTier.Bronze
    //       })
    //     )

    //   val none: LoyaltyAction = LoyaltyAction(identity[LoyaltyProgram])

    //   val upgradeTier: LoyaltyAction =
    //     LoyaltyAction(program =>
    //       program.copy(tier = program.tier match {
    //         case LoyaltyTier.Bronze => LoyaltyTier.Silver
    //         case LoyaltyTier.Silver => LoyaltyTier.Gold
    //         case LoyaltyTier.Gold   => LoyaltyTier.Gold
    //       })
    //     )
    // }
  }


}
