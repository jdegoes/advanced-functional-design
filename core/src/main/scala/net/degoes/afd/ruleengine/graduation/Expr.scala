package net.degoes.afd.ruleengine.graduation

import zio._
import scala.annotation._
import scala.language.implicitConversions

sealed trait Expr[-In, +Out] { self =>

  final def ++[In1 <: In, Fields1, Fields2](that: Expr[In1, Facts[Fields2]])(implicit
    ev: Out <:< Facts[Fields1]
  ): Expr[In1, Facts[Fields1 & Fields2]] =
    Expr.CombineFacts(self.widen[Facts[Fields1]], that)

  final def +[In1 <: In, Out1 >: Out](that: Expr[In1, Out1])(implicit tag: Numeric[Out1]): Expr[In1, Out1] =
    Expr.BinaryNumericOp(self.widen, that, Expr.NumericBinOpType.Add, tag)

  final def -[In1 <: In, Out1 >: Out](that: Expr[In1, Out1])(implicit tag: Numeric[Out1]): Expr[In1, Out1] =
    Expr.BinaryNumericOp(self.widen, that, Expr.NumericBinOpType.Subtract, tag)

  final def *[In1 <: In, Out1 >: Out](that: Expr[In1, Out1])(implicit tag: Numeric[Out1]): Expr[In1, Out1] =
    Expr.BinaryNumericOp(self.widen, that, Expr.NumericBinOpType.Multiply, tag)

  final def /[In1 <: In, Out1 >: Out](that: Expr[In1, Out1])(implicit tag: Numeric[Out1]): Expr[In1, Out1] =
    Expr.BinaryNumericOp(self.widen, that, Expr.NumericBinOpType.Divide, tag)

  final def %[In1 <: In, Out1 >: Out](that: Expr[In1, Out1])(implicit tag: Numeric[Out1]): Expr[In1, Out1] =
    Expr.BinaryNumericOp(self.widen, that, Expr.NumericBinOpType.Modulo, tag)

  final def &&[In1 <: In](that: Expr[In1, Boolean])(implicit ev: Out <:< Boolean): Expr[In1, Boolean] =
    Expr.And(self.widen, that)

  final def ||[In1 <: In](that: Expr[In1, Boolean])(implicit ev: Out <:< Boolean): Expr[In1, Boolean] =
    Expr.Or(self.widen, that)

  final def !=[In1 <: In, Out1 >: Out](that: Expr[In1, Out1]): Expr[In1, Boolean] =
    !(self === that)

  final def <[In1 <: In, Out1 >: Out](that: Expr[In1, Out1]): Expr[In1, Boolean] =
    Expr.LessThan(self, that)

  final def <=[In1 <: In, Out1 >: Out](that: Expr[In1, Out1]): Expr[In1, Boolean] =
    Expr.LessThan(self, that) || (self === that)

  final def >[In1 <: In, Out1 >: Out](that: Expr[In1, Out1]): Expr[In1, Boolean] =
    !(self <= that)

  final def >=[In1 <: In, Out1 >: Out](that: Expr[In1, Out1]): Expr[In1, Boolean] =
    !(self < that)

  final def ===[In1 <: In, Out1 >: Out](that: Expr[In1, Out1]): Expr[In1, Boolean] =
    Expr.EqualTo(self, that)

  final def >>>[Out2](that: Expr[Out, Out2]): Expr[In, Out2] =
    Expr.Pipe(self, that)

  def eval(in: In): Out = Expr.eval(in, self)

  def ifTrue[In1 <: In, Out2](ifTrue: Expr[In1, Out2])(implicit ev: Out <:< Boolean): Expr.IfTrue[In1, Out2] =
    Expr.IfTrue(self.widen[Boolean], ifTrue)

  final def unary_!(implicit ev: Out <:< Boolean): Expr[In, Boolean] =
    Expr.Not(self.widen)

  final def widen[Out2](implicit ev: Out <:< Out2): Expr[In, Out2] = self.asInstanceOf[Expr[In, Out2]]

}

object Expr {

  final case class IfTrue[In, Out](condition: Expr[In, Boolean], ifTrue: Expr[In, Out]) {
    def otherwise(ifFalse: Expr[In, Out]) = IfThenElse(condition, ifTrue, ifFalse)
  }

  final case class Fact[In, K <: Singleton with String, V](factDef: FactDefinition.KeyValue[K, V], value: Expr[In, V])
      extends Expr[In, Facts[(K, V)]]
  final case class CombineFacts[In, V1, V2](
    left: Expr[In, Facts[V1]],
    right: Expr[In, Facts[V2]]
  ) extends Expr[In, Facts[V1 & V2]]
  final case class Constant[Out](value: Out, tag: EngineType[Out])            extends Expr[Any, Out]
  final case class And[In](left: Expr[In, Boolean], right: Expr[In, Boolean]) extends Expr[In, Boolean]
  final case class Or[In](left: Expr[In, Boolean], right: Expr[In, Boolean])  extends Expr[In, Boolean]
  final case class Not[In](condition: Expr[In, Boolean])                      extends Expr[In, Boolean]
  final case class EqualTo[In, Out](lhs: Expr[In, Out], rhs: Expr[In, Out])   extends Expr[In, Boolean]
  final case class LessThan[In, Out](lhs: Expr[In, Out], rhs: Expr[In, Out])  extends Expr[In, Boolean]
  final case class Input[In](engineType: EngineType[In])                      extends Expr[In, In]
  final case class Get[In, K <: Singleton with String, V](
    expr: Expr[In, Facts[(K, V)]],
    factDef: FactDefinition.KeyValue[K, V]
  ) extends Expr[In, V]
  final case class Pipe[In, Out1, Out2](left: Expr[In, Out1], right: Expr[Out1, Out2]) extends Expr[In, Out2]
  final case class BinaryNumericOp[In, Out](
    lhs: Expr[In, Out],
    rhs: Expr[In, Out],
    op: NumericBinOpType,
    tag: Numeric[Out]
  ) extends Expr[In, Out]
  final case class IfThenElse[In, Out](condition: Expr[In, Boolean], ifTrue: Expr[In, Out], ifFalse: Expr[In, Out])
      extends Expr[In, Out]
  sealed trait NumericBinOpType
  object NumericBinOpType {
    case object Add      extends NumericBinOpType
    case object Subtract extends NumericBinOpType
    case object Multiply extends NumericBinOpType
    case object Divide   extends NumericBinOpType
    case object Modulo   extends NumericBinOpType
  }

  implicit def apply[Out](out: Out)(implicit tag: PrimitiveType[Out]): Expr[Any, Out] =
    Constant(out, EngineType.Primitive(tag))

  implicit def apply[Out](out: Facts[Out]): Expr[Any, Facts[Out]] =
    Constant(out, EngineType.fromFacts(out))

  private def eval[In, Out](in: In, expr: Expr[In, Out]): Out = evalWithType(in, expr)._2

  private def evalWithType[In, Out](in: In, expr: Expr[In, Out]): (EngineType[Out], Out) =
    expr match {
      case Fact(factDef, value) =>
        implicit val tag = factDef.tag

        val result = Facts.empty.add(factDef, value)
        (EngineType.fromFacts(result).asInstanceOf[EngineType[Out]], result)

      case CombineFacts(lhs, rhs) =>
        val left    = eval(in, lhs)
        val right   = eval(in, rhs)
        val results = left ++ right
        (EngineType.fromFacts(results).asInstanceOf[EngineType[Out]], results)

      case Constant(value, tag) =>
        (tag.asInstanceOf[EngineType[Out]], value)

      case And(lhs, rhs) =>
        val left  = eval(in, lhs)
        val right = eval(in, rhs)
        (EngineType.fromPrimitive[Boolean].asInstanceOf[EngineType[Out]], left && right)

      case Or(lhs, rhs) =>
        val left  = eval(in, lhs)
        val right = eval(in, rhs)
        (EngineType.fromPrimitive[Boolean].asInstanceOf[EngineType[Out]], left || right)

      case Not(condition) =>
        (EngineType.fromPrimitive[Boolean].asInstanceOf[EngineType[Out]], !eval(in, condition))

      case EqualTo(lhs, rhs) =>
        val (leftType, left)   = evalWithType(in, lhs)
        val (rightType, right) = evalWithType(in, rhs)

        import PrimitiveType._

        (EngineType.fromPrimitive(PrimitiveType[Boolean]).asInstanceOf[EngineType[Out]], leftType.equals(left, right))

      case LessThan(lhs, rhs) =>
        val (leftType, left)   = evalWithType(in, lhs)
        val (rightType, right) = evalWithType(in, rhs)

        import PrimitiveType._

        (EngineType.fromPrimitive(PrimitiveType[Boolean]).asInstanceOf[EngineType[Out]], leftType.lessThan(left, right))

      case Input(engineType) =>
        engineType.asInstanceOf[EngineType[Out]] -> in.asInstanceOf[Out]

      case Get(expr, fd) =>
        val facts = eval(in, expr)

        val value = Unsafe.unsafe { implicit u =>
          facts.unsafe.get(fd).asInstanceOf[Out]
        }
        (fd.tag.asInstanceOf[EngineType[Out]], value)

      case Pipe(lhs, rhs) => evalWithType(eval(in, lhs), rhs)

      case BinaryNumericOp(lhs, rhs, op, tag0) =>
        val tag        = tag0.asInstanceOf[Numeric[Out]]
        val left: Out  = eval(in, lhs)
        val right: Out = eval(in, rhs)
        (EngineType.fromPrimitive(tag.primitiveType), tag(op)(left, right))

      case IfThenElse(condition, ifTrue, ifFalse) =>
        val bool = eval(in, condition)
        if (bool) evalWithType(in, ifTrue)
        else evalWithType(in, ifFalse)

    }

  def fact[In, K <: Singleton with String, V](factDef: FactDefinition.KeyValue[K, V], value: Expr[In, V]) =
    Fact(factDef, value)

  def ifThenElse[In, Out](
    condition: Expr[In, Boolean]
  )(ifTrue: Expr[In, Out], ifFalse: Expr[In, Out]): Expr[In, Out] =
    Expr.IfThenElse(condition, ifTrue, ifFalse)

  def input[A](engineType: EngineType[A]): Expr[A, A] = Input(engineType)

}
