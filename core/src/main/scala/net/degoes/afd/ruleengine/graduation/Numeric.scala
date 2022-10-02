package net.degoes.afd.ruleengine.graduation

sealed trait Numeric[A] {

  type NumericType = A

  import Expr.NumericBinOpType
  import Expr.NumericBinOpType._

  def add(left: A, right: A): A

  def subtract(left: A, right: A): A

  def multiply(left: A, right: A): A

  def divide(left: A, right: A): A

  def modulo(left: A, right: A): A

  import PrimitiveType._
  def primitiveType: PrimitiveType[A] =
    (this match {
      case _: Numeric.ByteIsNumeric.type   => PrimitiveType.ByteType
      case _: Numeric.CharIsNumeric.type   => PrimitiveType.CharType
      case _: Numeric.IntIsNumeric.type    => PrimitiveType.IntType
      case _: Numeric.LongIsNumeric.type   => PrimitiveType.LongType
      case _: Numeric.FloatIsNumeric.type  => PrimitiveType.FloatType
      case _: Numeric.DoubleIsNumeric.type => PrimitiveType.DoubleType
    }).asInstanceOf[PrimitiveType[A]]

  def apply(binOp: NumericBinOpType)(left: A, right: A): A =
    binOp match {
      case Add      => add(left, right)
      case Subtract => subtract(left, right)
      case Multiply => multiply(left, right)
      case Divide   => divide(left, right)
      case Modulo   => modulo(left, right)
    }

}

object Numeric {
  implicit case object ByteIsNumeric extends Numeric[Byte] {
    def add(left: Byte, right: Byte): Byte = (left + right).toByte

    def subtract(left: Byte, right: Byte): Byte = (left - right).toByte

    def multiply(left: Byte, right: Byte): Byte = (left * right).toByte

    def divide(left: Byte, right: Byte): Byte = (left / right).toByte

    def modulo(left: Byte, right: Byte): Byte = (left % right).toByte

  }
  implicit case object CharIsNumeric extends Numeric[Char] {
    def add(left: Char, right: Char): Char = (left + right).toChar

    def subtract(left: Char, right: Char): Char = (left - right).toChar

    def multiply(left: Char, right: Char): Char = (left * right).toChar

    def divide(left: Char, right: Char): Char = (left / right).toChar

    def modulo(left: Char, right: Char): Char = (left % right).toChar

  }
  implicit case object IntIsNumeric extends Numeric[Int] {
    def add(left: Int, right: Int): Int = (left + right)

    def subtract(left: Int, right: Int): Int = (left - right)

    def multiply(left: Int, right: Int): Int = (left * right)

    def divide(left: Int, right: Int): Int = (left / right)

    def modulo(left: Int, right: Int): Int = (left % right)

  }
  implicit case object LongIsNumeric extends Numeric[Long] {
    def add(left: Long, right: Long): Long = (left + right)

    def subtract(left: Long, right: Long): Long = (left - right)

    def multiply(left: Long, right: Long): Long = (left * right)

    def divide(left: Long, right: Long): Long = (left / right)

    def modulo(left: Long, right: Long): Long = (left % right)

  }
  implicit case object FloatIsNumeric extends Numeric[Float] {
    def add(left: Float, right: Float): Float = (left + right)

    def subtract(left: Float, right: Float): Float = (left - right)

    def multiply(left: Float, right: Float): Float = (left * right)

    def divide(left: Float, right: Float): Float = (left / right)

    def modulo(left: Float, right: Float): Float = (left % right)

  }
  implicit case object DoubleIsNumeric extends Numeric[Double] {
    def add(left: Double, right: Double): Double = (left + right)

    def subtract(left: Double, right: Double): Double = (left - right)

    def multiply(left: Double, right: Double): Double = (left * right)

    def divide(left: Double, right: Double): Double = (left / right)

    def modulo(left: Double, right: Double): Double = (left % right)

  }

}
