package pandabot.plugins.calc

/**
 * Abstract Syntax Tree for simple numeric expressions.
 * It support addition, subtraction, multiplication, division and powers.
 */
sealed abstract class Expression extends Product with Serializable

/**
 * Contains node types of Expression AST.
 */
object Expression {
  /**
   * Represents decimal number.
   */
  final case class Number(value: BigDecimal) extends Expression

  /** Number Factory */
  object Number {
    /**
     * Tries to create number from string.
     *
     * @throws NumberFormatException if string doesn't contain valid decimal number.
     */
    def apply(value: String): Number = Number(BigDecimal(value))

    /**
     * Tries to create number from double.
     */
    def apply(value: Double): Number = Number(BigDecimal(value))
  }

  /**
   * Node represents addition of two expressions.
   */
  final case class Add(leftExp: Expression, rightExp: Expression) extends Expression

  /**
   * Node represents subtraction of two expressions.
   */
  final case class Subtract(leftExp: Expression, rightExp: Expression) extends Expression

  /**
   * Node represents multiplication of two expressions.
   */
  final case class Multiply(leftExp: Expression, rightExp: Expression) extends Expression

  /**
   * Node represents division of two expressions.
   */
  final case class Divide(leftExp: Expression, rightExp: Expression) extends Expression

  /**
   * Node represents operation of `base` to power of `power`.
   */
  final case class Power(base: Expression, power: Expression) extends Expression
}
