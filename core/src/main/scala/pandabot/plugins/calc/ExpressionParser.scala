package pandabot.plugins.calc

import fastparse.all._

/**
 * Parses [[Expression]] from string values.
 *
 * Parser uses following grammar:
 *
 * ## Language:
 * 0-9, '*', '+', '/', '-', '^', '(', ')', '.'
 *
 * ## Rules
 * ```
 * InputLine := InputEnd
 * Input := Term (['+'|'-'] Term)*
 * Term := Factor (['*'|'/'|'^'] Factor)*
 * Factor := Fraction | Parents
 * Parents := '(' Input ')'
 * Fraction := '-'* Digits | '-'* Digits Decimal*
 * Digits := [0-9]+
 * Decimal := '.'Digits
 * ```
 */
class ExpressionParser {
  import Expression._

  // Grammar Definition
  private val InputLine = P(Input ~ End)

  private val Input: P[Expression] = P(Term ~ (CharIn("+-").! ~/ Term).rep).map(eval)

  private val Term = P(Factor ~ (CharIn("*/^").! ~/ Factor).rep).map(eval)

  private val Factor = P(Fraction | Parents)

  private val Parents = P("(" ~/ Input ~ ")")

  // `.?` is optional
  // `.!` captures parsed Input
  private val Fraction = P(("-".? ~ Digits ~ Decimals.?).!).map(Number.apply)

  private val Digits = P(CharIn('0' to '9').rep(min=1))

  private val Decimals = P("." ~ Digits)

  // AST parsing

  /**
   * Folds Fastparse tree into single expression.
   */
  private def eval(tree: (Expression, Seq[(String, Expression)])) = {
    val (base, opt) = tree
    opt.foldLeft(base)(evalOperator)
  }

  /**
   * Creates binary expression based on mathematical operator and already parsed operands.
   */
  private def evalOperator(left: Expression, opAndRightExpr: (String, Expression)) = {
    val (op, right) = opAndRightExpr
    op match {
      case "+" => Add(left, right)
      case "-" => Subtract(left, right)
      case "*" => Multiply(left, right)
      case "/" => Divide(left, right)
      case "^" => Power(left, right)
    }
  }

  /**
   * Tries to parse expression from `value`.
   * If parsing succeeds, it returns right either that contains parsed expression.
   * Otherwise, it returns left either that contains message describing failure.
   */
  def parse(value: String): Either[String, Expression] =
    try {
      InputLine.parse(value) match {
        case Parsed.Success(expr, _) => Right(expr)
        case failure: Parsed.Failure => Left(failure.msg)
      }
    } catch {
      case e: Exception => Left(e.toString)
    }
}
