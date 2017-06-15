package sk.drunkenpanda.bot.plugins.calc

import scala.util.matching.Regex
import cats.syntax.either._
import sk.drunkenpanda.bot.{ Message, PrivateMessage, Response }
import sk.drunkenpanda.bot.plugins.Plugin

/**
 * Calculates result of [[Expression]].
 */
class Calculator {
  import Expression._

  def evaluate(exp: Expression): BigDecimal = exp match {
    case Number(value) => value
    case Add(left, right) => evaluate(left) + evaluate(right)
    case Subtract(left, right) => evaluate(left) - evaluate(right)
    case Multiply(left, right) => evaluate(left) * evaluate(right)
    case Divide(left, right) => evaluate(left) / evaluate(right)
    case Power(base, power) => evaluate(base).pow(evaluate(power).toInt)
  }
}

/**
 * Plugin for doing simple math.
 * It handles only incoming [[PrivateMessage]].
 * Message must be in format `panda compute <expression>,* please`
 *
 * @param calculator calculates results of expressions
 * @param parser parses math expressions from input messages.
 */
class CalculatorPlugin(calculator: Calculator, parser: ExpressionParser) extends Plugin {

  private lazy val format: Regex = "panda compute ([\\S ]+?), please".r

  def respond(message: Message): Option[Message] = message match {
    case PrivateMessage(from, text) => prepareResponse(text).map(Response(from, _))
    case _ => None
  }

  private def prepareResponse(text: String): Option[String] = text match {
    case format(expression) => {
      val result = process(expression)
      Option(result.fold(identity, r => s"And your result is... ${r}!!"))
    }
    case _ => None
  }

  private def process(value: String): Either[String, BigDecimal] =
    for {
      expr <- parser.parse(value)
    } yield calculator.evaluate(expr)

  def onShutdown(): Unit = ()
}
