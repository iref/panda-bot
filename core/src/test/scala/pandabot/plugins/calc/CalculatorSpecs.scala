package pandabot.plugins.calc

import org.scalatest.{ Matchers, FlatSpec }

class CalculatorSpecs extends FlatSpec with Matchers {
  import Expression._

  val calculator = new Calculator()

  behavior of "Calculator"

  it should "evaluate Number expression to its value" in {
    calculator.evaluate(Number(2.3)) should equal(BigDecimal(2.3))
  }

  it should "evaluate negative numbers" in {
    val negativeValue = BigDecimal(-1.2)
    val negativeNumber = Number(negativeValue)
    calculator.evaluate(negativeNumber) should equal(negativeValue)
  }

  it should "evaluate addition to sum of its left and right expression" in {
    val exp = Add(Number(1.2), Number(2.3))
    calculator.evaluate(exp) should equal(BigDecimal(3.5))
  }

  it should "evaluate subtraction to value of left side minus value of right side" in {
    val exp = Subtract(Number(1.2), Number(1.0))
    calculator.evaluate(exp) should equal(BigDecimal(0.2))
  }

  it should "evaluate multiplication to value of left side multiplied by right side" in {
    val exp = Multiply(Number(2.0), Number(1.3))
    calculator.evaluate(exp) should equal(BigDecimal(2.6))
  }

  it should "evaluate division to value of left side divided by right side" in {
    val exp = Divide(Number(6.0), Number(2.0))
    calculator.evaluate(exp) should equal(BigDecimal(3.0))
  }
}
