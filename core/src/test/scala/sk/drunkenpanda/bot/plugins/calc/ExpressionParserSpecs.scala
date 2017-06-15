package sk.drunkenpanda.bot.plugins.calc

import org.scalatest.{ EitherValues, FlatSpec, Matchers }

class ExpressionParserSpecs extends FlatSpec with Matchers with EitherValues {
  import Expression._

  val parser = new ExpressionParser

  behavior of "ExpressionParser"

  it should "parse simple numbers as Number" in {
    parser.parse("1").right.value should be(Number(1.0))
    parser.parse("1.2").right.value should be(Number(1.2))
  }

  it should "parse negative numbers as Number" in {
    val expected1 = Number(-1.0)
    parser.parse("-1").right.value should be(expected1)
    val expected2 = Number(-12.3)
    parser.parse("-12.3").right.value should be(expected2)
  }

  it should "parse numbers that starts with zero" in {
    parser.parse("012345").right.value should be(Number(12345.0))
  }

  it should "fail when parsing invalid decimal number" in {
    List("12.", "asd.12", "12.ads", ".12").map { n =>
      withClue(s"Input: $n") { parser.parse(n) should be('left) }
    }
  }

  it should "parse expression in parenthesis" in {
    val expected = Multiply(Number(3), Add(Number(1), Number(2)))
    parser.parse("3*(1+2)").right.value should equal(expected)
  }

  it should "parse '*' as BinaryOperator" in {
    val expected = Multiply(Number("1.0"), Number("2.3"))
    parser.parse("1.0*2.3").right.value should equal(expected)
  }

  it should "parse '/' as BinaryOperator" in {
    val expected = Divide(Number("1.0"), Number("3.4"))
    parser.parse("1.0/3.4").right.value should equal(expected)
  }

  it should "parse '^' as BinaryOperator" in {
    val expected = Power(Number("2.0"), Number("3.0"))
    parser.parse("2.0^3.0").right.value should equal(expected)
  }

  it should "parse '+' as BinaryOperator" in {
    val expected = Add(Number(1), Number(2))
    parser.parse("1+2").right.value should equal(expected)
  }

  it should "parse '-' as BinaryOperator" in {
    val expected = Subtract(Number(1), Number(2))
    parser.parse("1-2").right.value should equal(expected)
  }
}
