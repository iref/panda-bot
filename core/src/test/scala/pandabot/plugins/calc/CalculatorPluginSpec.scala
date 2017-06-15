package pandabot.plugins.calc

import cats.syntax.either._
import org.mockito.Mockito._
import pandabot._

class CalculatorPluginSpec extends PandaBotSpec {
  import Expression._

  val calculatorMock = mock[Calculator]
  val expressionParserMock = mock[ExpressionParser]
  val plugin = new CalculatorPlugin(calculatorMock, expressionParserMock)

  behavior of "CalculatorPlugin"

  it should "not respond to message other than PrivateMessage" in {
    plugin.respond(Ping("hash")) should not be defined
    plugin.respond(Pong("hash")) should not be defined
    plugin.respond(Notice("extremely important notice")) should not be defined
    plugin.respond(Response("octocat", "Hi, Octocat!")) should not be defined
    plugin.respond(Unknown) should not be defined
  }

  it should "respond to PrivateMessage" in {
    val expression = Add(Number(1.0), Number(2.0))
    val msg = PrivateMessage("octocat", "panda compute 1+2, please")
    val response = Response("octocat", "And your result is... 3.0!!")

    when(calculatorMock.evaluate(expression)).thenReturn(BigDecimal(3.0))
    when(expressionParserMock.parse("1+2")).thenReturn(Either.right(expression))

    plugin.respond(msg) shouldBe Some(response)
  }

  it should "respond with error description if expression is invalid" in {
    when(expressionParserMock.parse("1+a")).thenReturn(Either.left("a is not a number"))
    val msg = PrivateMessage("octocat", "panda compute 1+a, please")
    val response = Response("octocat", "a is not a number")
    plugin.respond(msg) shouldBe Some(response)
  }

  it should "not respond if text is invalid" in {
    val invalidMessageA = PrivateMessage("octocat", "compute 1+2, please")
    plugin.respond(invalidMessageA) should not be defined

    val invalidMessageB = PrivateMessage("octocat", "compute 2+1")
    plugin.respond(invalidMessageB) should not be defined
  }
}
