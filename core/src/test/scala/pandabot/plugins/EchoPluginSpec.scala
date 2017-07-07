package pandabot.plugins

import pandabot.PandaBotSpec
import pandabot.parameters.Target

class EchoPluginSpecs extends PandaBotSpec {
  import pandabot.Message._

  val echoPlugin = new EchoPlugin()

  val octocat = Target.Nickname("panda-bot")

  behavior of "EchoPlugin"

  it should "respond to private messages, that start with 'panda echo'" in {
    val message = ChatMessage(octocat, "panda echo this is awesome test.")
    val expectedResponse = ChatMessage(
      octocat,
      "this is awesome test test test test."
    )

    echoPlugin.respond(message) shouldBe Some(expectedResponse)
  }

  it should "not respond to other messages than private" in {
    echoPlugin.respond(Ping("pingpong")) should not be defined
    echoPlugin.respond(Pong("pongping")) should not be defined
    echoPlugin.respond(Notice(octocat, "note")) should not be defined
  }

  it should "parse text, that starts with 'panda echo'" in {
    val message = "panda echo this is awesome"
    echoPlugin.parseText(message) shouldBe Some(("this is awesome", ""))
  }

  it should "not parse text, that does not start with 'panda echo'" in {
    val message = "this is evil message"
    echoPlugin.parseText(message) should not be defined
  }

  it should "parse text with suffix" in {
    val message = "panda echo this is awesome!!"
    echoPlugin.parseText(message) shouldBe Some(("this is awesome", "!!"))
  }

  it should "prepare message if there is some text" in {
    val text = "this is awesome message, it has to be sent"
    echoPlugin.prepareResponse(octocat, text, "") should equal(
      ChatMessage(octocat, s"${text} sent sent sent")
    )
  }

  it should "prepare message with suffix" in {
    val message = "this is awesome message"
    echoPlugin.prepareResponse(octocat, message, "!!") should equal(
      ChatMessage(octocat, "this is awesome message message message message!!")
    )
  }
}
