package pandabot.plugins

import pandabot.PandaBotSpec
import pandabot.parameters.Target

class PongPluginSpecs extends PandaBotSpec {
  import pandabot.Message._

  val pongPlugin = new PongPlugin()

  val octocat = Target.Nickname("octocat")

  behavior of "PongPlugin"

  it should "respond to ping message" in {
    val hash = "abcde1234"
    pongPlugin.respond(Ping(hash)) shouldEqual Some(Pong(hash))
  }

  it should "not respond to other type of messages" in {
    pongPlugin.respond(Notice(octocat, "Message")) should not be defined
    pongPlugin.respond(ChatMessage(octocat, "This is weird msg")) should not be defined
    pongPlugin.respond(Pong("ponging")) should not be defined
  }

}
