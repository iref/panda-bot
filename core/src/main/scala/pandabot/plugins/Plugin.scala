package pandabot.plugins

import pandabot.Message
import pandabot.parameters.Target

trait Plugin {
  def respond(message: Message): Option[Message]

  def onShutdown(): Unit
}

class EchoPlugin extends Plugin {

  private lazy val format = "panda echo (.+?)([\\.\\!\\?]+)?".r

  private lazy val echoCount = 3

  override def respond(message: Message): Option[Message] = message match {
    case Message.ChatMessage(from, text) => for {
      (msg, suffix) <- parseText(text)
    } yield prepareResponse(from, msg, suffix)
    case _ => None
  }

  def prepareResponse(to: Target, message: String, suffix: String): Message = {
    val echo = List.fill(echoCount)(message.split(" ").last).mkString(" ")
    Message.ChatMessage(to, s"$message $echo$suffix")
  }

  def parseText(text: String): Option[(String, String)] = text match {
    case format(toEcho, null) => Some((toEcho, ""))
    case format(toEcho, suffix) => Some((toEcho, suffix))
    case _ => None
  }

  override def onShutdown(): Unit = ()
}

class PongPlugin extends Plugin {

  override def respond(message: Message) = message match {
    case Message.Ping(hash) => Option(Message.Pong(hash))
    case _ => None
  }

  override def onShutdown() = ()
}
