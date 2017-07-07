package pandabot

import cats.data.NonEmptyList
import pandabot.parameters._

/**
 * IRC protocol message.
 */
sealed abstract class Message extends Product with Serializable {
  /**
   * Message code as defined in RFC, e.g. PRIVMSG or 402.
   */
  def code: String
}

object Message {
  /**
   *
   */
  final case class ChatMessage(target: Target, text: String) extends Message {
    val code = "PRIVMSG"
  }
  final case class Ping(hash: String) extends Message {
    val code = "PING"
  }
  final case class Pong(hash: String) extends Message {
    val code = "PONG"
  }
  final case class Notice(target: Target, note: String) extends Message {
    val code = "NOTICE"
  }
  final case class Join(channels: NonEmptyList[Target.Channel], passwords: List[String]) extends Message {
    val code = "JOIN"
  }
  final case class Leave(channels: NonEmptyList[Target.Channel]) extends Message {
    val code = "PART"
  }
  final case class User(username: Username, serverName: Hostname, hostname: Hostname, realName: String) extends Message {
    val code = "USER"
  }
  final case class Nick(nickname: Target.Nickname) extends Message {
    val code = "NICK"
  }
  final case class Pass(password: String) extends Message {
    val code = "PASS"
  }
  final case class Numeric(numCode: Int, raw: Option[String] = None) extends Message {
    def code = numCode.toString
  }
}
