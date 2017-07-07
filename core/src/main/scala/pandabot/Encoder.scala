package pandabot

import com.twitter.io.Buf
import pandabot.parameters._

/**
 * Encodes [[Message]] into Twitter byte buffer.
 */
object Encoder {
  import Message._

  def encode(message: Message): Buf = {
    val encodedString = "%s %s".format(message.code, encodeParameters(message))
    Buf.Utf8(encodedString)
  }

  private def encodeParameters(message: Message): String = message match {
    case ChatMessage(target, text) => s"${target.value} :$text"
    case Ping(hash) => hash
    case Pong(hash) => hash
    case Notice(target, note) => s"${target.value} :$note"
    case Join(channels, passwords) =>
      val names = encodeList(channels.toList)(_.value)
      if (passwords.isEmpty) {
        names
      } else {
        val pswds = encodeList(passwords)(identity)
        s"$names $pswds"
      }
    case Leave(channels) => encodeList(channels.toList)(_.value)
    case User(username, serverName, hostname, realName) =>
      s"${username.value} ${serverName.value} ${hostname.value} :$realName"
    case Nick(nickname) => s"${nickname.value}"
    case Pass(password) => password
    case Numeric(_, raw) =>
      raw.getOrElse("")
  }

  private def encodeList[A](as: List[A])(f: A => String) =
    as.map(f).mkString(",")
}
