package pandabot.parameters

import cats.syntax.either._
import pandabot.Err

/**
 * Identifies entity that can send and receive IRC messages, e.g. user or channel.
 */
sealed abstract class Target extends Product with Serializable {
  def value: String
}

/** Target Factory. */
object Target {
  final case class Channel(value: String) extends Target
  final case class Nickname(value: String) extends Target

  /**
   * Tries to create new channel from given name.
   *
   * @param name the name of channel
   */
  def channel(name: String): Either[Err, Channel] =
    name match {
      case n if !n.startsWith("#") =>
        Either.left(Err.MissingPrefix("channel", name, "#"))

      case n if n.exists(_.isWhitespace) =>
        Either.left(Err.WhitespacesNotAllowed("channel", name))

      case n if n.exists(c => c == ',') =>
        Either.left(Err.BannedCharacter("channel", name, ","))

      case _ => Either.right(Channel(name))
    }

  /**
   * Tries to create new nickname from given string.
   *
   * @param nick the user's nickname candidate
   */
  def nickname(nick: String): Either[Err, Nickname] =
    nick match {
      case n if n.headOption.exists(!_.isLetter) =>
        Either.left(Err.MissingPrefix("nickname", nick, "ascii letter"))

      case n if n.exists(_.isWhitespace) =>
        Either.left(Err.WhitespacesNotAllowed("nickname", nick))

      case _ => Either.right(Nickname(nick))
    }
}
