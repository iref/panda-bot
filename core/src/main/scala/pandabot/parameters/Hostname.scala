package pandabot.parameters

import cats.syntax.either._
import pandabot.Err

final case class Hostname(value: String)

object Hostname {
  /**
   * Regular expression for checking hostname labels.
   */
  private val LabelRegExp = "(?i)[a-z0-9][a-z0-9\\-]{0,61}[a-z0-9]"

  /**
   * Checks if value is valid [[Hostname]].
   * Value is valid if:
   * 1. isn't blank
   * 2. it doesn't end with dot
   * 3. it is at most 255 characters long
   * 4. it contains only valid labels. Label must:
   *   a. Labels cannot start or end with hyphen
   *   b. Labels are at most 63 characters long
   *   c. Labels can contain only letters, digits or hyphen
   *
   * @param value the hostname value to check
   */
  def validate(value: String): Either[Err, Hostname] =
    value match {
      case _ if value.trim.isEmpty  => Either.left(Err.BlankParameter("Hostname"))
      case _ if value.endsWith(".") => Either.left(Err.HostnameEndsWithDot(value))
      case _ if value.length > 255  => Either.left(Err.HostnameTooLong(value))
      case _ if !checkLabels(value) => Either.left(Err.WrongHostnameLabel(value))
      case _                        => Either.right(Hostname(value))
    }

  private def checkLabels(value: String) = {
    value.split('.').forall(_.matches(LabelRegExp))
  }
}
