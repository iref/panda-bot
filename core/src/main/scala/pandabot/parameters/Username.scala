package pandabot.parameters

import cats.syntax.either._
import pandabot.Err

final case class Username(value: String)

object Username {
  /**
   * Checks if value is valid [[Username]].
   * Value is valid hostname if it isn't blank.
   *
   * @param value the username value to check
   */
  def validate(value: String): Either[Err, Username] =
    if (value.trim.isEmpty) {
      Either.left(Err.BlankParameter("username"))
    } else {
      Either.right(Username(value))
    }
}
