package pandabot

sealed abstract class Err extends Exception {
  def message: String

  override def getMessage: String = message

  override def toString: String = message
}

object Err {
  abstract class ParameterErr extends Err {
    def parameterName: String
    def parameterValue: String
    def parameterDescription: String = s"$parameterName [$parameterValue]"
  }

  final case class MissingPrefix(parameterName: String, parameterValue: String, prefix: String) extends ParameterErr {
    def message = s"$parameterDescription must start with $prefix"
  }

  final case class WhitespacesNotAllowed(parameterName: String, parameterValue: String) extends ParameterErr {
    def message = s"$parameterDescription must not contain whitespace"
  }

  final case class BannedCharacter(parameterName: String, parameterValue: String, bannedCharacter: String) extends ParameterErr {
    def message = s"$parameterDescription must not contain $bannedCharacter"
  }

  final case class BlankParameter(parameterName: String) extends ParameterErr {
    val parameterValue = ""

    def message = s"$parameterDescription must not be blank."

    override def parameterDescription = parameterName

  }

  final case class WrongHostnameLabel(parameterValue: String) extends ParameterErr {
    val parameterName = "Hostname"
    def message =
      s"""
         |$parameterDescription labels may contain only letters, digits and hyphen.
         |Labels cannot start or end with hyphen and have between 1 and 63 characters.
       """.stripMargin
  }

  final case class HostnameEndsWithDot(parameterValue: String) extends ParameterErr {
    val parameterName = "Hostname"
    def message = s"$parameterDescription must not end with dot (.)"
  }

  final case class HostnameTooLong(parameterValue: String) extends ParameterErr {
    val parameterName = "Hostname"
    def message = s"$parameterDescription must be shorter than 264 charatecters."
  }
}
