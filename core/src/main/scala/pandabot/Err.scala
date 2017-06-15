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
    def parameterDescription: String = s"$parameterName=$parameterValue"
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
    def parameterValue = ""

    def message = s"$parameterDescription must not be blank."

    override def parameterDescription = parameterName

  }
}
