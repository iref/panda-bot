package pandabot

import cats.data.NonEmptyList
import cats.instances.either._
import cats.instances.list._
import cats.syntax.either._
import cats.syntax.traverse._
import com.twitter.io.Buf
import fastparse.all._
import pandabot.parameters._

/**
 * The BNF representation for this is:
 * <message>  ::= [':' <prefix> <SPACE> ] <command> <params> <crlf>
 * <prefix>   ::= <servername> | <nick> [ '!' <user> ] [ '@' <host> ]
 * <command>  ::= <letter> { <letter> } | <number> <number> <number>
 * <SPACE>    ::= ' ' { ' ' }
 * <params>   ::= <SPACE> [ ':' <trailing> | <middle> <params> ]
 *
 * <middle>   ::= <Any *non-empty* sequence of octets not including SPACE
 *               or NUL or CR or LF, the first of which may not be ':'>
 * <trailing> ::= <Any, possibly *empty*, sequence of octets not including
 *               NUL or CR or LF>
 *
 * <crlf>     ::= CR LF
 *
 * <target>     ::= <to> [ "," <target> ]
 * <to>         ::= <channel> | <user> '@' <servername> | <nick> | <mask>
 * <channel>    ::= ('#' | '&') <chstring>
 * <servername> ::= <host>
 * <host>       ::= see RFC 952 [DNS:4] for details on allowed hostnames
 * <nick>       ::= <letter> { <letter> | <number> | <special> }
 * <mask>       ::= ('#' | '$') <chstring>
 * <chstring>   ::= <any 8bit code except SPACE, BELL, NUL, CR, LF and
 *                     comma (',')>
 *
 * Other parameter syntaxes are:
 *
 * <user>       ::= <nonwhite> { <nonwhite> }
 * <letter>     ::= 'a' ... 'z' | 'A' ... 'Z'
 * <number>     ::= '0' ... '9'
 * <special>    ::= '-' | '[' | ']' | '\' | '`' | '^' | '{' | '}
 */
object Decoder {
  import Message._

  val letter = P(CharIn('a' to 'Z'))

  val digits = P(CharIn('0' to '9'))

  val special = P(CharIn("-[]\\`^{}"))

  val string = P((letter | digits | special).rep(min=1))

  val crlf = P("\r\n")

  val username = P(AnyChar ~ string.?).!.map(Username.validate)

  val nickname = P(letter ~ (letter | digits | special).rep).!.map(Target.nickname)

  val channel = P("#" ~ string).!.map(Target.channel)

  val target = P(nickname | channel)

  val text = P(":" ~ string.!).map(Either.right[Err, String])

  val hostname = P(string.!).map(Hostname.validate)

  val nick = P("NICK " ~ nickname).map(nickname => nickname.map(Nick))

  val pass = P("PASS " ~ (letter | digits | special).rep.!).map(p => Either.right(Pass(p)))

  val pong = P("PONG " ~/ string.!).map(hash => Either.right(Pong(hash)))

  val ping = P("PING " ~ string.!).map(hash => Either.right(Ping(hash)))

  val numeric = P(digits.rep.! ~ text).map {
    case (n, msg) => msg.map(m => Numeric(n.toInt, Option(m)))
  }
  val notice = P("NOTICE " ~ target ~ text).map {
    case (target, msg) =>
      for {
        t <- target
        m <- msg
      } yield Notice(t, m)
  }

  val user = P("USER " ~ username ~ " " ~ hostname ~ " " ~ hostname ~ text).map(parseUser)

  val privMsg = P("PRIVMSG " ~ target ~ text).map {
    case (target, msg) =>
      for {
        t <- target
        m <- msg
      } yield ChatMessage(t, m)
  }
  val join = P("JOIN " ~ channel.rep(sep=",", min=1) ~ " " ~ text.rep(sep=",")).map(parameters => parseJoin(parameters))

  val part = P("PART " ~ channel.rep(sep=",", min=1)).map(chs => transformToNEL(chs).map(Leave))

  val message: P[Either[Err, Message]] = P((nick | pass | pong | ping | numeric | notice | user | privMsg | join | part) ~ crlf)

  private def transformToNEL[A](as: Seq[Either[Err, A]]): Either[Err, NonEmptyList[A]] = {
    NonEmptyList.fromList(as.toList)
      .map(_.sequenceU)
      .getOrElse {
        Either.left(Err.DecodingErr("Seq must contain at least one element."))
      }
  }

  private def parseJoin(channelsAndPasswords: (Seq[Either[Err, Target.Channel]], Seq[Either[Err, String]])): Either[Err, Join] =
    for {
      channels <- transformToNEL(channelsAndPasswords._1)
      passwords <- channelsAndPasswords._2.toList.sequenceU
    } yield Join(channels, passwords.toList)

  private def parseUser(parameters: (Either[Err, Username], Either[Err, Hostname], Either[Err, Hostname], Either[Err, String])): Either[Err, User] = {
    for {
      username <- parameters._1
      serverName <- parameters._2
      hostname <- parameters._3
      realName <- parameters._4
    } yield User(username, serverName, hostname, realName)
  }

  private def decodeString(msg: String): Either[Err, Message] = message.parse(msg) match {
    case Parsed.Success(msg, _) => msg
    case failure: Parsed.Failure => Either.left(Err.DecodingErr(failure.msg))
  }

  def decode(buf: Buf): Either[Err, Message] = buf match {
    case Buf.Utf8(msg) => println(msg); decodeString(msg)
  }
}
