package pandabot

import cats.data.NonEmptyList
import com.twitter.io.Buf
import pandabot.parameters._

class EncoderSpec extends PandaBotSpec {
  import Message._

  it should "encode chat message" in {
    val expected = Buf.Utf8("PRIVMSG toMe :This is awesome")
    val message = ChatMessage(Target.Nickname("toMe"), "This is awesome")
    Encoder.encode(message) should be(expected)
  }

  it should "encode ping message" in {
    val expected = Buf.Utf8("PING abcde")
    val message = Ping("abcde")
    Encoder.encode(message) should be(expected)
  }

  it should "encode pong message" in {
    val expected = Buf.Utf8("PONG abcde")
    val message = Pong("abcde")
    Encoder.encode(message) should be(expected)
  }

  it should "encode notice message" in {
    val expected = Buf.Utf8("NOTICE #panda-bot :my super notice")
    val message = Notice(Target.Channel("#panda-bot"), "my super notice")
    Encoder.encode(message) should be(expected)
  }

  it should "encode join message" in {
    val expected = Buf.Utf8("JOIN #drunken_panda")
    val message = Join(NonEmptyList.of(Target.Channel("#drunken_panda")), List[String]())
    Encoder.encode(message) should be(expected)
  }

  it should "encode leave message" in {
    val expected = Buf.Utf8("PART #drunken_panda,#foo")
    val channels: NonEmptyList[Target.Channel] = NonEmptyList.of(Target.Channel("#drunken_panda"), Target.Channel("#foo"))
    val message = Leave(channels)
    Encoder.encode(message) should be(expected)
  }

  it should "encode user message" in {
    val expected = Buf.Utf8("USER drunken_panda 0 * :Drunken Panda Bot")
    val message = User(Username("drunken_panda"), Hostname("0"), Hostname("*"), "Drunken Panda Bot")
    Encoder.encode(message) should be(expected)
  }

  it should "encode nick message" in {
    val expected = Buf.Utf8("NICK DrunkenPanda")
    val message = Nick(Target.Nickname("DrunkenPanda"))
    Encoder.encode(message) should be(expected)
  }

  it should "encode pass message" in {
    val expected = Buf.Utf8("PASS passw0rd")
    val message = Pass("passw0rd")
    Encoder.encode(message) should be(expected)
  }

  it should "encode numeric message" in {
    val expected = Buf.Utf8("432 :testing")
    val message = Numeric(432, Option(":testing"))
    Encoder.encode(message) should be(expected)
  }
}
