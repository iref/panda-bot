package pandabot

import cats.data.NonEmptyList
import com.twitter.io.Buf
import pandabot.parameters._

class DecoderSpec extends PandaBotSpec {
  import Message._

  it should "decode chat message" in {
    val text = Buf.Utf8("PRIVMSG panda :This is awesome message\r\n")
    val expected = ChatMessage(Target.Nickname("panda"), "This is awesome message")
    Decoder.decode(text).right.value should be(expected)
  }

  it should "decode ping message" in {
    val text = Buf.Utf8("PING :abcde1234\r\n")
    val expected = Ping("abcde1234")
    Decoder.decode(text).right.value should be(expected)
  }

  it should "decode pong message" in {
    val text = Buf.Utf8("PONG :abcde1234\r\n")
    val expected = Pong("abcde1234")
    Decoder.decode(text).right.value should be(expected)
  }

  it should "decode join message" in {
    val text = Buf.Utf8("JOIN #panda-bot,#octocat passw0rd\r\n")
    val expected = Join(
      NonEmptyList.of(Target.Channel("#panda-bot"), Target.Channel("#octocat")),
      List("passw0rd")
    )
    Decoder.decode(text).right.value should be(expected)
  }

  it should "decode leave message" in {
    val text = Buf.Utf8("PART #panda-bot,#octocat\r\n")
    val expected = Leave(NonEmptyList.of(Target.Channel("#panda-bot"), Target.Channel("#octocat")))
    Decoder.decode(text).right.value should be(expected)
  }

  it should "decode notice message" in {
    val text = Buf.Utf8("NOTICE #panda-bot :Super important magic notice\r\n")
    val expected = Notice(Target.Channel("#panda-bot"), "Super important magic notice")
    Decoder.decode(text).right.value should be(expected)
  }

  it should "decode nick message" in {
    val text = Buf.Utf8("NICK octocat\r\n")
    val expected = Nick(Target.Nickname("octocat"))
    Decoder.decode(text).right.value should be(expected)
  }

  it should "decode pass message" in {
    val text = Buf.Utf8("PASS passw0rd\r\n")
    val expected = Pass("passw0rd")
    Decoder.decode(text).right.value should be(expected)
  }

  it should "decode user message" in {
    val text = Buf.Utf8("USER pandabot * 0 :Panda Bot\r\n")
    val expected = User(Username("pandabot"), Hostname("*"), Hostname("0"), "Panda Bot")
    Decoder.decode(text).right.value should be(expected)
  }

  it should "decode numeric message" in {
    val text = Buf.Utf8("432 :some irc error\r\n")
    val expected = Numeric(432, Option("some irc error"))
    Decoder.decode(text).right.value should be(expected)
  }
}
