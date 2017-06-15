package pandabot

class DecoderSpec extends PandaBotSpec {
  import Message._

  it should "decode chat message" in {
    val text = "PRIVMSG panda :This is awesome message"
    val expected = ChatMessage("panda", "This is awesome message")
    Message.parse(text) should be(expected)
  }

  it should "decode ping message" in {
    val text = "PING :abcde1234"
    val expected = Ping("abcde1234")
    Message.parse(text) should be(expected)
  }

  it should "decode notice message" in {
    val text = "NOTICE :Super important magic notice"
    val expected = Notice("Super important magic notice")
    Message.parse(text) should be(expected)
  }
}
