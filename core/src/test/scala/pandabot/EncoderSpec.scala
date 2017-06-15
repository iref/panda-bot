package pandabot

class EncoderSpec extends PandaBotSpec {
  import Message._

  it should "encode chat message" in {
    val expected = "PRIVMSG toMe :This is awesome"
    val message = ChatMessage("toMe", "This is awesome")
    Message.print(message) should be(expected)
  }

  it should "encode pong message" in {
    val expected = "PONG :abcde"
    val message = Pong("abcde")
    Message.print(message) should be(expected)
  }

  it should "encode notice message" in {
    val expected = "NOTICE :my super notice"
    val message = Notice("my super notice")
    Message.print(message) should be(expected)
  }

  it should "encode join message" in {
    val expected = "JOIN #drunken_panda"
    val message = Join("#drunken_panda")
    Message.print(message) should be(expected)
  }

  it should "encode leave message" in {
    val expected = "PART #drunken_panda"
    val message = Leave("#drunken_panda")
    Message.print(message) should be(expected)
  }

  it should "encode user message" in {
    val expected = "USER drunken_panda 0 * :Drunken Panda Bot"
    val message = User("drunken_panda", "Drunken Panda Bot")
    Message.print(message) should be(expected)
  }

  it should "encode nick message" in {
    val expected = "NICK DrunkenPanda"
    val message = Nick("DrunkenPanda")
    Message.print(message) should be(expected)
  }

  it should "encode pass message" in {
    val expected = "PASS passw0rd"
    val message = Pass("passw0rd")
    Message.print(message) should be(expected)
  }
}
