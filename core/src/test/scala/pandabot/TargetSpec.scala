package pandabot

class TargetSpec extends PandaBotSpec {
  behavior of "Target"

  it should "reject channel names that don't start with #" in {
    val channel = Target.channel("abcdef")
    channel.left.value shouldBe a [Err.MissingPrefix]
  }

  it should "reject channel names that contain whitespaces" in {
    val channel = Target.channel("#abc def\t")
    channel.left.value shouldBe a [Err.WhitespacesNotAllowed]
  }

  it should "reject channel names that contain comma" in {
    val channel = Target.channel("#abc,def")
    channel.left.value shouldBe a [Err.BannedCharacter]
  }

  it should "accept correct channel names" in {
    val channel = Target.channel("#panda")
    channel.right.value.value should be("#panda")
  }

  it should "reject nickname that don't start with ascii letter" in {
    val nickname = Target.nickname("123panda")
    nickname.left.value shouldBe a [Err.MissingPrefix]
  }

  it should "reject nickname that contain whitespaces" in {
    val nickname = Target.nickname("loremaster cho")
    nickname.left.value shouldBe a [Err.WhitespacesNotAllowed]
  }

  it should "accept correct nickname" in {
    val nickname = Target.nickname("Lore[m4ster]Cho")
    nickname.right.value.value should be("Lore[m4ster]Cho")
  }
}
