package pandabot

class UsernameSpec extends PandaBotSpec {

  it should "accept correct username" in {
    val username = Username.validate("loremasterCho")
    username.right.value.value should be ("loremasterCho")
  }

  it should "rejects blank username" in {
    val username = Username.validate("   ")
    username.left.value shouldBe a[Err.BlankParameter]
  }
}
