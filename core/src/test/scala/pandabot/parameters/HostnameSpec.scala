package pandabot.parameters

import pandabot.{ Err, PandaBotSpec }

class HostnameSpec extends PandaBotSpec {

  it should "accept correct hostname" in {
    val hostname = Hostname.validate("panda-bot123.me")
    hostname.right.value.value should be("panda-bot123.me")
  }

  it should "reject blank hostname" in {
    val hostname = Hostname.validate("   ")
    hostname.left.value shouldBe a[Err.BlankParameter]
  }

  it should "reject hostname that ends with dot" in {
    val hostname = Hostname.validate("pandabot.me.")
    hostname.left.value shouldBe a [Err.HostnameEndsWithDot]
  }

  it should "reject hostname if label starts with hyphen" in {
    val hostname = Hostname.validate("-pandabot.com")
    hostname.left.value shouldBe a [Err.WrongHostnameLabel]
  }

  it should "reject hostname if label ends with hyphen" in {
    val hostname = Hostname.validate("pandabot.com-")
    hostname.left.value shouldBe a [Err.WrongHostnameLabel]
  }

  it should "reject hostname if label is longer than 63 characters" in {
    val hostname = Hostname.validate(List.fill(64)("a").mkString)
    hostname.left.value shouldBe a [Err.WrongHostnameLabel]
  }

  it should "reject hostname if label contains other characters than letters, digits or hyphen" in {
    val hostname = Hostname.validate("pandabot!.me")
    hostname.left.value shouldBe a [Err.WrongHostnameLabel]
  }

  it should "reject hostnames longer than 255 characters" in {
    val hostname = Hostname.validate(List.fill(256)("a").mkString)
    hostname.left.value shouldBe a [Err.HostnameTooLong]
  }
}
