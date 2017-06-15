package pandabot

import rx.lang.scala.Subscription
import pandabot.io._
import pandabot.plugins.PluginModule

class Bot(ircClient: IrcClient, pluginModule: PluginModule) {

  def start(username: String, nickname: String, realName: String, channels: Seq[String]): Subscription = {
    // check and handle errors
    ircClient.connect(username, nickname, realName)
    ircClient.listen(channels).filter(!_.isEmpty)
      .map(s => Message.parse(s))
      .map(pluginModule.process(_))
      .subscribe(
        responseMessages => {
          responseMessages.foreach(ircClient.write(_))
        },
        err => {
          // report errors
        },
        () => stop
      )
  }

  def stop(): Unit = {
    ircClient.shutdown
    pluginModule.shutdown
  }
}
