package pandabot.plugins

import pandabot.Message

trait PluginModule {
  def plugins: Set[Plugin]

  def process(message: Message): Set[Message]

  def shutdown(): Unit
}

abstract class AbstractPluginModule extends PluginModule {

  def process(message: Message): Set[Message] =
    plugins flatMap (_.respond(message))

  def shutdown(): Unit = plugins.foreach(_.onShutdown)
}
