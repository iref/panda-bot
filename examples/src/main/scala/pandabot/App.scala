package pandabot

import pandabot.io.NetworkIrClient
import pandabot.plugins.AbstractPluginModule
import pandabot.plugins.EchoPlugin
import pandabot.plugins.PongPlugin
import pandabot.plugins.calc.{ Calculator, ExpressionParser, CalculatorPlugin }

object App {

  def main(args: Array[String]): Unit = {
    val client = new NetworkIrClient("irc.freenode.net", 6667)
    val pluginModule = new SimplePluginModule
    val bot = new Bot(client, pluginModule)
    bot.start("Drunken_Panda", "Drunken_Panda", "Drunken Panda Bot", List("#drunken_panda"))
  }

  def registerShutdownHook(bot: Bot) = Runtime.getRuntime.addShutdownHook(new Thread() {
    override def run() = bot.stop
  })

  class SimplePluginModule extends AbstractPluginModule {

    override val plugins = Set(new PongPlugin(), new EchoPlugin(),
      new CalculatorPlugin(new Calculator, new ExpressionParser))
  }
}

