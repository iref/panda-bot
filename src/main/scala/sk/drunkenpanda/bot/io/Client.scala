package sk.drunkenpanda.bot.io

import rx.lang.scala.{Subscription, Observable}
import sk.drunkenpanda.bot.{Join, Message}

import scala.util.Try

trait IrcClient {
  def source: ConnectionSource

  def connect(username: String, nickname: String, realName: String): Unit = {
    source.write(s"NICK $nickname")
    source.write(s"USER $username 0 * :$realName")
  }

  def listen(channels: Seq[String]): Observable[String] = Observable[String](subscriber => {
    new Thread(new Runnable {
      override def run() = {
        try {
          channels.map(Join(_)).foreach(write(_))

          while (!subscriber.isUnsubscribed) {
            source.read(br => subscriber.onNext(br.readLine()))
          }

          if (!subscriber.isUnsubscribed) {
            subscriber.onCompleted
          }
        } catch {
          case e: Throwable => if (!subscriber.isUnsubscribed) subscriber.onError(e)
        }
      }
    }).start()
  })

  def write(message: Message): Unit = {
    val msg = Message.print(message)
    source.write(msg)
  }

  def shutdown: Unit = source.shutdown
}

class NetworkIrClient(val server: String, val port: Int) extends IrcClient {
  lazy val source = new SocketConnectionSource(server, port)
}
