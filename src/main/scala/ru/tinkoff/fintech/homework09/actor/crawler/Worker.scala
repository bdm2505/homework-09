package ru.tinkoff.fintech.homework09.actor.crawler
import akka.actor.{Actor, ActorLogging, ActorRef}

class Worker(http: Http, parser: Parsr, master: ActorRef) extends Actor with ActorLogging {
  implicit val ec = context.dispatcher
  override def receive: Receive = {
    case Crawl(url) =>
      http.get(url).map(parser.links).map(master ! CrawlResult(url, _)).recover{
        case _:Exception => master ! CrawlResult(url, Nil)
      }
  }
}
