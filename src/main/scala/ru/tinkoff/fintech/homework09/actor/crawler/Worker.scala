package ru.tinkoff.fintech.homework09.actor.crawler
import akka.actor.{Actor, ActorLogging, ActorRef}

class Worker(http: Http, parser: Parsr, master: ActorRef) extends Actor with ActorLogging {
  implicit val ec = context.dispatcher
  var free = true

  override def receive: Receive = {
    case Crawl(url) if free  =>
      free = false
      http.get(url).map(parser.links).recover{
        case _:Exception => Nil
      }.foreach(self ! Result(url, _))

    case Result(url, links) =>
      free = true
      master ! CrawlResult(url, links)
  }

}
