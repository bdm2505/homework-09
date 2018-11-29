package ru.tinkoff.fintech.homework09.actor.crawler

import akka.actor.{Actor, ActorLogging, ActorRef}

class Worker(http: Http, parser: Parsr, master: ActorRef) extends Actor with ActorLogging {
  implicit val ec = context.dispatcher

  var urls: List[Url] = List.empty
  var free = true

  def work(): Unit = if (free && urls.nonEmpty) {
    free = false
    val url = urls.head
    urls = urls.tail
    http.get(url)
      .map(parser.links)
      .recover { case _: Exception => Nil }
      .foreach(self ! Result(url, _))
  }


  override def receive: Receive = {
    case Crawl(url) =>
      urls ::= url
      work()

    case Result(url, links) =>
      master ! CrawlResult(url, links)
      free = true
      work()
  }

}
