package ru.tinkoff.fintech.homework09.monix.crawler
import cats.effect.CancelToken
import monix.eval.{Fiber, Task}
import ru.tinkoff.fintech.homework09.crawler

trait Worker {
  def http: Http
  def parseLinks: Parsr

  def worker(workerQueue: MQueue[Url], crawlerQueue: MQueue[CrawlerMessage]): Task[Fiber[Unit]] = {
    val res: Task[Unit] = for {
      url <- workerQueue.take
      body <- http.get(url)
      res = parseLinks.links(body)
      _ <- crawlerQueue.offer(CrawlResult(url, res))
    } yield ()

    res.start
  }
}
