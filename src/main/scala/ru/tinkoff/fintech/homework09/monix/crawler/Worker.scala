package ru.tinkoff.fintech.homework09.monix.crawler

import cats.effect.CancelToken
import monix.eval.{Fiber, Task}
import monix.execution.Scheduler.Implicits.global
import ru.tinkoff.fintech.homework09.crawler

import scala.annotation.tailrec

trait Worker {
  def http: Http

  def parseLinks: Parsr


  def worker(workerQueue: MQueue[Url], crawlerQueue: MQueue[CrawlerMessage]): Task[Fiber[Unit]] = {

    def workUrl(taskUrl: Task[Url]): Task[Unit] = {
      taskUrl.flatMap { url =>
        http.get(url)
          .map(parseLinks.links)
          .onErrorRecover { case _: Exception => Nil }
          .flatMap(links => crawlerQueue.offer(CrawlResult(url, links)))
          .flatMap(_ => workUrl(workerQueue.take))
      }
    }

    workUrl(workerQueue.take).start
  }
}
