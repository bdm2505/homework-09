package ru.tinkoff.fintech.homework09.monix.crawler

import monix.eval.{Fiber, Task}

import monix.execution.Scheduler.Implicits.global
import cats.implicits._

trait Manager {
  self: Worker =>

  def crawler(crawlerQueue: MQueue[CrawlerMessage], data: CrawlerData): Task[Map[Host, Int]] = {
    def handleMessage(msg: CrawlerMessage, data: CrawlerData): Task[CrawlerData] = msg match {
      case Start(url) =>
        crawlUrl(data, url)

      case CrawlResult(url, links) =>
        val data2 = data.copy(inProgress = data.inProgress - url)

        links.foldM(data2) {
          case (d, link) =>
            val d2 = d.copy(referenceCount = d.referenceCount.updated(link.host, d.referenceCount.getOrElse(link.host, 0) + 1))
            crawlUrl(d2, link)
        }
    }

    def crawlUrl(data: CrawlerData, url: Url): Task[CrawlerData] = {
      if (!data.visitedLinks.contains(url)) {
        workerFor(data, url).flatMap {
          case (data2, workerQueue) =>
            workerQueue.offer(url).map { _ =>
              data2.copy(
                visitedLinks = data2.visitedLinks + url,
                inProgress = data2.inProgress + url
              )
            }
        }
      } else Task.now(data)
    }

    def workerFor(data: CrawlerData, url: Url): Task[(CrawlerData, MQueue[Url])] = {
      val workerQueue = MQueue.make[Url]
      worker(workerQueue, crawlerQueue).map { workerFiber =>
        (data.copy(workers = data.workers + (url -> WorkerData(workerQueue, workerFiber))), workerQueue)
      }
    }

    crawlerQueue.take.flatMap { msg =>
      handleMessage(msg, data).flatMap { data2 =>
        if (data2.inProgress.isEmpty) {
          data2.workers.values.map(_.fiber.cancel).toList.sequence_.map(_ => data2.referenceCount)
        } else {
          crawler(crawlerQueue, data2)
        }
      }
    }
  }
}
