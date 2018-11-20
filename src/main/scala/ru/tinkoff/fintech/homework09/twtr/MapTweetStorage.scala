package ru.tinkoff.fintech.homework09.twtr

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}

class MapTweetStorage extends TweetStorage {
  private val mapID: mutable.Map[String, Tweet] = mutable.Map.empty
  private val mapTags: mutable.Map[String, Seq[Tweet]] = mutable.Map.empty

  def saveTweet(tweet: Tweet): Future[Unit] = Future {
    mapID += (tweet.id -> tweet)
    tweet.hashTags foreach { tag =>
      if (mapTags contains tag)
        mapTags update(tag, mapTags(tag) :+ tweet)
      else
        mapTags += (tag -> Seq(tweet))
    }
  }

  def getTweet(id: String): Future[Tweet] = Future {
    mapID(id)
  }

  def findTag(tag: String): Future[Seq[Tweet]] = Future {
    if (mapTags contains tag)
      mapTags(tag)
    else
      Seq.empty
  }

  override implicit val ec: ExecutionContext = ExecutionContext.global
}
