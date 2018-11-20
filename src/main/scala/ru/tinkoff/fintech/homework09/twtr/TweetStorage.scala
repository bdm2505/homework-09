package ru.tinkoff.fintech.homework09.twtr

import scala.concurrent.{ExecutionContext, Future}

trait TweetStorage {
  implicit val ec: ExecutionContext

  def saveTweet(tweet: Tweet): Future[Unit]

  def getTweet(id: String): Future[Tweet]

  def findTag(tag: String): Future[Seq[Tweet]]

  def updateTweet(id: String)(funUpdate: Tweet => Tweet): Future[Tweet] = {
    for {
      tweet <- getTweet(id)
      tweetUpdated = funUpdate(tweet)
    } yield tweetUpdated match {
        case tweet: Tweet if tweet.id == id =>
          saveTweet(tweet)
          tweet
        case _ =>
          throw new Exception(s"fun update should not change id")
      }

  }
}

object TweetStorage {
  def apply(): TweetStorage = new MapTweetStorage
}
