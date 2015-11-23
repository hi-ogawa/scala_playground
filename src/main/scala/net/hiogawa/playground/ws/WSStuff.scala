package net.hiogawa.playground.ws

import scala.concurrent.Future
import play.api.libs.ws.{WS, WSResponse}

// implicits
import scala.concurrent.ExecutionContext.Implicits.global // ExecutionContext
import play.api.Play.current // Application


object WSStuff {

  def getNonBlock(url : String) : Future[String] = {
    WS.url(url).get() map (resp => resp.body)
  }

  def getParallelAndProcess[T](urls : List[String])(f : WSResponse => T) : Future[List[T]] = {
    sequence(urls.map((url) => WS.url(url).get() map f))
  }

  // TODO: use applicative from scalaz
  // cf. https://www.haskell.org/hoogle/?hoogle=sequence
  def sequence[T](ms : List[Future[T]]) : Future[List[T]] = {
    ms.foldLeft[Future[List[T]]](Future(List.empty))((accFutures, future) =>
      for {
        accResps <- accFutures
        resp <- future
      } yield (resp :: accResps)
    )
  }
}
