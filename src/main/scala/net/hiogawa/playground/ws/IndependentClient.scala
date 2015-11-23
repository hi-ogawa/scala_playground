package net.hiogawa.playground.ws

// java
import com.ning.http.client.AsyncHttpClientConfig

// scala
import scala.concurrent.Future
import play.api.libs.ws.{WS, WSClient, WSResponse}
import play.api.libs.ws.ning.NingWSClient

// implicits
import scala.concurrent.ExecutionContext.Implicits.global // ExecutionContext


object IndependentClient {

  def getNonBlockWithClient(url : String)(implicit c : WSClient) : Future[String] = {
    WS.clientUrl(url).get() map (resp => resp.body)
  }

  // NOTE: implicit val implicitClient = IndependentClient.makeDummyClient
  def makeDummyClient : NingWSClient = {
    val builder = new AsyncHttpClientConfig.Builder()
    val client = new NingWSClient(builder.build())
    client
  }
}
