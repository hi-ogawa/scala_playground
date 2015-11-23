package net.hiogawa.playground.ws

import org.specs2.mutable._

import scala.concurrent.Await
import scala.concurrent.duration.Duration

class IndependentClientTest extends Specification {

  "IndependentClient" >> {


    // TODO: how to inject implicit declaration as an before all context or anything
    //       https://etorreborre.github.io/specs2/guide/SPECS2-3.6.5/org.specs2.guide.ContextObjects.html
    "getNonBlock" >> {
      implicit val implicitClient = IndependentClient.makeDummyClient
      val f = IndependentClient.getNonBlockWithClient("http://stackoverflow.com/questions/24881145/how-do-i-use-play-ws-library-in-normal-sbt-project-instead-of-play")
      Await.result(f.map((body) => {body.substring(0, 100).length}), Duration.Inf) must be_==(100)
      implicitClient.close()
      true
    }
  }
}
