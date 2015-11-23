package net.hiogawa.playground.ws

import org.specs2.mutable._
import play.api.test.{FakeApplication, WithApplication}

import scala.concurrent.Await
import scala.concurrent.duration.Duration

// implicits
import scala.concurrent.ExecutionContext.Implicits.global

class WSStuffTest extends Specification {

  "WSStuff" >> {

    ".getParallelAndProcess" in new WithApplication(new FakeApplication) {
      val urls = List(
        "http://stackoverflow.com/questions/33718775/changing-bootstrap-btn-css-and-hover-action-issue",
        "http://stackoverflow.com/questions/33718719/github-hot-to-avoid-exceeding-search-limit-while-searching-for-accounts-detail",
        "http://stackoverflow.com/questions/33718754/my-javascript-script-for-changing-css-dont-seem-to-be-workin"
      )
      // NOTE: you would see output order is defferent between each test run
      Await.result(WSStuff.getParallelAndProcess(urls){(resp) =>
        println("-----------")
        println(resp.body.substring(0, 200))
      }, Duration.Inf).length must be_==(3)
    }
  }
}
