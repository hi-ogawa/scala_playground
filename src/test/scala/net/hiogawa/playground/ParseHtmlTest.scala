package net.hiogawa.playground

import org.specs2.mutable._

class ParseHtmlTest extends Specification {

  // NOTE: on console, you can get same thing by new URL("file:///<absolute path to test.html>")
  val url : java.net.URL = getClass.getResource("/test.html")
  val htmlContent : String = scala.io.Source.fromURL(getClass.getResource("/test.html")).mkString

  "ParseHtml" >> {
    "parseFromURL" >> {
      val node = ParseHtml.parseFromURL(url)
      (node \\ "title" text) must contain("Modern Family")
    }

    "parse throws an error weirdly" >> {
      ParseHtml.parse(htmlContent) must throwA[java.net.MalformedURLException]
    }
  }
}
