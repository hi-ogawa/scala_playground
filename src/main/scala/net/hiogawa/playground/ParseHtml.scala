package net.hiogawa.playground

import nu.validator.htmlparser.common.XmlViolationPolicy
import nu.validator.htmlparser.sax.HtmlParser
import org.xml.sax.InputSource
import scala.xml.Node
import scala.xml.parsing.NoBindingFactoryAdapter


object ParseHtml {

  def parse(body : String) : Node = {
    var adapter = new NoBindingFactoryAdapter
    val reader  = new HtmlParser
    reader.setXmlPolicy(XmlViolationPolicy.ALLOW)
    reader.setContentHandler(adapter)
    reader.parse(body)
    adapter.rootElem
  }

  def parseFromURL(url : java.net.URL) : Node = {
    val con : java.net.URLConnection = url.openConnection
    con.addRequestProperty("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_5) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/46.0.2490.80 Safari/537.36")
    val source  = new InputSource(con.getInputStream)
    var adapter = new NoBindingFactoryAdapter
    val reader  = new HtmlParser
    reader.setXmlPolicy(XmlViolationPolicy.ALLOW)
    reader.setContentHandler(adapter)
    reader.parse(source)
    adapter.rootElem
  }
}
