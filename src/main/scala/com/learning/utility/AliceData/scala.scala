package com.learning

import scala.io.Source

object AliceData extends App{
  val bookText: String = Source.
    fromInputStream(getClass.
      getResourceAsStream("aliceInWonderland.txt")).mkString
  val stopWordText: String = Source.fromInputStream(getClass.getResourceAsStream("stopWords.txt")).mkString

  val bookRegex = """[\s|:|.|,|"]+"""
  val stopWordRegex = "\\s+"
}