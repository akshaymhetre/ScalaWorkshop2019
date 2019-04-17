package com.learning.workshop.s01e03

import com.learning.AliceData.getClass

import scala.io.Source

object AliceData {
  val bookText: String = Source.
    fromInputStream(getClass.
      getResourceAsStream("/aliceInWonderland.txt")).mkString
  val stopWordText: String = Source.fromInputStream(getClass.getResourceAsStream("/stopWords.txt")).mkString

  val bookRegex = """[\s|:|.|,|"]+"""
  val stopWordRegex = "\\s+"
}

object Exercise extends App {
  val aliceWords = AliceData.bookText
    .split(AliceData.bookRegex)
    .map(_.trim)
    .filterNot(_.isEmpty)

  val stopWords: Set[String] = AliceData.stopWordText
    .split(AliceData.stopWordRegex)
    .map(_.trim)
    .filterNot(_.isEmpty).toSet

  val topWords: Seq[String] = aliceWords
    .filterNot(stopWords)
    .groupBy(identity)
    .mapValues(_.length) // Map(a => 3, b => 1)
    .toSeq // List((a, 3), (b, 1), (c, 4))
    .sortBy(tup => - tup._2) // List((c, 4), (a, 3), (b, 1))
    .take(10)
    .map(_._1)


  println(topWords)

  /// Map(a => List(a,a,a), b => List(b))



}








