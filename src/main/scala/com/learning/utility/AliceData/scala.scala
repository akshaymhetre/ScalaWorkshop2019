import scala.io.Source

object AliceData extends App{
  val bookText = Source.
    fromInputStream(getClass.
      getResourceAsStream("aliceInWonderland.txt")).mkString
  val stopWordText = Source.fromInputStream(getClass.getResourceAsStream("stopWords.txt")).mkString

  val bookRegex = """[\s|:|.|,|"]+"""
  val stopWordRegex = "\\s+"
}