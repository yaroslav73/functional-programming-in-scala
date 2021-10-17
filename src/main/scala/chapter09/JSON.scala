package chapter09

trait JSON
object JSON {
  case object JsNull extends JSON
  case class JsString(value: String) extends JSON
  case class JsNumber(value: Double) extends JSON
  case class JsBoolean(value: Boolean) extends JSON
  case class JsArray(value: IndexedSeq[JSON]) extends JSON
  case class JsObject(value: Map[String, JSON]) extends JSON

  def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    import P.{string => _, _}
    ???
  }

  /**
    * JSON parsing example.
    */
  object JSONExample extends App {
    val jsonTxt = """
      {
        "Company name" : "Microsoft Corporation",
        "Ticker"  : "MSFT",
        "Active"  : true,
        "Price"   : 30.66,
        "Shares outstanding" : 8.38e9,
        "Related companies" : [ "HPQ", "IBM", "YHOO", "DELL", "GOOG" ]
      }
      """

    val malformedJson1 = """
      {
        "Company name" ; "Microsoft Corporation"
      }
      """

    val malformedJson2 = """
      [
        [ "HPQ", "IBM",
        "YHOO", "DELL" ++
        "GOOG"
        ]
      ]
      """

    //  val P = Reference
    //  import ReferenceTypes.Parser
    //  def printResult[E](e: Either[E, JSON]) =
    //    e.fold(println, println)
    //  val json: Parser[JSON] = JSON.jsonParser(P)
    //  printResult { P.run(json)(jsonTxt) }
    //  println("--")
    //  printResult { P.run(json)(malformedJson1) }
    //  println("--")
    //  printResult { P.run(json)(malformedJson2) }
  }
}
