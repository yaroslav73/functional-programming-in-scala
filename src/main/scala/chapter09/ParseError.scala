package chapter09

case class ParseError(errors: List[(Location, String)] = List()) {
  def push(location: Location, msg: String): ParseError = copy(errors = (location, msg) :: errors)
}

case class Location(input: String, offset: Int = 0) {
  lazy val line: Int = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val column: Int = input.slice(0, offset + 1).lastIndexOf('\n') match {
    case -1        => offset + 1
    case lineStart => offset - lineStart
  }
}
