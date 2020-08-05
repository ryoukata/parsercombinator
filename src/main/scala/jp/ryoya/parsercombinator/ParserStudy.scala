package jp.ryoya.parsercombinator

object ParserStudy {

  sealed class ParserResult[+T]
  case class Success[+T](value: T, next: String) extends ParserResult[T]
  case object Failure extends ParserResult[Nothing]

  type Parser[+T] = String => ParserResult[T]

  def trueParser: Parser[Boolean] = input =>
    if (input.startsWith("true")) {
      Success(true, input.substring("true".length))
    } else {
      Failure
    }

  def falseParser: Parser[Boolean] = input =>
    if (input.startsWith("false")) {
      Success(false, input.substring("false".length))
    } else {
      Failure
    }

  def booleanParser: Parser[Boolean] = input =>
    trueParser(input) match {
      case success@Success(_, _) => success
      case Failure => falseParser(input)
    }

}
