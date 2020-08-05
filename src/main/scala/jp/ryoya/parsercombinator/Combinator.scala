package jp.ryoya.parsercombinator

abstract class Combinator {

  sealed trait ParseResult[+T]
  case class Success[+T](value: T, next: String) extends ParseResult[T]
  case object Failure extends ParseResult[Nothing]

  type Parser[+T] = String => ParseResult[T]

  def string(literal: String): Parser[String] = input => {
    if (input.startsWith(literal)) {
      Success(literal, input.substring(literal.length))
    } else {
      Failure
    }
  }

  /**
   * string parser
   * @param literal 文字列
   * @return
   */
  def s(literal: String): Parser[String] = string(literal)

  def select[T, U >: T](left: => Parser[T], right: => Parser[U]): Parser[U] = input => {
    left(input) match {
      case success@Success(_, _) => success
      case Failure => right(input)
    }
  }

  def map[T, U](parser: Parser[T], function: T => U): Parser[U] = input => {
    parser(input) match {
      case Success(value, next) => Success(function(value), next)
      case Failure => Failure
    }
  }

}
