package jp.ryoya.parsercombinator

import org.graalvm.compiler.graph.Node.Input

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

  def oneOf(chars: Seq[Char]): Parser[String] = input => {
    if (input.length != 0 && chars.contains(input.head)) {
      Success(input.head.toString, input.tail)
    } else {
      Failure
    }
  }

  def select[T, U >: T](left: => Parser[T], right: => Parser[U]): Parser[U] = input => {
    left(input) match {
      case success@Success(_, _) => success
      case Failure => right(input)
    }
  }

  def combine[T, U](left: Parser[T], right: Parser[U]): Parser[(T, U)] = input => {
    left(input) match {
      case Success(value1, next1) =>
        right(next1) match {
          case Success(value2, next2) =>
            Success((value1, value2), next2)
          case Failure =>
            Failure
        }
      case Failure =>
        Failure
    }
  }

  // 繰り返しのパーサーコンビネーター
  def rep[T](parser: Parser[T]): Parser[List[T]] = input => {

    def repeatRec(input: String): (List[T], String) = parser(input) match {
      case Success(value, next1) =>
        val (result, next2) = repeatRec(next1)
        (value::result, next2)
      case Failure =>
        (Nil, input)
    }

    val (result, next) = repeatRec(input)
    Success(result, next)
  }

  def map[T, U](parser: Parser[T], function: T => U): Parser[U] = input => {
    parser(input) match {
      case Success(value, next) => Success(function(value), next)
      case Failure => Failure
    }
  }

}
