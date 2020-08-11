package jp.ryoya.parsercombinator

case class NaturalNumberArith(num: Int, terms: List[(String, Int)])

object NaturalNumberArithParser extends Combinator {

  def digitExcludingZero: Parser[String] = oneOf('1' to '9')

  def digit: Parser[String] = select(s("0"), digitExcludingZero)

  def naturalNumber: Parser[Int] = map(combine(digitExcludingZero, rep(digit)), {
    t: (String, List[String]) => (t._1 + t._2.mkString).toInt
  })

  def apply(input: String): ParseResult[NaturalNumberArith] =
    map(combine(naturalNumber, rep(select(combine(s("+"), naturalNumber), combine(s("-"), naturalNumber)))), {
      t: (Int, List[(String, Int)]) => NaturalNumberArith(t._1, t._2)
    })(input)

}
