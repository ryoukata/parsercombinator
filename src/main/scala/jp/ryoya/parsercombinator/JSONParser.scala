package jp.ryoya.parsercombinator

object JSONParser extends Combinator {

  def obj: Parser[Map[String, Any]] =
    (ss("{") ~> repsep(member, ss(",")) <~ ss("}") ^^ { Map() ++ _}) <~ spacing

  def arr: Parser[List[Any]] =
    (ss("[") ~> repsep(value, ss(",")) <~ ss("]")) <~ spacing

  def member: Parser[(String, Any)] =
    ((stringLiteral <~ spacing) ~ ss(":") ~ value ^^ { t => (t._1._1, t._2) }) <~ spacing

  def value: Parser[Any] = {
    obj <~ spacing |
      arr <~ spacing |
      stringLiteral <~ spacing |
      (floatingPointNumber ^^ { _.toDouble }) <~ spacing |
      ss("null") ^^ { _ => null } |
      ss("true") ^^ { _ => true } |
      ss("false") ^^ { _ => false }
  }

  def apply(input: String): Any = value(input)

}
