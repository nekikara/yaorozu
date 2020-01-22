package yaorozu.query.sql

sealed trait TokenState {
  type Result = (TokenState, Option[String])
  def event(c: Char): Result = (this, None)
  def flush(): Option[String] = None
}
case object NeutralState extends TokenState {
  override def event(c: Char): Result = c match {
    case ' ' | '\n' => (WhiteSpaceState(c.toString), None)
    case _ => (WordState(c.toString), None)
  }

}
case class WhiteSpaceState(candidate: String) extends TokenState {
  override def event(c: Char): Result = c match {
    case ' ' | '\n' => (WhiteSpaceState(candidate :+ c), None)
    case _ => (WordState(c.toString), Some(candidate))
  }

  override def flush(): Option[String] = Some(candidate)
}
case class WordState(candidate: String) extends TokenState {
  override def event(c: Char): Result = c match {
    case ' ' => (WhiteSpaceState(c.toString), Some(candidate))
    case _ => (WordState(candidate :+ c), None)
  }
  override def flush(): Option[String] = Some(candidate)
}

object TokenReader {
  def readOne(c: Char, state: TokenState): (TokenState, Option[String]) = state.event(c)
}
