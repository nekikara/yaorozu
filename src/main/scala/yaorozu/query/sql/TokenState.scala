package yaorozu.query.sql

sealed trait TokenState {
  type Result = (TokenState, Option[String])
  def event(c: Char): Result = (this, None)
}
case object NeutralState extends TokenState {
  override def event(c: Char): Result = c match {
    case ' ' => (WhiteSpaceState(c.toString), None)
    case _ => (WordState(c.toString), None)
  }
}
case class WhiteSpaceState(candidate: String) extends TokenState {
  override def event(c: Char): Result = c match {
    case ' ' => (WhiteSpaceState(candidate :+ c), None)
    case _ => (WordState(c.toString), Some(candidate))
  }
}
case class WordState(candidate: String) extends TokenState {
  override def event(c: Char): Result = c match {
    case ' ' => (WhiteSpaceState(c.toString), Some(candidate))
    case _ => (WordState(candidate :+ c), None)
  }
}
