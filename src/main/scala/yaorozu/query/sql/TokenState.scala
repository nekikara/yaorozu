package yaorozu.query.sql

sealed trait TokenState {
  type Result = (TokenState, Option[Token])
  def event(c: Char): Result = (this, None)
  def flush(): Option[Token] = None
}
case object NeutralState extends TokenState {
  override def event(c: Char): Result = c match {
    case ' ' | '\n' => (WhiteSpaceState(c.toString), None)
    case '(' => (L_ParenthesisState(c.toString), None)
    case ')' => (R_ParenthesisState(c.toString), None)
    case _ => (WordState(c.toString), None)
  }
}
case class WhiteSpaceState(candidate: String) extends TokenState {
  override def event(c: Char): Result = c match {
    case ' ' | '\n' => (WhiteSpaceState(candidate :+ c), None)
    case '(' => (L_ParenthesisState(c.toString), None)
    case ')' => (R_ParenthesisState(c.toString), None)
    case _ => (WordState(c.toString), None)
  }
  override def flush(): Option[Token] = Some(Token.mapWord(candidate))
}
case class L_ParenthesisState(candidate: String) extends TokenState {
  override def event(c: Char): Result = c match {
    case ' ' | '\n' => (WhiteSpaceState(candidate :+ c), Some(Token.mapWord(candidate)))
    case '(' => (L_ParenthesisState(c.toString), Some(Token.mapWord(candidate)))
    case ')' => (R_ParenthesisState(c.toString), Some(Token.mapWord(candidate)))
    case _ => (WordState(c.toString), Some(Token.mapWord(candidate)))
  }
  override def flush(): Option[Token] = Some(Token.mapWord(candidate))
}
case class R_ParenthesisState(candidate: String) extends TokenState {
  override def event(c: Char): Result = c match {
    case ' ' | '\n' => (WhiteSpaceState(candidate :+ c), None)
    case '(' => (L_ParenthesisState(c.toString), Some(Token.mapWord(candidate)))
    case ')' => (R_ParenthesisState(c.toString), Some(Token.mapWord(candidate)))
    case _ => (WordState(c.toString), Some(Token.mapWord(candidate)))
  }
  override def flush(): Option[Token] = Some(Token.mapWord(candidate))
}
case class WordState(candidate: String) extends TokenState {
  override def event(c: Char): Result = c match {
    case ' ' => (WhiteSpaceState(c.toString), Some(Token.mapWord(candidate)))
    case '(' => (L_ParenthesisState(c.toString), Some(Token.mapWord(candidate)))
    case ')' => (R_ParenthesisState(c.toString), Some(Token.mapWord(candidate)))
    case _ => (WordState(candidate :+ c), None)
  }
  override def flush(): Option[Token] = Some(Token.mapWord(candidate))
}
case object EndState extends TokenState {
  override def flush(): Option[Token] = Some(End)
}

object TokenReader {
  def readOne(c: Char, state: TokenState): (TokenState, Option[Token]) = state.event(c)
}
