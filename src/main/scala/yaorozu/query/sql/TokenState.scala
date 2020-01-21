package yaorozu.query.sql

import cats.data.State

sealed trait TokenState {
  type Result = (TokenState, String)
  def event(c: Char): Result = (this, "")
}
case object NeutralState extends TokenState {
  override def event(c: Char): Result = c match {
    case ' ' => (WhiteSpaceState(c.toString), "")
    case _ => (WordState(c.toString), "")
  }
}
case class WhiteSpaceState(candidate: String) extends TokenState {
  override def event(c: Char): Result = c match {
    case ' ' => (WhiteSpaceState(candidate :+ c), "")
    case _ => (WordState(c.toString), candidate)
  }
}
case class WordState(candidate: String) extends TokenState {
  override def event(c: Char): Result = c match {
    case ' ' => (WhiteSpaceState(c.toString), candidate)
    case _ => (WordState(candidate :+ c), "")
  }
}

object TokenState {
  type TokenReadState[A] = State[TokenState, A]

  def readOne(c: Char): TokenReadState[String] = State[TokenState, String] {
    case NeutralState => NeutralState.event(c)
    case ws: WhiteSpaceState => ws.event(c)
    case w: WordState => w.event(c)
  }
}
