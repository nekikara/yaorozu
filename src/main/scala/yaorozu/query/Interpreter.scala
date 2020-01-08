package yaorozu.query

import scala.annotation.tailrec

object Interpreter {
  def run(query: String): Any = {
    @tailrec
    def compile(index: Int, state: GrammarState, stack: List[Any]): List[Any] = {
      if (query.length <= index) {
        stack
      } else {
        val (token, nextIndex) = Tokenizer.next(query, index)
        val (nextState, code, b) = state.read(token)
        val nextStack = if (b) code :: stack else stack
        compile(nextIndex, nextState, nextStack)
      }
    }
    val codes = compile(0, ReturnState(), List.empty[Any])
    codes.head
  }
}

sealed trait GrammarState {
  def read(t: CypToken): (GrammarState, Any, Boolean)
}
case class ReturnState() extends GrammarState {
  override def read(t: CypToken): (GrammarState, Any, Boolean) = {
    t.value.toUpperCase match {
      case "RETURN" => (LiteralState(), 1, false)
    }
  }
}
case class LiteralState() extends GrammarState {
  override def read(t: CypToken): (GrammarState, Any, Boolean) = {
    val toInteger = """(\d)""".r
    val toList = """^(\[.*])$""".r
    t.value match {
      case toInteger(_) => (ReturnState(), t.value.toInt, true)
      case toList(_) => (ReturnState(), t.value.filter(c => c.isDigit).map(_.asDigit).toList, true)
    }
  }
}
