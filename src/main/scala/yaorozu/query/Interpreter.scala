package yaorozu.query

import scala.annotation.tailrec

object Interpreter {
  def run(query: String): Any = {
    @tailrec
    def compile(q: String, state: GrammarState): Code = {
      if (q.isEmpty) {
        Code(ReturnE(1))
      } else {
        val token = Tokenizer.next(q)
        state.read(token)
        compile(q.tail, state)
      }
    }
    val code = compile(query, ReturnState())
    code.run()
  }
}

case class ReturnE(literal: Any)

case class Code(returnE: ReturnE) {
  def run(): Any = returnE.literal
}

sealed trait GrammarState {
  def read(t: CypToken): (GrammarState, Any)
}
case class ReturnState() extends GrammarState {
  override def read(t: CypToken): (GrammarState, Any) = {
    t.value.toUpperCase match {
      case "RETURN" => (LiteralState(), 1)
    }
  }
}
case class LiteralState() extends GrammarState {
  override def read(t: CypToken): (GrammarState, Any) = {
    val toInteger = """(\d)""".r
    t.value match {
      case toInteger(_) => (ReturnState(), t.value.toInt)
    }
  }
}
