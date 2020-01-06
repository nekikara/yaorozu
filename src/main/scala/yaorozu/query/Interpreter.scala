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

sealed trait GrammarState
case class ReturnState() extends GrammarState
