package yaorozu.query.sql

import scala.annotation.tailrec

case class Lexer(sql: String, i: Int = 0) {
  val isOver: Boolean = sql.length <= i
  def char(): Char = sql(i)
  def increment(): Lexer = Lexer(sql, i + 1)
}

object Lexer {
  def listTokens(sql: String): List[String] = {
    @tailrec
    def loop(stock: List[String], lexer: Lexer): List[String] = {
      if (lexer.isOver) stock
      else {
        lexer.char() match {
          case ' ' | '\t' | '\n' | '\r' =>


        }

        loop(stock, lexer.increment())
      }
    }
    val lexer = Lexer(sql)
    loop(List.empty[String], lexer)
  }

  def ws(lexer: Lexer): (Lexer, String) = {
    

  }
}
