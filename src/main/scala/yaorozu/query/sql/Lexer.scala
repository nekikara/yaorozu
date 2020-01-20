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
      if (lexer.isOver) {
        stock
      } else {
        val c = lexer.char()
        c match {
          case _ if isWS(c) => {
            val (wsrest, newLexer) = ws(c, lexer.increment())
            loop(stock :+ wsrest, newLexer)
          }
          case _ => {
            val (wd, newLexer) = word(c, lexer.increment())
            loop(stock :+ wd, newLexer)
          }
        }

      }
    }
    val lexer = Lexer(sql)
    loop(List.empty[String], lexer)
  }

  def ws(start: Char, lexer: Lexer): (String, Lexer) = {
    @tailrec
    def loop(str: String, lex: Lexer): (String, Lexer) = {
      if (lex.isOver) (str, lex)
      else {
        val c = lex.char()
        c match {
          case _ if isWS(c) => loop(str :+ c, lex.increment())
          case _ => (str, lex)
        }
      }
    }
    loop(start.toString, lexer)
  }

  def word(start: Char, lexer: Lexer): (String, Lexer) = {
    @tailrec
    def loop(str: String, lex: Lexer): (String, Lexer) = {
      if (lex.isOver) (str, lex)
      else {
        val c = lex.char()
        c match {
          case _ if isWord(c) => loop(str :+ c, lex.increment())
          case _ => (str, lex)
        }
      }
    }
    loop(start.toString, lexer)
  }

  def isWS(c: Char): Boolean = c == ' ' || c == '\t' || c == '\n' || c == '\r'
  def isWord(c: Char): Boolean = ('a' <= c  && c <= 'z') || ('A' <= c && c <= 'Z')
}
