package yaorozu.query.sql

import org.scalatest._
import yaorozu.db.DatabaseTable

class ParserSpec extends FunSuite with DiagrammedAssertions {
  test("Parser should not throw a RuntimeError") {
    val lexer = Lexer("CREATE DATABASE    \n test")
    val parser = Parser(lexer)
    try {
      val ast = parser.readCreate()
      ast.execute()
      println(DatabaseTable.list())
    } catch {
      case ex: RuntimeException => fail(s"Should not call here : $ex")
    }
  }
}
