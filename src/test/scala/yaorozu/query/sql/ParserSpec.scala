package yaorozu.query.sql

import org.scalatest._
import yaorozu.db.{Database, DatabaseTable}

class ParserSpec extends FunSuite with DiagrammedAssertions {
  test("Parser should not throw a RuntimeError") {
    val lexer = Lexer("CREATE DATABASE    \n test")
    val parser = Parser(lexer)
    val ast = parser.parse()
    ast.execute()
    assert(DatabaseTable.list() == List(Database("test")))
  }
  test("Parser should not try to create an existed database with if-not-exits") {
    assert(DatabaseTable.list() == List(Database("test")))
    val lexer = Lexer("CREATE SCHEMA IF not exists    \n test")
    val parser = Parser(lexer)
    val ast = parser.parse()
    ast.execute()
    assert(DatabaseTable.list() == List(Database("test")))
  }
  test("Parser should create a new database if it does not exits") {
    assert(DatabaseTable.list() == List(Database("test")))
    val lexer = Lexer("CREATE SCHEMA IF not exists    \n test2")
    val parser = Parser(lexer)
    val ast = parser.parse()
    ast.execute()
    assert(DatabaseTable.list() == List(Database("test"), Database("test2")))
  }
}
