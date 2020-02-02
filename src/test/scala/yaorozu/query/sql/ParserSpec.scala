package yaorozu.query.sql

import org.scalatest._
import yaorozu.db.{Database, DatabaseTable}

class ParserSpec extends FunSuite with DiagrammedAssertions {
  test("Parser should not throw a RuntimeError") {
    val lexer = Lexer("CREATE DATABASE     \n test")
    val parser = Parser(lexer)
    val ast = parser.parse()
    val expect = CreateDatabaseNode(Word("test"))
    assert(ast == expect)
  }
  test("Parser should not try to create an existed database with if-not-exits") {
    DatabaseTable.clear()
    DatabaseTable.add(Database("test"))
    assert(DatabaseTable.list() == List(Database("test")))
    val lexer = Lexer("CREATE SCHEMA IF not exists    \n test")
    val parser = Parser(lexer)
    val ast = parser.parse()
    val expect = CreateDatabaseNode(Word("test"), existsCheck = true)
    assert(ast == expect)
  }
  test("Parser should create a new database if it does not exits") {
    DatabaseTable.clear()
    DatabaseTable.add(Database("test"))
    assert(DatabaseTable.list() == List(Database("test")))
    val lexer = Lexer("CREATE SCHEMA IF not exists    \n test2")
    val parser = Parser(lexer)
    val ast = parser.parse()
    val expect = CreateDatabaseNode(Word("test2"), existsCheck = true)
    assert(ast == expect)
  }
  test("Parser should set the current database by using USE clause") {
    DatabaseTable.clear()
    DatabaseTable.add(Database("test"))
    val lexer = Lexer("use   test")
    val parser = Parser(lexer)
    val ast = parser.parse()
    val expect = UseNode(Word("test"))
    assert(ast == expect)
  }
}
