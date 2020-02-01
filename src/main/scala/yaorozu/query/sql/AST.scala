package yaorozu.query.sql

sealed trait AST
case class CreateDataBaseNode(word: Word, existsCheck: Boolean = false) extends AST
case class UseNode(w: Word) extends AST
case class Nope() extends AST
