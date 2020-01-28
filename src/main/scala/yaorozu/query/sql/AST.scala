package yaorozu.query.sql

import yaorozu.db.{ Database, DatabaseTable }

sealed trait AST {
  def execute(): Unit
}
case class CreateDataBaseNode(word: Word, existsCheck: Boolean = false) extends AST {
  override def execute(): Unit =
    if (existsCheck)
      DatabaseTable.addIfNotExists(Database(word.name))
    else
      DatabaseTable.add(Database(word.name))
}
case class UseNode(name: String) extends AST {
  override def execute(): Unit = DatabaseTable.changeCurrent(name)
}
case class Nope() extends AST {
  override def execute(): Unit = ()
}
