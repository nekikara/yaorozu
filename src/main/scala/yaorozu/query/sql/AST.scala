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
case class UseNode(w: Word) extends AST {
  override def execute(): Unit = DatabaseTable.changeCurrent(w.name)
}
case class Nope() extends AST {
  override def execute(): Unit = ()
}
