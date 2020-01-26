package yaorozu.query.sql

import yaorozu.db.{ Database, DatabaseTable }

sealed trait AST {
  def execute(): Unit
}
case class CreateDataBaseNode(name: String, existsCheck: Boolean = false) extends AST {
  override def execute(): Unit =
    if (existsCheck)
      DatabaseTable.addIfNotExists(Database(name))
    else
      DatabaseTable.add(Database(name))
}
case class UseNode(name: String) extends AST {
  override def execute(): Unit = DatabaseTable.changeCurrent(name)
}
