package yaorozu.query.sql

import yaorozu.db.{Database, DatabaseTable}

sealed trait AST {
  def execute(): Unit
}
case class CreateDataBaseNode(name: String) extends AST {
  override def execute(): Unit = {
    DatabaseTable.add(Database(name))
  }
}
