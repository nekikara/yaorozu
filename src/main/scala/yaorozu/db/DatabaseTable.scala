package yaorozu.db

object DatabaseTable {
  private var databases: List[Database] = List.empty[Database]

  def add(db: Database): Unit = {
    databases = databases :+ db
  }

  def addIfNotExists(db: Database): Unit = {
    if (!list().exists(_.name == db.name)) {
      add(db)
    }
  }

  def list(): List[Database] = databases
}

