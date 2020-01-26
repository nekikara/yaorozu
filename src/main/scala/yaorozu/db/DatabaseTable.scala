package yaorozu.db

object DatabaseTable {
  private var databases: List[Database] = List.empty[Database]
  private var current: Option[Database] = None

  def add(db: Database): Unit = {
    databases = databases :+ db
  }

  def addIfNotExists(db: Database): Unit = {
    if (!list().exists(_.name == db.name)) {
      add(db)
    }
  }

  def list(): List[Database] = databases

  def find(name: String): Either[String, Database] = list().find(_.name == name) match {
    case None => Left(s"Not found the database $name")
    case Some(db) => Right(db)
  }

  def changeCurrent(name: String): Unit = {
    DatabaseTable.find(name) match {
      case Left(error) => throw new RuntimeException(error)
      case Right(db) => {
        current = Some(db)
      }
    }
  }
}

