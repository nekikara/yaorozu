package yaorozu.db

case class DatabaseTable() {
  private var databases: List[Database] = List.empty[Database]

  def add(db: Database): Unit = {
    databases = databases :+ db
  }
}

