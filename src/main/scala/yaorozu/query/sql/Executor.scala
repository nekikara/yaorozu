package yaorozu.query.sql

sealed trait ExecuteResult[T] {
  val successful: Boolean
}
case class Success[T](result: T) extends ExecuteResult[T] {
  override val successful: Boolean = true
}

object Executor {
  def execute(ast: AST): ExecuteResult = ast match {
    case CreateDataBaseNode(word, existsCheck) => Success[T]("hogehoe")
  }
}

