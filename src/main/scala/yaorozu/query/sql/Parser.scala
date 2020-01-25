package yaorozu.query.sql

case class Parser(lexer: Lexer) {
  private var lookahead: Option[String] = lexer.nextToken()

  def readCreate(): AST = {
    suit("CREATE")
    readDatabase()
    CreateDataBaseNode(lookahead.get)
  }
  def readDatabase() = {
    suit("DATABASE")
  }
  def readWord() = {
    lookahead match {
      case None => throw new RuntimeException(s"Expecting some variables; Found None")
      case Some(x) => println(x)
    }
  }

  def suit(check: String): Unit = {
    lookahead match {
      case Some(x) if x.toUpperCase == check => consume()
      case _ => throw new RuntimeException(s"Expecting ${check}; Found ${lookahead}")
    }
  }
  def consume(): Unit = { lookahead = lexer.nextToken() }
}
