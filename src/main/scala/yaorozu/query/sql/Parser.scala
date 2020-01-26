package yaorozu.query.sql

case class Parser(lexer: Lexer) {
  private var lookahead: Option[String] = lexer.nextToken()

  def parse(): AST = readCreate()

  def readCreate(): AST = {
    suit("CREATE") match {
      case None => throw new RuntimeException(s"Expecting CREATE; Found ${lookahead.get}")
      case _ => true
    }
    readCreateSubCommand() match {
      case None => throw new RuntimeException(s"Expecting DATABASE or SCHEMA; Found ${lookahead.get}")
      case _ => true
    }
    val confirmationOption = readIfNotExists() match {
      case Some(x) => x
      case _ => false
    }
    val name = readWord()
    CreateDataBaseNode(name, confirmationOption)
  }

  def readCreateSubCommand(): Option[Boolean] =
    suit("DATABASE") match {
      case None => suit("SCHEMA") match {
        case None => None
        case Some(x) => Some(x)
      }
      case Some(x) => Some(x)
    }

  def readIfNotExists(): Option[Boolean] =
    for {
      i <- suit("IF")
      n <- suit("NOT")
      e <- suit("EXISTS")
    } yield i && n && e

  def readWord(): String = {
    lookahead match {
      case None => throw new RuntimeException(s"Expecting some variables; Found None")
      case Some(x) => x
    }
  }

  def suit(check: String): Option[Boolean] = {
    lookahead match {
      case Some(x) if x.toUpperCase == check => {
        consume()
        Some(true)
      }
      case _ => None
    }
  }
  def consume(): Unit = { lookahead = lexer.nextToken() }
}
