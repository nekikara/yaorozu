package yaorozu.query.sql

case class Parser(lexer: Lexer) {
  private var lookahead: Option[String] = lexer.nextToken()

  def parse(): AST = lookup() match {
    case Some("CREATE") => readCreate() match {
      case Left(error) => throw new RuntimeException(error)
      case Right(ast) => ast
    }
    case _ => throw new RuntimeException("Can't found a first token")
  }

  def readCreate(): Either[String, AST] = {
    for {
      _ <- readCreateSubCommand()
      confirmationOption <- readIfNotExists()
      name <- readWord()
    } yield CreateDataBaseNode(name, confirmationOption)
  }

  def readCreateSubCommand(): Either[String, Boolean] =
    suit("DATABASE") match {
      case None => suit("SCHEMA") match {
        case None => Left(s"Expecting DATABASE or SCHEMA; Found ${lookahead.get}")
        case Some(x) => Right(x)
      }
      case Some(x) => Right(x)
    }

  def readIfNotExists(): Either[String, Boolean] = {
    val option = for {
      i <- suit("IF")
      n <- suit("NOT")
      e <- suit("EXISTS")
    } yield i && n && e
    option match {
      case Some(true) => Right(true)
      case _ => Right(true)
    }
  }

  def readWord(): Either[String, String] = {
    lookahead match {
      case None => Left(s"Expecting some variables; Found None")
      case Some(x) => Right(x)
    }
  }

  def lookup(): Option[String] = {
    lookahead match {
      case Some(x)  => {
        consume()
        Some(x)
      }
      case _ => None
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
