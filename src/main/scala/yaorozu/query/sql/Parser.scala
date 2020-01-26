package yaorozu.query.sql

case class Parser(lexer: Lexer) {
  private var lookahead: Option[String] = lexer.nextToken()

  def parse(): AST = {
    val result = lookup() match {
      case Some("CREATE") => readCreate()
      case Some("USE") => readUse()
      case _ => Left("Can't found a first token")
    }
    result match {
      case Left(err) => throw new RuntimeException(err)
      case Right(ast) => ast
    }
  }

  def readCreate(): Either[String, AST] =
    for {
      _ <- readCreateSubCommand()
      confirmationOption <- readIfNotExists()
      name <- readWord()
    } yield CreateDataBaseNode(name, confirmationOption)

  def readUse(): Either[String, AST] =
    for {
      name <- readWord()
    } yield UseNode(name)

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
