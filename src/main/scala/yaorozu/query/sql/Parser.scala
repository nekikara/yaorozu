package yaorozu.query.sql

case class Parser(lexer: Lexer) {
  private var lookahead: Option[Token] = lexer.nextToken()

  def parse(): AST = {
    val result = lookup() match {
      case Some(clause) => readClause(clause)
      case None => readClause(End)
      case _ => Left("Can't found a first token")
    }
    result match {
      case Left(err) => throw new RuntimeException(err)
      case Right(ast) => ast
    }
  }

  def readClause(token: Token): Either[String, AST] = token match {
    case Create => readCreate()
    case Use => readUse()
    case End => readNope()
    case _ => Left("Nothing to read")
  }

  def readCreate(): Either[String, AST] = readCreateSubCommand()

  def readNope(): Either[String, AST] = Right(Nope())

  def readUse(): Either[String, AST] =
    for {
      word <- readWord()
    } yield UseNode(word)

  def readCreateSubCommand(): Either[String, AST] =
    lookup() match {
      case Some(Database) | Some(Schema) =>
        for {
          confirmationOption <- readIfNotExists()
          word <- readWord()
        } yield CreateDataBaseNode(word, confirmationOption)
      case Some(Table) => Right(Nope())
      case _ => Left(s"Expecting DATABASE or SCHEMA; Found ${lookahead.get}")
    }

  def readIfNotExists(): Either[String, Boolean] = {
    val option = for {
      i <- suit(If)
      n <- suit(Not)
      e <- suit(Exists)
    } yield i && n && e
    option match {
      case Some(true) => Right(true)
      case _ => Right(true)
    }
  }

  def readWord(): Either[String, Word] = {
    lookahead match {
      case Some(Word(x)) => Right(Word(x))
      case _ => Left(s"Expecting some variables; Found None")
    }
  }

  def lookup(): Option[Token] = {
    lookahead match {
      case Some(x)  =>
        consume()
        Some(x)
      case _ => None
    }
  }

  def suit(check: Token): Option[Boolean] = {
    lookahead match {
      case Some(x) if x == check =>
        consume()
        Some(true)
      case _ => None
    }
  }
  def consume(): Unit = { lookahead = lexer.nextToken() }
}
