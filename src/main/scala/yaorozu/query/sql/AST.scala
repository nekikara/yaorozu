package yaorozu.query.sql

sealed trait AST
case class CreateDatabaseNode(word: Word, existsCheck: Boolean = false) extends AST
case class CreateTableNode(word: Word, columns: List[ColumnNode]) extends AST
case class ColumnNode(word: Word, datatype: DataTypeNode) extends AST
case class DataTypeNode(word: Word) extends AST
case class UseNode(w: Word) extends AST
case class Nope() extends AST

object Builder {
  def sql(lexer: Lexer): Either[String, AST] = {
    val token = lexer.nextToken()
    token match {
      case Create => create(lexer)
      case Use => use(lexer)
      case _ => Left(s"Nothing to read $token")
    }
  }

  def create(lexer: Lexer): Either[String, AST] = {
    val token = lexer.nextToken()
    token match {
      case Database | Schema => database(lexer)
      case Table => table(lexer)
      case _ => Left(s"Expecting DATABASE or SCHEMA; Found ${token}")
    }
  }

  def use(lexer: Lexer): Either[String, AST] = {
    for {
      w <- word(lexer)
    } yield UseNode(w)
  }

  def database(lexer: Lexer): Either[String, AST] = {
    ifNotExists(lexer) match {
      case Right(x) =>
        for {
          w <- word(lexer)
        } yield CreateDatabaseNode(w, existsCheck = x)
      case Left(error) => Left(error)
    }
  }

  def table(lexer: Lexer): Either[String, AST] =
    for {
      w <- word(lexer)
      columns <- columns(lexer)
    } yield CreateTableNode(w, columns)

  def ifNotExists(lexer: Lexer): Either[String, Boolean] = {
    val ifT :: noT :: rest = lexer.nextNToken(3)
    val eT = rest.head
    val backCount = List(ifT, noT, eT).count(_ != End)
    val i = suit(ifT, If)
    val n = suit(noT, Not)
    val e = suit(eT, Exists)
    val result = (i, n, e) match {
      case (Some(true), None, None) => Left(s"Expecting Not but $noT")
      case (Some(true), Some(true), None) => Left(s"Expecting Not but $eT")
      case (Some(true), Some(true), Some(true)) => Right(true)
      case (None, _, _) => Right(false)
      case _ => Left(s"Something wrong")
    }
    result match {
      case Right(true) => Right(true)
      case anything =>
        lexer.backN(backCount)
        anything
    }
  }

  def columns(lexer: Lexer): Either[String, List[ColumnNode]] = {
    val result = for {
      _ <- suit(lexer.nextToken(), L_Parenthesis)
      column <- column(lexer)
      _ <- suit(lexer.nextToken(), R_Parenthesis)
    } yield List(column)
    result match {
      case Some(c) => Right(c)
      case None => Left("Expecting Columns")
    }
  }

  def column(lexer: Lexer): Option[ColumnNode] =
    for {
      cName1 <- word(lexer)
      cType1 <- word(lexer)
    } yield {
      val dType1 = DataTypeNode(cType1)
      ColumnNode(cName1, dType1)
    }

  def word(lexer: Lexer): Either[String, Word] = {
    val token = lexer.nextToken()
    token match {
      case Word(x) => Right(Word(x))
      case _ => Left(s"Expecting a word token, not $token")
    }
  }

  def suit(lookahead: Token, check: Token): Option[Boolean] = {
    lookahead match {
      case x if x == check => Some(true)
      case _ => None
    }
  }
}
