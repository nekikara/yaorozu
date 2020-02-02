package yaorozu.query.sql

case class Parser(lexer: Lexer) {
  def parse(): AST = {
    Builder.sql(lexer) match {
      case Right(ast) => ast
      case Left(err) => throw new RuntimeException(err)
    }
  }
}
