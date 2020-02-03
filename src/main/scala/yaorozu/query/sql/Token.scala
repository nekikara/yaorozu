package yaorozu.query.sql

sealed trait Token
case object Create extends Token
case object Database extends Token
case object Schema extends Token
case object Table extends Token
case object Use extends Token
case object If extends Token
case object Not extends Token
case object Exists extends Token
case object L_Parenthesis extends Token
case object R_Parenthesis extends Token
case class Word(name: String) extends Token
case object End extends Token

object Token {
  private val symbols = Map(
    "CREATE" -> Create,
    "DATABASE" -> Database,
    "SCHEMA" -> Schema,
    "TABLE" -> Table,
    "USE" -> Use,
    "IF" -> If,
    "NOT" -> Not,
    "(" -> L_Parenthesis,
    ")" -> R_Parenthesis,
    "EXISTS" -> Exists
  )

  def mapWord(word: String): Token =
    mapToSymbol(word.toUpperCase) match {
      case None => Word(word)
      case Some(token) => token
    }

  def mapToSymbol(word: String): Option[Token] = symbols.get(word)
}
