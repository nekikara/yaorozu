package yaorozu.query.sql

sealed trait Token
case object Create extends Token
case object Database extends Token
case object Schema extends Token
case object Use extends Token
case object If extends Token
case object Not extends Token
case object Exists extends Token
case class Word(name: String) extends Token
case object End extends Token

object Token {
  private val symbols = Map(
    "CREATE" -> Create,
    "DATABASE" -> Database,
    "SCHEMA" -> Schema,
    "USE" -> Use,
    "IF" -> If,
    "NOT" -> Not,
    "EXISTS" -> Exists
  )

  def mapWord(word: String): Token =
    mapToSymbol(word.toUpperCase) match {
      case None => Word(word)
      case Some(token) => token
    }

  def mapToSymbol(word: String): Option[Token] = symbols.get(word)
}
