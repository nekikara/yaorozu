package yaorozu.query.sql

case class Lexer(sql: String) {
  private var i: Int = 0
  private var state: TokenState = NeutralState
  private def isOver: Boolean = sql.length <= i
  private def increment(): Unit = {i = i + 1}

  def char(): Char = sql(i)

  def listTokens(): List[Token] = {
    var l = List.empty[Token]
    var t = nextToken()
    while (t.isDefined && !isOver) {
      l = l :+ t.get
      t = nextToken()
    }
    state.flush() match {
      case None => l
      case Some(last) => l :+ last
    }
  }

  def nextToken(): Option[Token] = {
    var tokenResult: Option[Token] = None
    while (!(isOver || tokenResult.isDefined)) {
      val (s, r) = TokenReader.readOne(char(), state)
      state = s
      tokenResult = r
      increment()
    }
    if (isOver) {
      tokenResult = state.flush()
    }
    tokenResult
  }
}

