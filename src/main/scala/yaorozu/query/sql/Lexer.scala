package yaorozu.query.sql

case class Lexer(sql: String) {
  private var i: Int = 0
  private var state: TokenState = NeutralState
  private def isOver: Boolean = sql.length <= i
  private def increment(): Unit = {i = i + 1}

  def char(): Char = sql(i)
  def listTokens(): List[String] = {
    var l = List.empty[String]
    var t = nextToken()
    while (t.isDefined) {
      l = l :+ t.get
      t = nextToken()
    }
    state.flush() match {
      case None => l
      case Some(last) => l :+ last
    }
  }

  def nextToken(): Option[String] = {
    var tokenResult: Option[String] = None
    while (!(isOver || tokenResult.isDefined)) {
      val (s, r) = TokenReader.readOne(char(), state)
      state = s
      tokenResult = r
      increment()
    }
    tokenResult
  }
}

