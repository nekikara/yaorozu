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
    while (!t.contains(End)) {
      l = l :+ t.get
      t = nextToken()
    }
    l
  }

  def nextToken(): Option[Token] = {
    state match {
      case EndState => EndState.flush()
      case _ if isOver => {
        val x = state.flush()
        state = EndState
        x
      }
      case _ => {
        var tokenResult: Option[Token] = None
        while (!(tokenResult.isDefined || isOver)) {
          val (s, r) = TokenReader.readOne(char(), state)
          state = s
          tokenResult = r
          increment()
        }
        if (tokenResult.isDefined) {
          tokenResult
        } else {
          nextToken()
        }
      }
    }
  }
}

