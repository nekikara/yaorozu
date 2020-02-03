package yaorozu.query.sql

case class Lexer(sql: String) {
  private var i: Int = 0
  private var tokenIndexes: List[Int] = List.empty[Int]
  private var state: TokenState = NeutralState
  private def isOver: Boolean = sql.length <= i
  private def increment(): Unit = {i = i + 1}

  def char(): Char = sql(i)

  def listTokens(): List[Token] = {
    var l = List.empty[Token]
    var t = nextToken()
    while (t != End) {
      l = l :+ t
      t = nextToken()
    }
    l
  }

  def nextToken(): Token = {
    state match {
      case EndState => EndState.flush().get
      case _ if isOver => {
        val x = state.flush()
        state = EndState
        x.get
      }
      case _ =>
        var tokenResult: Option[Token] = None
        while (!(tokenResult.isDefined || isOver)) {
          val (s, r) = TokenReader.readOne(char(), state)
          state = s
          tokenResult = r
          increment()
        }
        if (tokenResult.isDefined) {
          tokenIndexes = tokenIndexes :+ i
          tokenResult.get
        } else {
          nextToken()
        }
    }
  }

  def nextNToken(n: Int): List[Token] = (0 until n).map(_ => nextToken()).toList

  def back(): Unit = {
    val init = tokenIndexes.init
    i = tokenIndexes.last
    tokenIndexes = init
    state = NeutralState
  }

  def backN(n: Int): Unit = (0 until n).foreach(x =>  back() )
}

