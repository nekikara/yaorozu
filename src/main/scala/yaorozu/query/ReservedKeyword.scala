package yaorozu.query

object ReservedKeyword {
  object Clause {
    val CREATE = "CREATE"
    val DELETE = "DELETE"
    val DETACH = "DETACH"
    val EXISTS = "EXISTS"
    val MATCH = "MATCH"
    val MERGE = "MERGE"
    val OPTIONAL = "OPTIONAL"
    val REMOVE = "REMOVE"
    val RETURN = "RETURN"
    val SET = "SET"
    val UNION = "UNION"
    val UNWIND = "UNWIND"
    val WITH = "WITH"
  }

  object Subclause {
    val LIMIT = "LIMIT"
    val ORDER = "ORDER"
    val SKIP = "SKIP"
    val WHERE = "WHERE"
  }

  object Modifier {
    val ASC = "ASC"
    val ASCENDING = "ASCENDING"
    val BY = "BY"
    val DESC = "DESC"
    val DESCENDING = "DESCENDING"
    val ON = "ON"
  }
}

