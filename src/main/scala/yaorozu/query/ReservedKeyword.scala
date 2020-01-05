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

  object Expression {
    val ALL = "ALL"
    val CASE = "CASE"
    val ELSE = "ELSE"
    val END = "END"
    val THEN = "THEN"
    val WHEN = "WHEN"
  }

  object Operator {
    val AND = "AND"
    val AS = "AS"
    val CONTAINS = "CONTAINS"
    val DISTINCT = "DISTINCT"
    val ENDS = "ENDS"
    val IN = "IN"
    val IS = "IS"
    val NOT = "NOT"
    val OR = "OR"
    val STARTS = "STARTS"
    val XOR = "XOR"
  }

  object Literal {
    val false = "false"
    val null = "null"
    val true = "false"
  }

  object ForFuture {
    val ADD = "ADD"
    val CONSTRAINT = "CONSTRAINT"
    val DO = "DO"
    val DROP = "DROP"
    val FOR = "FOR"
    val MANDATORY = "MANDATORY"
    val OF = "OF"
    val REQUIRE = "REQUIRE"
    val SCALAR = "SCALAR"
    val UNIQUE = "UNIQUE"
  }

}

