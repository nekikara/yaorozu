package yaorozu.query

sealed abstract class EntityType
case class Node() extends EntityType
case class Relationship() extends EntityType

sealed abstract class TokenType
case class Label() extends TokenType
case class RelationshipType() extends TokenType
case class PropertyKey() extends TokenType

case class Path()
case class Property()
