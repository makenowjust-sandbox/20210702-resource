package codes.quine.sandbox.resource

sealed abstract class Type

object Type {
  case object Bool extends Type
  final case class Pair(t1: Type, t2: Type) extends Type
  final case class List(t: Type) extends Type
}
