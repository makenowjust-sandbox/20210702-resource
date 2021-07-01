package codes.quine.sandbox.resource

sealed abstract class RAType

object RAType {
  case object Bool extends RAType
  final case class Pair(t1: RAType, t2: RAType) extends RAType
  final case class List(p: Poly, t: RAType) extends RAType
}
