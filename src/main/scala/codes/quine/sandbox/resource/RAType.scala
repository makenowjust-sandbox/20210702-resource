package codes.quine.sandbox.resource

/** Resource annotated type.
  *
  * P is a type represents potential.
  */
sealed abstract class RAType[+P]

object RAType {
  case object Unit extends RAType[Nothing]
  case object Bool extends RAType[Nothing]
  final case class Pair[P](r1: RAType[P], r2: RAType[P]) extends RAType[P]
  final case class List[P](p: Poly[P], r: RAType[P]) extends RAType[P]
}
