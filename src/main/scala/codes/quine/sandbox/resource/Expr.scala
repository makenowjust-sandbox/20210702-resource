package codes.quine.sandbox.resource

sealed abstract class Expr

object Expr {

  /** `true` or `false` */
  final case class Lit(v: Boolean) extends Expr

  /** `x` */
  final case class Var(x: Name) extends Expr

  /** `f(x)` */
  final case class App(f: Name, x: Name) extends Expr

  /** `tick(v)` */
  final case class Tick(d: Int) extends Expr

  /** `let x = e1 in e2` */
  final case class Let(x: Name, e1: Expr, e2: Expr) extends Expr

  /** `share (x2, x3) = x in e` */
  final case class Share(x1: Name, x2: Name, x3: Name, e: Expr) extends Expr

  /** `[]` */
  final case class Nil(t: Type) extends Expr

  /** `x1 :: x2` */
  final case class Cons(x1: Name, x2: Name) extends Expr

  /** `(x1, x2)` */
  final case class Pair(x1: Name, x2: Name) extends Expr

  /** {{{
    * match x1 with
    * | []       -> e1
    * | x2 :: x2 -> e2
    * }}}
    */
  final case class MatchList(x1: Name, e1: Expr, x2: Name, x3: Name, e2: Expr) extends Expr

  /** {{{
    * match x1 with
    * | (x2, x3) -> e
    * }}}
    */
  final case class MatchPair(x1: Name, x2: Name, x3: Name, e: Expr) extends Expr
}
