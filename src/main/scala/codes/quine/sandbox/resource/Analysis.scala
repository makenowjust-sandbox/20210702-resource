package codes.quine.sandbox.resource

import scala.collection.mutable

import codes.quine.sandbox.resource.Analysis.Result
import codes.quine.sandbox.resource.Expr._

object Analysis {
  val quad = MatchList(
    "l",
    Nil(Type.Bool),
    "x",
    "xs",
    Let(
      "_",
      Tick(1),
      Share(
        "xs",
        "xs1",
        "xs2",
        Let(
          "l0",
          Nil(Type.Bool),
          Let(
            "p1",
            Pair("xs1", "l0"),
            Let(
              "l1",
              App("append", "p1"),
              Let(
                "l2",
                App("quad", "xs2"),
                Let("p2", Pair("l1", "l2"), Let("l3", App("append", "p2"), Cons("x", "l3")))
              )
            )
          )
        )
      )
    )
  )
  val append = Func(
    RAType.Pair(RAType.List(Vector(1, 0), RAType.Bool), RAType.List(Vector(0, 0), RAType.Bool)),
    RAType.List(Vector(0, 0), RAType.Bool),
    0,
    0
  )
  val fns = Map("append" -> append)

  val analysis = new Analysis(
    2,
    "quad",
    "l",
    Type.List(Type.Bool),
    Type.List(Type.Bool),
    quad,
    fns
  )

  final case class Result(q1: Name, q2: Name, r: RAType[Name])
}

class Analysis(k: Int, f: Name, x: Name, t1: Type, t2: Type, e: Expr, fns: Map[Name, Func[Int]]) {
  val r1 = annotate(t1)
  val r2 = annotate(t2)
  val q1 = allocateVar("q")
  val q2 = allocateVar("q")

  def analyze(): Unit = {
    val Result(q1, q2, r2) = analyze(Map(x -> r1), e)
    // constraint("analyze", s"${this.q1} >= $q1")
    // constraint("analyze", s"${this.q1} - ($q1 - $q2) >= ${this.q2}")
    // unify(r2, this.r2, ">=")

    def collectVars(r: RAType[Name]): Seq[Name] = r match {
      case RAType.Unit         => Seq.empty
      case RAType.Bool         => Seq.empty
      case RAType.Pair(r1, r2) => collectVars(r1) ++ collectVars(r2)
      case RAType.List(p, r)   => p ++ collectVars(r)
    }

    val vs = Seq(this.q1, this.q2) ++ collectVars(this.r1) ++ collectVars(this.r2)

    for (v <- vars) println(v)
    println()
    println(s"minimize analyze: ${vs.mkString(" + ")};")
    println()
    for (c <- constraints) println(c)
    println()
    println("solve;")
    println()
    println("printf \"\\n\";")
    for (v <- vs) {
      println("printf \"" + v + " = %d\\n\", " + v + ";")
    }
    println("printf \"\\n\";")
    println("end;")
  }

  def analyze(env: Map[Name, RAType[Name]], e: Expr): Result = e match {
    case Expr.Lit(_) =>
      val q1 = allocateVar("q")
      val q2 = allocateVar("q")
      constraint("lit", s"$q1 >= $q2")
      Result(q1, q2, RAType.Bool)
    case Expr.Var(x) =>
      val q1 = allocateVar("q")
      val q2 = allocateVar("q")
      constraint("var", s"$q1 >= $q2")
      Result(q1, q2, env(x))
    case Expr.App(f, x) if f == this.f =>
      val q1 = allocateVar("q")
      val q2 = allocateVar("q")
      constraint("rec", s"$q1 >= ${this.q1}")
      constraint("rec", s"$q1 - (${this.q1} - ${this.q2}) >= ${q2}")
      val r1 = env(x)
      unify(r1, this.r1, ">=")
      Result(q1, q2, this.r2)
    case Expr.App("append", x) =>
      val r1 = env(x).asInstanceOf[RAType.Pair[Name]]
      val r2 = r1.r1.asInstanceOf[RAType.List[Name]]
      val r3 = r1.r2.asInstanceOf[RAType.List[Name]]
      val p0 = allocateVar("p")
      constraint("append", s"${r2.p(0)} >= ${p0} + 1")
      val q1 = allocateVar("q")
      val q2 = allocateVar("q")
      constraint("append", s"$q1 >= $q2")
      Result(q1, q2, RAType.List(r2.p.updated(0, p0), r2.r))
    case Expr.App(f, x) =>
      val Func(r1, r2, q1, q2) = fns(f)
      val r3 = env(x)
      val q3 = allocateVar("q")
      val q4 = allocateVar("q")
      constraint("app", s"$q3 >= $q1")
      constraint("app", s"$q3 - ($q1 - $q2) >= $q4")
      unify(r3, r1, ">=")
      Result(q3, q4, clone(r2))
    case Expr.Tick(d) =>
      val q1 = allocateVar("q")
      val q2 = allocateVar("q")
      constraint("tick", s"$q1 >= $d")
      constraint("tick", s"$q1 - $d >= $q2")
      Result(q1, q2, RAType.Unit)
    case Expr.Let(x, e1, e2) =>
      val Result(q1, q2, r1) = analyze(env, e1)
      val Result(q3, q4, r2) = analyze(env ++ Map(x -> r1), e2)
      constraint("let", s"$q2 >= $q3")
      Result(q1, q4, r2)
    case Expr.Share(x1, x2, x3, e) =>
      val r = env(x1)
      val (r1, r2) = share(r)
      analyze(env ++ Map(x2 -> r1, x3 -> r2), e)
    case Expr.Nil(t) =>
      val q1 = allocateVar("q")
      val q2 = allocateVar("q")
      constraint("nil", s"$q1 >= $q2")
      val ps = Vector.fill(k)(allocateVar("p"))
      Result(q1, q2, RAType.List(ps, annotate(t)))
    case Expr.Cons(x1, x2) =>
      val q1 = allocateVar("q")
      val q2 = allocateVar("q")
      val r1 = env(x1)
      val r2 = env(x2).asInstanceOf[RAType.List[Name]]
      val ps = Vector.fill(k)(allocateVar("p"))
      for ((p, i) <- ps.zipWithIndex) {
        if (i + 1 == ps.size) constraint("cons", s"${r2.p(i)} >= $p")
        else constraint("cons", s"${r2.p(i)} >= $p + ${ps(i + 1)}")
      }
      constraint("cons", s"$q1 >= $q2 + ${ps(0)}")
      unify(r1, r2.r, "==")
      Result(q1, q2, RAType.List(ps, r1))
    case Expr.Pair(x1, x2) =>
      val q1 = allocateVar("q")
      val q2 = allocateVar("q")
      val r1 = env(x1)
      val r2 = env(x2)
      constraint("pair", s"$q1 >= $q2")
      Result(q1, q2, RAType.Pair(r1, r2))
    case Expr.MatchList(x1, e1, x2, x3, e2) =>
      val r1 = env(x1).asInstanceOf[RAType.List[Name]]
      val Result(q1, q2, r2) = analyze(env, e1)
      val ps = Vector.fill(k)(allocateVar("p"))
      for ((p, i) <- r1.p.zipWithIndex) {
        if (i + 1 == r1.p.size) constraint("match_list", s"$p >= ${ps(i)}")
        else constraint("match_list", s"$p + ${r1.p(i + 1)} >= ${ps(i)}")
      }
      val Result(q3, q4, r3) = analyze(env ++ Map(x2 -> r1.r, x3 -> RAType.List(ps, r1.r)), e2)
      val q5 = allocateVar("q")
      val q6 = allocateVar("q")
      constraint("match_list", s"$q5 >= $q1")
      constraint("match_list", s"$q5 + ${r1.p(0)} >= $q3")
      constraint("match_list", s"$q5 - ($q1 - $q2) >= $q6")
      constraint("match_list", s"$q5 + ${r1.p(0)} - ($q3 - $q4) >= $q6")
      unify(r2, r3, "==")
      Result(q5, q6, r3)
    case Expr.MatchPair(x1, x2, x3, e) =>
      val r = env(x1).asInstanceOf[RAType.Pair[Name]]
      analyze(env ++ Map(x2 -> r.r1, x3 -> r.r2), e)
  }

  def share(r: RAType[Name]): (RAType[Name], RAType[Name]) = r match {
    case RAType.Unit => (RAType.Unit, RAType.Unit)
    case RAType.Bool => (RAType.Bool, RAType.Bool)
    case RAType.Pair(r1, r2) =>
      val (r11, r12) = share(r1)
      val (r21, r22) = share(r2)
      (RAType.Pair(r11, r21), RAType.Pair(r12, r22))
    case RAType.List(p, r) =>
      val ps1 = Vector.fill(k)(allocateVar("p"))
      val ps2 = Vector.fill(k)(allocateVar("p"))
      for ((p, i) <- p.zipWithIndex) {
        constraint("share", s"$p >= ${ps1(i)} + ${ps2(i)}")
      }
      val (r1, r2) = share(r)
      (RAType.List(ps1, r1), RAType.List(ps2, r2))
  }

  def unify[A, B](r1: RAType[A], r2: RAType[B], op: String): Unit = (r1, r2) match {
    case (RAType.Unit, RAType.Unit) => ()
    case (RAType.Bool, RAType.Bool) => ()
    case (RAType.List(p1, r1), RAType.List(p2, r2)) =>
      for ((p1, p2) <- p1.zip(p2)) {
        constraint("unify", s"$p1 $op $p2")
      }
      unify(r1, r2, op)
    case (RAType.Pair(r11, r12), RAType.Pair(r21, r22)) =>
      unify(r11, r21, op)
      unify(r12, r22, op)
  }

  def clone(r: RAType[Int]): RAType[Name] = r match {
    case RAType.Unit         => RAType.Unit
    case RAType.Bool         => RAType.Bool
    case RAType.Pair(r1, r2) => RAType.Pair(clone(r1), clone(r2))
    case RAType.List(p, r) =>
      val ps = Vector.fill(k)(allocateVar("p"))
      for ((p1, p2) <- p.zip(ps)) {
        constraint("clone", s"$p1 >= $p2")
      }
      RAType.List(ps, clone(r))
  }

  def annotate(t: Type): RAType[Name] = t match {
    case Type.Unit         => RAType.Unit
    case Type.Bool         => RAType.Bool
    case Type.Pair(t1, t2) => RAType.Pair(annotate(t1), annotate(t2))
    case Type.List(t)      => RAType.List(Vector.fill(k)(allocateVar("p")), annotate(t))
  }

  lazy val ids: mutable.Map[Name, Int] = mutable.Map.empty.withDefaultValue(1)

  def allocateName(prefix: Name): Name = {
    val id = ids(prefix)
    ids(prefix) = id + 1
    s"$prefix$id"
  }

  lazy val vars: mutable.Buffer[String] = mutable.Buffer.empty

  def allocateVar(prefix: Name): Name = {
    val name = allocateName(prefix)
    vars.append(s"var $name >= 0, integer;")
    name
  }

  lazy val constraints: mutable.Buffer[String] = mutable.Buffer.empty

  def constraint(name: String, cond: String): Unit = {
    constraints.append(s"s.t. ${allocateName(name)}: $cond;")
  }
}
