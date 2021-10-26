package codes.quine.sandbox.resource

final case class Func[P](t1: RAType[P], t2: RAType[P], q1: P, q2: P)
