package foo
import scala.util.NotGiven

sealed trait HList
sealed trait HNil extends HList
case object HNil extends HNil
case class ::[+H, +T <: HList](head: H, tail: T) extends HList

// start section memImplicitHlistOps
trait Remove[V, Ps <: HList, O <: HList]
object Remove {
  implicit def casehead[V, Ps <: HList, O <: HList]: Remove[V, V :: Ps, Ps] = new Remove {}
  implicit def casetail[V, Ph, Pt <: HList, O <: HList]
    (implicit ev: Remove[V, Pt, O]): Remove[V, Ph :: Pt, Ph :: O] = new Remove {}
}

trait NotIn[V, Ps <: HList]
object NotIn {
  implicit def casenil[V]: NotIn[V, HNil] = new NotIn {}
  implicit def casecons[Ph, Pt <: HList, V]
    (implicit
      no: NotGiven[V =:= Ph],
      xs: NotIn[V, Pt]
    ): NotIn[V, Ph :: Pt] = new NotIn {}
}
// end section memImplicitHlistOps

// start section memImplicitContext
trait Context[Ps <: HList] {
  def malloc[V <: Singleton]
    (size: Int, v: V)
    (implicit ev: NotIn[V, Ps])
    : Context[V :: Ps] = ???

  def free[V <: Singleton, Out <: HList]
    (v: V)
    (implicit ev: Remove[V, Ps, Out])
    : Context[Out] = ???

  def call[Out <: HList]
    (f: Context[Ps] => Context[Out])
    : Context[Out] = ???

  def deref[V <: Singleton]
    (v: V)
    (body: Array[Byte] => Unit)
    (implicit ev: Remove[V, Ps, ?])
    : Context[Ps] = ???
}
// end section memImplicitContext

// start section memImplicitFunction
def f[Ps <: HList](ctx: Context["bool" :: Ps]): Context["bool" :: Ps] =
  ctx.deref("bool") { b =>
    println(if (b(0) == 0) false else true)
  }
// end section memImplicitFunction

// start section memImplicitMain
def main(ctx: Context[HNil]): Context[HNil] =
  ctx.malloc(32, "mem")
     .malloc(1, "bool")
     .call(f)
     .deref("mem") { (m: Array[Byte]) =>
       println(new String(m))
     }
     .free("mem")
     .free("bool")
// end section memImplicitMain
