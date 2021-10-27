package bar
import scala.util.NotGiven

sealed trait HList
sealed trait HNil extends HList
case object HNil extends HNil
case class ::[+H, +T <: HList](head: H, tail: T) extends HList

// start section memMatchtypeHlistOps
type NotIn[V, Ps] <: HList = Ps match {
  case V :: t => NotIn[V, Ps]
  case h :: t => h :: NotIn[V, t]
  case HNil => HNil
}

type Remove[V, Ps] <: HList = Ps match {
  case V :: t => t
  case h :: t => h :: Remove[V, t]
}
// end section memMatchtypeHlistOps

// start section memMatchtypeContext
trait Context[Ps <: HList] {
  def malloc[V <: Singleton](size: Int, v: V): Context[V :: NotIn[V, Ps]] = ???
  def free[V <: Singleton, Out <: HList](v: V): Context[Remove[V, Ps]] = ???
  def call[Out <: HList](f: Context[Ps] => Context[Out]): Context[Out] = ???
  def deref[V <: Singleton, EV <: Remove[V, Ps]](v: V)(body: Array[Byte] => Unit)
    : Context[Ps] = ???
}
// end section memMatchtypeContext

def f[Ps <: HList](ctx: Context["bool" :: Ps]): Context["bool" :: Ps] =
  ctx.deref("bool") { b =>
    println(if (b(0) == 0) false else true)
  }

// start section memMatchtypeMain
def main(ctx: Context[HNil]): Context[HNil] =
  ctx.malloc(32, "mem")
     .malloc(1, "bool")
     .call(f)
     .deref("mem") { (m: Array[Byte]) =>
       println(new String(m))
     }
     .free("mem")
     .free("bool")
// end section memMatchtypeMain
