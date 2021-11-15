package foo
import scala.util.NotGiven

// start section hlistEnumDefinition
enum HList:
  case HNil()
  case ::[H, T <: HList](head: H, tail: T)
// end section hlistEnumDefinition
import HList._

// sealed trait HList
// case object HNil extends HList
// case class ::[+H, +T <: HList](head: H, tail: T) extends HList
// type HNil = HNil.type

// start section memImplicitRemove
trait Remove[V, Ps <: HList, Out <: HList]

object Remove:
  implicit def casehead[V, Ps <: HList]
    : Remove[V, V :: Ps, Ps] = new Remove {}

  implicit def casetail[V, Ph, Pt <: HList, Out <: HList]
    (implicit ev: Remove[V, Pt, Out])
    : Remove[V, Ph :: Pt, Ph :: Out] = new Remove {}
// end section memImplicitRemove

trait NotIn[V, Ps <: HList]

object NotIn:
  implicit def casenil[V]: NotIn[V, HNil] = new NotIn {}
  implicit def casecons[Ph, Pt <: HList, V]
    (implicit
      no: NotGiven[V =:= Ph],
      xs: NotIn[V, Pt]
    ): NotIn[V, Ph :: Pt] = new NotIn {}

// start section memImplicitContext
trait Context[Ps <: HList]:
  def free[V <: Singleton, Out <: HList]
    (v: V)
    (implicit ev: Remove[V, Ps, Out])
    : Context[Out]
// end section memImplicitContext

  def malloc[V <: Singleton]
    (size: Int, v: V)
    (implicit ev: NotIn[V, Ps])
    : Context[V :: Ps]

  def call[Out <: HList]
    (f: Context[Ps] => Context[Out])
    : Context[Out]

  def deref[V <: Singleton]
    (v: V)
    (body: Array[Byte] => Unit)
    (implicit ev: Remove[V, Ps, ?])
    : Context[Ps]

def f[Ps <: HList](ctx: Context["bool" :: Ps]): Context["bool" :: Ps] =
  ctx.deref("bool") { b =>
    println(if (b(0) == 0) false else true)
  }

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

trait js {
// start section addEventListenerJS
/** Attaches an event handler.
 *
 *  @param event     The event type, one of "mousedown", "mouseup",
 *                   "mouseover", "mousewheel" and "contextmenu".
 *  @param listener  The function to run when the event occurs.
 */
def addEventListener(event: String, listener: Event => Unit): Unit
// end section addEventListenerJS

}
trait Event {

// start section addEventListenerImplicitDef
type EventTypes =
  "mousedown" :: "mouseup" :: "mouseover" :: "mousewheel" :: "contextmenu" :: HNil

def addEventListener[E <: Singleton]
  (event: E, listener: Event => Unit)
  (implicit ev: Remove[E, EventTypes, ?]): Unit
// end section addEventListenerImplicitDef

// start section addEventListenerImplicitCall
addEventListener("mouseover", e => ())(
  // Evidence that "mouseover" is in EventTypes,
  // automatically infered by the compiler.
  Remove.casetail(Remove.casetail(Remove.casehead))
)
// end section addEventListenerImplicitCall

}
