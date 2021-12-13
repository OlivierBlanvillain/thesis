import java.lang.{IllegalArgumentException => IAE}

object NumpyMatchTypes {
  sealed trait Shape
  dependent case class Cons(head: Int, tail: Shape) extends Shape
  dependent case class HNil() extends Shape

  object Impl {
    dependent def reduceAxes(s: Shape, axes: Shape): Shape =
      loop(s, axes, 0)

    dependent def loop(s: Shape, axes: Shape, i: Int): Shape =
      if (s.isInstanceOf[Cons]) {
        val head = s.asInstanceOf[Cons].head
        val tail = s.asInstanceOf[Cons].tail
        if (contains(axes, i))
          loop(tail, remove(axes, i), i + 1)
        else
          Cons(head, loop(tail, axes, i + 1))
      } else {
        if (axes.isInstanceOf[HNil])
          HNil()
        else
          throw new IAE("Unexpected empty axes")
      }

    dependent def remove(s: Shape, e: Any): Shape =
      if (s.isInstanceOf[Cons]) {
        val head = s.asInstanceOf[Cons].head
        val tail = s.asInstanceOf[Cons].tail
      // case Cons(head, tail) =>
        if (e.equals(head)) tail
        else Cons(head, remove(tail, e))
      } else {
        throw new IAE(s"$e not found in $s")
      }

    dependent def contains(s: Shape, e: Any): Boolean =
      if (s.isInstanceOf[Cons]) {
        val head = s.asInstanceOf[Cons].head
        val tail = s.asInstanceOf[Cons].tail
      // case Cons(head, tail) =>
        if (e.equals(head)) true
        else contains(tail, e)
      } else {
        false
      }
  }

}
