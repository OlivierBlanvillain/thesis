import java.lang.{IllegalArgumentException => IAE}

object NumpyMatchTypes {
  sealed trait Shape
  dependent case class Cons(head: Int, tail: Shape) extends Shape
  dependent case object Nil extends Shape

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
      if (axes.isInstanceOf[Nil.type])
        Nil
      else
        throw new IAE("Unexpected empty axes")
    }

  dependent def remove(s: Shape, i: Int): Shape =
    if (s.isInstanceOf[Cons]) {
      val head = s.asInstanceOf[Cons].head
      val tail = s.asInstanceOf[Cons].tail
      if (i == head) tail
      else Cons(head, remove(tail, i))
    } else {
      throw new IAE(s"$i not found in $s")
    }

  dependent def contains(s: Shape, i: Int): Boolean =
    if (s.isInstanceOf[Cons]) {
      val head = s.asInstanceOf[Cons].head
      val tail = s.asInstanceOf[Cons].tail
      if (i == head) true
      else contains(tail, i)
    } else {
      false
    }

  object Bench {
    def s: {
      Cons(0, //X
        Nil
      ) //X
    } = ???

    dependent def out = reduceAxes(s, s)

    def result: { Nil } = out
  }
}
