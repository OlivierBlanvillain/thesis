import java.lang.{IllegalArgumentException => IAE}

object NumpyMatchTypes {
  sealed trait Shape {
    dependent def ::(head: Int): Shape = new ::(head, this)
  }
  dependent case class ::(head: Int, tail: Shape) extends Shape
  dependent case object HNil extends Shape

  dependent def reduceAxes(s: Shape, axes: Shape): Shape =
    loop(s, axes, 0)

  dependent def loop(s: Shape, axes: Shape, i: Int): Shape =
    if (s.isInstanceOf[::]) {
      val head = s.asInstanceOf[::].head
      val tail = s.asInstanceOf[::].tail
      if (contains(axes, i))
        loop(tail, remove(axes, i), i + 1)
      else
        head :: loop(tail, axes, i + 1)
    } else {
      if (axes.isInstanceOf[HNil.type])
        HNil
      else
        throw new IAE("Unexpected empty axes")
    }

  dependent def remove(s: Shape, i: Int): Shape =
    if (s.isInstanceOf[::]) {
      val head = s.asInstanceOf[::].head
      val tail = s.asInstanceOf[::].tail
      if (i == head) tail
      else head :: remove(tail, i)
    } else {
      throw new IAE(s"$i not found in $s")
    }

  dependent def contains(s: Shape, i: Int): Boolean =
    if (s.isInstanceOf[::]) {
      val head = s.asInstanceOf[::].head
      val tail = s.asInstanceOf[::].tail
      if (i == head) true
      else contains(tail, i)
    } else {
      false
    }

  object Bench {
    def s: {
      0 :: //X
      HNil
    } = ???

    dependent def out = reduceAxes(s, s)

    def result: { HNil } = out
  }
}
