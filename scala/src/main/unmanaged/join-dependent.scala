import java.lang.{IllegalArgumentException => IAE}

object JoinDependent {
  sealed trait Lst {
    dependent def ::(head: Int): Lst = new ::(head, this)
  }
  dependent case class ::(head: Int, tail: Lst) extends Lst
  dependent case object HNil extends Lst

  dependent def concat(l1: Lst, l2: Lst): Lst =
    if (l1.isInstanceOf[::]) {
      val head = l1.asInstanceOf[::].head
      val tail = l1.asInstanceOf[::].tail
      head :: concat(tail, l2)
    } else {
      l2
    }

  dependent def remove(l: Lst, e: Int): Lst =
    if (l.isInstanceOf[::]) {
      val head = l.asInstanceOf[::].head
      val tail = l.asInstanceOf[::].tail
      if (e == head) tail
      else head :: remove(tail, e)
    } else {
      throw new IAE(s"$e not found in $l")
    }

  dependent def join(schema1: Lst, schema2: Lst, key: Int): Lst = {
    val s1 = remove(schema1, key)
    val s2 = remove(schema2, key)
    key :: concat(s1, s2)
  }

  object Bench {
    def main(args: Array[String]): Unit = {
      def l1: {
        0 :: //X
        -1 :: HNil
      } = ???

      dependent def joined = join(l1, l1, -1)

      def results: {
        -1 ::
        0 :: //X
        0 :: //X
        HNil
      } = joined
    }
  }
}
