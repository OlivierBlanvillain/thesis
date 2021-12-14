import java.lang.{IllegalArgumentException => IAE}

object RemoveDependent {
  sealed trait HList {
    dependent def ::(head: Int): HList = new ::(head, this)
  }
  dependent case class ::(head: Int, tail: HList) extends HList
  dependent case object HNil extends HList

  dependent def remove(l: HList, e: Int): HList =
    if (l.isInstanceOf[::]) {
      val head = l.asInstanceOf[::].head
      val tail = l.asInstanceOf[::].tail
      if (e == head) tail
      else head :: remove(tail, e)
    } else {
      throw new IAE(s"$e not found in $l")
    }

  object Bench {
    def main(args: Array[String]): Unit = {
      def l1: {
        0 :: //X
        -1 ::
        HNil
      } = ???

      dependent def l2 = remove(l1, -1)

      def result: {
        0 :: //X
        HNil
      } = l2
    }
  }
}
