object ConcatDependent {
  sealed trait HList {
    dependent def ::(head: Any): HList = new ::(head, this)
  }
  dependent case class ::(head: Any, tail: HList) extends HList
  dependent case object HNil extends HList

  dependent def concat(l1: HList, l2: HList): HList =
    if (l1.isInstanceOf[::]) {
      val head = l1.asInstanceOf[::].head
      val tail = l1.asInstanceOf[::].tail

      head :: concat(tail, l2)
    } else {
      l2
    }

  object Bench {
    def main(args: Array[String]): Unit = {
      def l1: {
        0 :: //X
        -1 ::
        HNil
      } = ???

      dependent def l2 = concat(l1, l1)

      def l3: {
        0 :: //X
        -1 ::
        0 :: //X
        -1 ::
        HNil
      } = l2
    }
  }
}
