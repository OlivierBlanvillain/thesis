import java.lang.{IllegalArgumentException => IAE}

object RemoveDependent {
  sealed trait HList
  dependent case class Cons(head: Int, tail: HList) extends HList
  dependent case object Nil extends HList

  dependent def remove(l: HList, e: Int): HList =
    if (l.isInstanceOf[Cons]) {
      val head = l.asInstanceOf[Cons].head
      val tail = l.asInstanceOf[Cons].tail
      if (e == head) tail
      else Cons(head, remove(tail, e))
    } else {
      throw new IAE(s"$e not found in $l")
    }

  object Bench {
    def l1: {
      Cons(1, //X
        Cons(42,
          Nil
        )
      )
    } = ???

    dependent def l2 = remove(l1, 42)

    def result: {
      Cons(1, //X
        Nil
      ) //X
    } = l2
  }
}
