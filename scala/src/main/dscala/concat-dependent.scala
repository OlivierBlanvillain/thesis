object ConcatDependent {
  sealed trait HList
  dependent case class Cons(head: Any, tail: HList) extends HList
  dependent case object Nil extends HList

  dependent def concat(l1: HList, l2: HList): HList =
    if (l1.isInstanceOf[Cons]) {
      val head = l1.asInstanceOf[Cons].head
      val tail = l1.asInstanceOf[Cons].tail
      Cons(head, concat(tail, l2))
    } else {
      l2
    }

  object Bench {
    def l1: {
      Cons(1, //X
        Cons(42,
          Nil
        )
      )
    } = ???

    def l2: {
      Cons(2, //X
        Cons(42,
          Nil
        )
      )
    } = ???

    dependent def l3 = concat(l1, l2)

    def result: {
      Cons(1, //X
        Cons(42,
          Cons(2, //X
            Cons(42,
              Nil
            )
          ) //X
        )
      ) //X
    } = l3
  }
}
