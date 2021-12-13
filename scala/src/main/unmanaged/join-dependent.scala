import java.lang.{IllegalArgumentException => IAE}

object JoinDependent {
  sealed trait Lst
  dependent case class Cons(head: Int, tail: Lst) extends Lst
  dependent case object Nil extends Lst

  dependent def concat(l1: Lst, l2: Lst): Lst =
    if (l1.isInstanceOf[Cons]) {
      val head = l1.asInstanceOf[Cons].head
      val tail = l1.asInstanceOf[Cons].tail
      Cons(head, concat(tail, l2))
    } else {
      l2
    }

  dependent def remove(l: Lst, e: Int): Lst =
    if (l.isInstanceOf[Cons]) {
      val head = l.asInstanceOf[Cons].head
      val tail = l.asInstanceOf[Cons].tail
      if (e == head) tail
      else Cons(head, remove(tail, e))
    } else {
      throw new IAE(s"$e not found in $l")
    }

  dependent def join(schema1: Lst, schema2: Lst, key: Int): Lst = {
    val s1 = remove(schema1, key)
    val s2 = remove(schema2, key)
    Cons(key, concat(s1, s2))
  }

  object Bench {
    def l1: {
      Cons(1, //X
        Cons(42, Nil)
      ) //X
    } = ???

    dependent def joined = join(l1, l1, 42)

    def results: {
      Cons(42,
        Cons(1, //X
          Cons(1, //X
            Nil
          )
        )
      )
    } = joined
  }
}
