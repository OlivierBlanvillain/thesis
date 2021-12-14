object ConcatMatchTypes {
  sealed trait HList
  final case class ::[+H, +T <: HList](head: H, tail: T) extends HList
  case object HNil extends HList
  type HNil = HNil.type

  type Concat[X <: HList, Y <: HList] = X match {
    case h :: t => h :: Concat[t, Y]
    case HNil => Y
  }

  def concat[L1 <: HList, L2 <: HList]
    (l1: L1, l2: L2)
    : Concat[L1, L2] = ???

  object Bench {
    def main(args: Array[String]): Unit = {
      val l1:
        0 :: //X
        -1 ::
        HNil = ???

      val l2 = concat(l1, l1)

      val l3:
        0 :: //X
        -1 ::
        0 :: //X
        -1 ::
        HNil = l2
    }
  }
}
