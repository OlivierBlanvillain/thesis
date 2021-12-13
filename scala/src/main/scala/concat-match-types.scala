object ConcatMatchTypes {
  sealed trait HList
  final case class ::[+H, +T <: HList](head: H, tail: T) extends HList
  case object HNil extends HList
  type HNil = HNil.type

  type Concat[X <: HList, Y <: HList] = X match {
    case HNil => Y
    case h :: t => h :: Concat[t, Y]
  }

  def concat[L1 <: HList, L2 <: HList]
    (l1: L1, l2: L2)
    : Concat[L1, L2] = ???

  object Bench {
    val l1:
      1 :: //X
      42 ::
      HNil = ???

    val l2:
      2 :: //X
      42 ::
      HNil = ???

    val l3 = concat(l1, l2)

    val l4:
      1 :: //X
      42 ::
      2 :: //X
      42 ::
      HNil = l3
  }
}
