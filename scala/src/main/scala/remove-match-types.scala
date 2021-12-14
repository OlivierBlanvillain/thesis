object RemoveMatchTypes {
  sealed trait HList
  final case class ::[+H, +T <: HList](head: H, tail: T) extends HList
  case object HNil extends HList
  type HNil = HNil.type

  type Remove[From <: HList, X] <: HList = From match {
    case HNil => HNil
    case head :: tail => head match {
      case X => Remove[tail, X]
      case _ => head :: Remove[tail, X]
    }
  }

  def remove[X <: Singleton, L1 <: HList]
    (x: X, l2: L1)
    : Remove[L1, X] = ???

  object Bench {
    val l1:
      0 :: //X
      42 ::
      HNil = ???

    val l2 = remove(42, l1)

    val l3:
      0 :: //X
      HNil = l2
  }

}

