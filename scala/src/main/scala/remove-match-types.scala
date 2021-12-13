object RemoveMatchTypes {
  sealed trait HList
  final case class ::[+H, +T <: HList](head: H, tail: T) extends HList
  case object HNil extends HList
  type HNil = HNil.type

  type Remove[X, From <: HList] <: HList = From match {
    case HNil => HNil
    case head :: tail => head match {
      case X => Remove[X, tail]
      case _ => head :: Remove[X, tail]
    }
  }

  def remove[X <: Singleton, L1 <: HList]
    (x: X, l2: L1)
    : Remove[X, L1] = ???

  object Bench {
    val l1:
      1 :: //X
      42 ::
      HNil = ???

    val l2 = remove(42, l1)

    val l3:
      1 :: //X
      HNil = l2
  }

}

