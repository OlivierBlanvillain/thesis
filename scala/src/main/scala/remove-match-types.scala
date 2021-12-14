object RemoveMatchTypes {
  sealed trait HList
  final case class ::[+H, +T <: HList](head: H, tail: T) extends HList
  case object HNil extends HList
  type HNil = HNil.type

  type Remove[From <: HList, X] <: HList = From match {
    case head :: tail => head match {
      case X => tail
      case _ => head :: Remove[tail, X]
    }
    case HNil => HNil
  }

  def remove[L1 <: HList, X <: Singleton]
    (l2: L1, x: X)
    : Remove[L1, X] = ???

  object Bench {
    def main(args: Array[String]): Unit = {
      val l1:
        0 :: //X
        -1 ::
        HNil = ???

      val l2 = remove(l1, -1)

      val l3:
        0 :: //X
        HNil = l2
    }
  }
}

