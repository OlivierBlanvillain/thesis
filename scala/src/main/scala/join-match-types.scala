object JoinMatchTypes {
  sealed trait HList
  final case class ::[+H, +T <: HList](head: H, tail: T) extends HList
  case object HNil extends HList
  type HNil = HNil.type

  type Concat[X <: HList, Y <: HList] <: HList = X match {
    case h :: t => h :: Concat[t, Y]
    case HNil => Y
  }

  type Remove[From <: HList, X] <: HList = From match {
    case head :: tail => head match {
      case X => tail
      case _ => head :: Remove[tail, X]
    }
    case HNil => HNil
  }

  type Join[L1 <: HList, L2 <: HList, X] =
    X :: Concat[Remove[L1, X], Remove[L2, X]]

  def join[S1 <: HList, S2 <: HList, X <: Singleton]
    (s1: S1, s2: S2, x: X)
    : Join[S1, S2, X] = ???

  object Bench {
    def main(args: Array[String]): Unit = {
      val s1:
        0 :: //X
        -1 ::
        HNil = ???

      join(s1, s1, -1):
        -1 ::
        0 :: //X
        0 :: //X
        HNil
    }
  }
}
