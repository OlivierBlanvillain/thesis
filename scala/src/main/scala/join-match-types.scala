object JoinMatchTypes {
  sealed trait HList
  final case class ::[+H, +T <: HList](head: H, tail: T) extends HList
  case object HNil extends HList
  type HNil = HNil.type

  type Concat[X <: HList, Y <: HList] <: HList = X match {
    case HNil => Y
    case h :: t => h :: Concat[t, Y]
  }


  type Remove[From <: HList, X] <: HList = From match {
    case HNil => HNil
    case head :: tail => head match {
      case X => Remove[tail, X]
      case _ => head :: Remove[tail, X]
    }
  }

  type Join[L1 <: HList, L2 <: HList, X] =
    X :: Concat[Remove[L1, X], Remove[L2, X]]

  def join[S1 <: HList, S2 <: HList, X <: Singleton]
    (s1: S1, s2: S2, x: X)
    : Join[S1, S2, X] = ???

  object Bench {
    val s1:
      0 :: //X
      42 ::
      HNil = ???

    join(s1, s1, 42):
      42 ::
      0 :: //X
      0 :: //X
      HNil
  }
}
