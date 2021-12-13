object RemoveImplicits {
  sealed trait HList
  final case class ::[+H, +T <: HList](head: H, tail: T) extends HList
  case object HNil extends HList
  type HNil = HNil.type

  trait Remove[L <: HList, V] { type Out <: HList }

  object Remove {
    type Aux[L <: HList, V, O <: HList] =
      Remove[L, V] { type Out = O }

    val instance: Nothing = (new Remove {}).asInstanceOf

    implicit def caseMatch[Lt <: HList, V]: Aux[V :: Lt, V, Lt] = instance

    implicit def caseCons[Lh, Lt <: HList, V, O <: HList]
      (implicit ev: Aux[Lt, V, O])
      : Aux[Lh :: Lt, V, Lh :: O] = instance
  }

  def remove[X <: Singleton, L1 <: HList]
    (x: X, l1: L1)
    (implicit ev: Remove[L1, X])
    : ev.Out = ???

  object Bench {
    val s1:
      1 :: //X
      42 ::
      HNil = ???

    val s2 = remove(42, s1)

    val s3:
      1 :: //X
      HNil = s2
  }
}
