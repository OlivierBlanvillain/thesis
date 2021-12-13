object RemoveImplicits {
  sealed trait HList
  final case class ::[+H, +T <: HList](head: H, tail: T) extends HList
  case object HNil extends HList
  type HNil = HNil.type

  trait Remove[V, L <: HList] { type Out <: HList }

  object Remove {
    type Aux[V, L <: HList, O <: HList] =
      Remove[V, L] { type Out = O }

    val instance: Nothing = (new Remove {}).asInstanceOf

    implicit def caseMatch[V, Lt <: HList]: Aux[V, V :: Lt, Lt] = instance

    implicit def caseCons[V, Lh, Lt <: HList, O <: HList]
      (implicit ev: Aux[V, Lt, O])
      : Aux[V, Lh :: Lt, Lh :: O] = instance
  }

  def remove[X <: Singleton, L1 <: HList]
    (x: X, l1: L1)
    (implicit ev: Remove[X, L1])
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
