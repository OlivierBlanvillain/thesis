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

  def remove[L1 <: HList, X <: Singleton]
    (l1: L1, x: X)
    (implicit ev: Remove[L1, X])
    : ev.Out = ???

  object Bench {
    def main(args: Array[String]): Unit = {
      val s1:
        0 :: //X
        -1 ::
        HNil = ???

      val s2 = remove(s1, -1)

      val s3:
        0 :: //X
        HNil = s2
    }
  }
}
