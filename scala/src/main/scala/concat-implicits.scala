object ConcatImplicits {
  sealed trait HList
  final case class ::[+H, +T <: HList](head: H, tail: T) extends HList
  case object HNil extends HList
  type HNil = HNil.type

  trait Concat[X <: HList, Y <: HList] { type Out <: HList }

  object Concat {
    type Aux[X <: HList, Y <: HList, O <: HList] =
      Concat[X, Y] { type Out = O }

    val instance: Nothing = (new Concat {}).asInstanceOf

    implicit def caseNil[Y <: HList]: Aux[HNil, Y, Y] = instance

    implicit def caseCons[Xh, Xt <: HList, Y <: HList, O <: HList]
      (implicit ev: Aux[Xt, Y, O])
      : Aux[Xh :: Xt, Y, Xh :: O] = instance
  }

  def concat[L1 <: HList, L2 <: HList]
    (l1: L1, l2: L2)
    (implicit ev: Concat[L1, L2])
    : ev.Out = ???

  object Bench {
    val l1:
      0 :: //X
      42 ::
      HNil = ???

    val l2 = concat(l1, l1)

    val l3:
      0 :: //X
      42 ::
      0 :: //X
      42 ::
      HNil = l2
  }
}
