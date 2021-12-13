object JoinImplicits {
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

  trait Join[V, X <: HList, Y <: HList] { type Out <: HList }

  object Join {
    implicit def instance[V, X <: HList, Y <: HList, Rx <: HList, Ry <: HList, Rxy <: HList]
      (implicit
        i1: Remove.Aux[V, X, Rx],
        i2: Remove.Aux[V, Y, Ry],
        i3: Concat.Aux[Rx, Ry, Rxy]
      ): Join[V, X, Y] { type Out = V :: Rxy }
      = new Join[V, X, Y] { type Out = V :: Rxy }
  }

  object Bench {
    def the[T](implicit ev: T): ev.type = ev
    type S1 =
      1 :: //X
      42 ::
      HNil

    type S2 =
      2 :: //X
      42 ::
      HNil

    the[Join[42, S1, S2]]: Join[42, S1, S2] { type Out =
      42 ::
      1 :: //X
      2 :: //X
      HNil
    }
  }
}
