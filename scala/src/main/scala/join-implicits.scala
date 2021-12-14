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

  trait Join[X <: HList, Y <: HList, V] { type Out <: HList }

  object Join {
    implicit def instance[X <: HList, Y <: HList, V, Rx <: HList, Ry <: HList, Rxy <: HList]
      (implicit
        i1: Remove.Aux[X, V, Rx],
        i2: Remove.Aux[Y, V, Ry],
        i3: Concat.Aux[Rx, Ry, Rxy]
      ): Join[X, Y, V] { type Out = V :: Rxy }
      = new Join[X, Y, V] { type Out = V :: Rxy }
  }

  def join[S1 <: HList, S2 <: HList, X <: Singleton]
    (s1: S1, s2: S2, x: X)
    (implicit ev: Join[S1, S2, X])
    : ev.Out = ???

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
