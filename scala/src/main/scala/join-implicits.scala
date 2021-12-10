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

object Tests {
  def the[T](implicit ev: T): ev.type = ev
  the[Concat    [1 :: 2 :: HNil, 3 :: 4 :: HNil]]
    : Concat.Aux[1 :: 2 :: HNil, 3 :: 4 :: HNil, 1 :: 2 :: 3 :: 4 :: HNil]
  the[Concat    [HNil, 3 :: 4 :: HNil]]
    : Concat.Aux[HNil, 3 :: 4 :: HNil, 3 :: 4 :: HNil]
  the[Concat    [1 :: 2 :: HNil, HNil]]
    : Concat.Aux[1 :: 2 :: HNil, HNil, 1 :: 2 :: HNil]
  the[Concat    [HNil, HNil]]
    : Concat.Aux[HNil, HNil, HNil]

  the[Remove[1, 1 :: 2 :: 3 :: 4 :: HNil]]
    : Remove.Aux[1, 1 :: 2 :: 3 :: 4 :: HNil, 2 :: 3 :: 4 :: HNil]
  the[Remove[2, 1 :: 2 :: 3 :: 4 :: HNil]]
    : Remove.Aux[2, 1 :: 2 :: 3 :: 4 :: HNil, 1 :: 3 :: 4 :: HNil]
  the[Remove[3, 1 :: 2 :: 3 :: 4 :: HNil]]
    : Remove.Aux[3, 1 :: 2 :: 3 :: 4 :: HNil, 1 :: 2 :: 4 :: HNil]
  the[Remove[4, 1 :: 2 :: 3 :: 4 :: HNil]]
    : Remove.Aux[4, 1 :: 2 :: 3 :: 4 :: HNil, 1 :: 2 :: 3 :: HNil]

  the[Join[2, 1 :: 2 :: 3 :: HNil, 4 :: 2 :: 5 :: HNil]]
    : Join[2, 1 :: 2 :: 3 :: HNil, 4 :: 2 :: 5 :: HNil] { type Out = 2 :: 1 :: 3 :: 4 :: 5 :: HNil }
  the[Join[2, 1 :: 3 :: 2 :: HNil, 2 :: 4 :: 5 :: HNil]]
    : Join[2, 1 :: 3 :: 2 :: HNil, 2 :: 4 :: 5 :: HNil] { type Out = 2 :: 1 :: 3 :: 4 :: 5 :: HNil }
  the[Join[1, 1 :: HNil, 1 :: HNil]]
    : Join[1, 1 :: HNil, 1 :: HNil] { type Out = 1 :: HNil }
}

}
