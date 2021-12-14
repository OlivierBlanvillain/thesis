object JoinImplicitsTests {
  import JoinImplicits._
  def the[T](implicit ev: T): ev.type = ev
  the[Concat    [1 :: 2 :: HNil, 3 :: 4 :: HNil]]
    : Concat.Aux[1 :: 2 :: HNil, 3 :: 4 :: HNil, 1 :: 2 :: 3 :: 4 :: HNil]
  the[Concat    [HNil, 3 :: 4 :: HNil]]
    : Concat.Aux[HNil, 3 :: 4 :: HNil, 3 :: 4 :: HNil]
  the[Concat    [1 :: 2 :: HNil, HNil]]
    : Concat.Aux[1 :: 2 :: HNil, HNil, 1 :: 2 :: HNil]
  the[Concat    [HNil, HNil]]
    : Concat.Aux[HNil, HNil, HNil]

  the[Remove[1 :: 2 :: 3 :: 4 :: HNil, 1]]
    : Remove.Aux[1 :: 2 :: 3 :: 4 :: HNil, 1, 2 :: 3 :: 4 :: HNil]
  the[Remove[1 :: 2 :: 3 :: 4 :: HNil, 2]]
    : Remove.Aux[1 :: 2 :: 3 :: 4 :: HNil, 2, 1 :: 3 :: 4 :: HNil]
  the[Remove[1 :: 2 :: 3 :: 4 :: HNil, 3]]
    : Remove.Aux[1 :: 2 :: 3 :: 4 :: HNil, 3, 1 :: 2 :: 4 :: HNil]
  the[Remove[1 :: 2 :: 3 :: 4 :: HNil, 4]]
    : Remove.Aux[1 :: 2 :: 3 :: 4 :: HNil, 4, 1 :: 2 :: 3 :: HNil]

  the[Join[1 :: 2 :: 3 :: HNil, 4 :: 2 :: 5 :: HNil, 2]]
    : Join[1 :: 2 :: 3 :: HNil, 4 :: 2 :: 5 :: HNil, 2] { type Out = 2 :: 1 :: 3 :: 4 :: 5 :: HNil }
  the[Join[1 :: 3 :: 2 :: HNil, 2 :: 4 :: 5 :: HNil, 2]]
    : Join[1 :: 3 :: 2 :: HNil, 2 :: 4 :: 5 :: HNil, 2] { type Out = 2 :: 1 :: 3 :: 4 :: 5 :: HNil }
  the[Join[1 :: HNil, 1 :: HNil, 1]]
    : Join[1 :: HNil, 1 :: HNil, 1] { type Out = 1 :: HNil }
}

object NumpyImplicitsTests {
  import NumpyImplicits._
  def the[T](implicit ev: T): ev.type = ev
  the[Remove[1 :: 2 :: 3 :: Ø, 1]]: Remove.Aux[1 :: 2 :: 3 :: Ø, 1, 2 :: 3 :: Ø]
  the[Remove[1 :: 2 :: 3 :: Ø, 2]]: Remove.Aux[1 :: 2 :: 3 :: Ø, 2, 1 :: 3 :: Ø]
  the[Remove[1 :: 2 :: 3 :: Ø, 3]]: Remove.Aux[1 :: 2 :: 3 :: Ø, 3, 1 :: 2 :: Ø]

  the[ReduceAxes[1 :: 2 :: 3 :: Ø, None]]: ReduceAxes.Aux[1 :: 2 :: 3 :: Ø, None, Ø]

  the[ReduceAxes    [25 :: 256 :: 256 :: 3 :: Ø, 0 :: 1 :: 2 :: Ø]]
    : ReduceAxes.Aux[25 :: 256 :: 256 :: 3 :: Ø, 0 :: 1 :: 2 :: Ø, 3 :: Ø]

  the[ReduceAxes    [25 :: 256 :: 256 :: 3 :: Ø, 0 :: 1 :: 3 :: Ø]]
    : ReduceAxes.Aux[25 :: 256 :: 256 :: 3 :: Ø, 0 :: 1 :: 3 :: Ø, 256 :: Ø]

  the[ReduceAxes    [25 :: 256 :: 256 :: 3 :: Ø, 2 :: 3 :: Ø]]
    : ReduceAxes.Aux[25 :: 256 :: 256 :: 3 :: Ø, 2 :: 3 :: Ø, 25 :: 256 :: Ø]
}
