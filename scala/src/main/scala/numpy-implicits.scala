object NumpyImplicits {

sealed trait Shape
final case class #:[H <: Int, T <: Shape](head: H, tail: T) extends Shape
case object Ø extends Shape

type Ø = Ø.type
type None = None.type

trait Contains[Haystack <: Shape, Needle <: Int] { type Out <: Boolean }

object Contains {
  type Aux[Haystack <: Shape, Needle <: Int, O <: Boolean] =
    Contains[Haystack, Needle] { type Out = O }

  val instance: Nothing = (new Contains {}).asInstanceOf

  implicit def caseNil[Needle <: Int]
    : Aux[Ø, Needle, false] = instance

  implicit def caseMatch[HayT <: Shape, Needle <: Int]
    : Aux[Needle #: HayT, Needle, true] = instance

  implicit def caseCons[HayH <: Int, HayT <: Shape, Needle <: Int, O <: Boolean]
    (implicit ev: Aux[HayT, Needle, O])
    : Aux[HayH #: HayT, Needle, O] = instance
}

trait Remove[From <: Shape, Value <: Int] { type Out <: Shape }

object Remove {
  val instance: Nothing = (new Remove {}).asInstanceOf

  type Aux[From <: Shape, Value <: Int, O <: Shape] =
    Remove[From, Value] { type Out = O }

  implicit def caseNil[Value <: Int]
    : Aux[Ø, Value, Ø] = instance

  implicit def caseMatch[FromT <: Shape, Value <: Int]
    : Aux[Value #: FromT, Value, FromT] = instance

  implicit def caseCons[FromH <: Int, FromT <: Shape, Value <: Int, O <: Shape]
    (implicit ev: Aux[FromT, Value, O])
    : Aux[FromH #: FromT, Value, FromH #: O] = instance
}

trait ReduceAxes[S <: Shape, Axes <: None | Shape] { type Out <: Shape }

object ReduceAxes {
  val instance: Nothing = (new ReduceAxes {}).asInstanceOf

  type Aux[S <: Shape, Axes <: None | Shape, O <: Shape] =
    ReduceAxes[S, Axes] { type Out = O }

  implicit def caseNone[S <: Shape]: Aux[S, None, Ø] = instance

  implicit def caseShape[S <: Shape, Axes <: Shape, O <: Shape]
    (implicit ev: Loop.Aux[S, Axes, 0, O]): Aux[S, Axes, O] = instance
}

trait Loop[S <: Shape, Axes <: Shape, I <: Int] { type Out <: Shape }

object Loop {
  val instance: Nothing = (new Loop {}).asInstanceOf

  type Aux[S <: Shape, Axes <: Shape, I <: Int, O <: Shape] =
    Loop[S, Axes, I] { type Out = O }

  implicit def caseNil[I <: Int]: Aux[Ø, Ø, I, Ø] = instance

  import scala.compiletime.ops.int.{S => Succ}
  implicit def caseConsTrue[Sh <: Int, St <: Shape, Axes <: Shape, I <: Int, RemoveOut <: Shape, Out <: Shape]
    (implicit
      co: Contains.Aux[Axes, I, true],
      rm: Remove.Aux[Axes, I, RemoveOut],
      lo: Aux[St, RemoveOut, Succ[I], Out]
    ): Aux[Sh #: St, Axes, I, Out] = instance

  implicit def caseConsFalse[Sh <: Int, St <: Shape, Axes <: Shape, I <: Int, RemoveOut <: Shape, Out <: Shape]
    (implicit
      co: Contains.Aux[Axes, I, false],
      lo: Aux[St, Axes, Succ[I], Out]
    ): Aux[Sh #: St, Axes, I, Sh #: Out] = instance
}

object Test {
  def the[T](implicit ev: T): ev.type = ev
  the[Contains[1 #: 2 #: 3 #: Ø, 0]]: Contains.Aux[1 #: 2 #: 3 #: Ø, 0, false]
  the[Contains[1 #: 2 #: 3 #: Ø, 1]]: Contains.Aux[1 #: 2 #: 3 #: Ø, 1, true]
  the[Contains[1 #: 2 #: 3 #: Ø, 2]]: Contains.Aux[1 #: 2 #: 3 #: Ø, 2, true]
  the[Contains[1 #: 2 #: 3 #: Ø, 3]]: Contains.Aux[1 #: 2 #: 3 #: Ø, 3, true]
  the[Contains[1 #: 2 #: 3 #: Ø, 4]]: Contains.Aux[1 #: 2 #: 3 #: Ø, 4, false]

  the[Remove[1 #: 2 #: 3 #: Ø, 1]]: Remove.Aux[1 #: 2 #: 3 #: Ø, 1, 2 #: 3 #: Ø]
  the[Remove[1 #: 2 #: 3 #: Ø, 2]]: Remove.Aux[1 #: 2 #: 3 #: Ø, 2, 1 #: 3 #: Ø]
  the[Remove[1 #: 2 #: 3 #: Ø, 3]]: Remove.Aux[1 #: 2 #: 3 #: Ø, 3, 1 #: 2 #: Ø]

  the[ReduceAxes[1 #: 2 #: 3 #: Ø, None]]: ReduceAxes.Aux[1 #: 2 #: 3 #: Ø, None, Ø]

  the[ReduceAxes    [25 #: 256 #: 256 #: 3 #: Ø, 0 #: 1 #: 2 #: Ø]]
    : ReduceAxes.Aux[25 #: 256 #: 256 #: 3 #: Ø, 0 #: 1 #: 2 #: Ø, 3 #: Ø]

  the[ReduceAxes    [25 #: 256 #: 256 #: 3 #: Ø, 0 #: 1 #: 3 #: Ø]]
    : ReduceAxes.Aux[25 #: 256 #: 256 #: 3 #: Ø, 0 #: 1 #: 3 #: Ø, 256 #: Ø]

  the[ReduceAxes    [25 #: 256 #: 256 #: 3 #: Ø, 2 #: 3 #: Ø]]
    : ReduceAxes.Aux[25 #: 256 #: 256 #: 3 #: Ø, 2 #: 3 #: Ø, 25 #: 256 #: Ø]
}

}
