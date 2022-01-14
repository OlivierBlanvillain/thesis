import scala.compiletime.ops.int.{S => Succ}

object NumpyImplicits {
  sealed trait Shape
  final case class ::[+H <: Int, +T <: Shape](head: H, tail: T) extends Shape
  case object Ø extends Shape

  type Ø = Ø.type
  type None = None.type

  trait Remove[From <: Shape, Value <: Int] { type Out <: Shape }

  object Remove {
    val instance: Nothing = (new Remove {}).asInstanceOf

    type Aux[From <: Shape, Value <: Int, O <: Shape] =
      Remove[From, Value] { type Out = O }

    implicit def caseNil[Value <: Int]
      : Aux[Ø, Value, Ø] = instance

    implicit def caseMatch[FromT <: Shape, Value <: Int]
      : Aux[Value :: FromT, Value, FromT] = instance

    implicit def caseCons[FromH <: Int, FromT <: Shape, Value <: Int, O <: Shape]
      (implicit ev: Aux[FromT, Value, O])
      : Aux[FromH :: FromT, Value, FromH :: O] = instance
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
    type Aux[S <: Shape, Axes <: Shape, I <: Int, O <: Shape] =
      Loop[S, Axes, I] { type Out = O }

    val instance: Nothing = (new Loop {}).asInstanceOf

    implicit def caseNil[I <: Int]: Aux[Ø, Ø, I, Ø] = instance

    implicit def caseConsTrue[Sh <: Int, St <: Shape, Axes <: Shape, I <: Int, RemoveOut <: Shape, Out <: Shape]
      (implicit
        rm: Remove.Aux[Axes, I, RemoveOut],
        lo: Aux[St, RemoveOut, Succ[I], Out]
      ): Aux[Sh :: St, Axes, I, Out] = instance

    implicit def caseConsFalse[Sh <: Int, St <: Shape, Axes <: Shape, I <: Int, Out <: Shape]
      (implicit
        lo: Aux[St, Axes, Succ[I], Out]
      ): Aux[Sh :: St, Axes, I, Sh :: Out] = instance
  }

  object Bench {
    def main(args: Array[String]): Unit = {
      type S = (
        0 :: //X
        Ø
      )

      def the[T](implicit x: T): x.type = x
      val reduced = the[ReduceAxes[S, S]]
      implicitly[reduced.Out =:= Ø]
    }
  }
}
