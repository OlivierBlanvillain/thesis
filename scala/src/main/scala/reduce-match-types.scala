object NumpyMatchTypes {

sealed trait Shape
final case class #:[+H <: Int & Singleton, +T <: Shape](head: H, tail: T) extends Shape
case object Ø extends Shape

// cheating a little bit here, but there's a footnote to explain it!
object SimplifiedShape {
// start section shapeEnum
enum Shape:
  case #:[H <: Int, T <: Shape](head: H, tail: T)
  case Ø
// end section shapeEnum
}

type Ø = Ø.type
type None = None.type

// start section datatypeTrait
trait DataType[T]
// end section datatypeTrait

// start section ndarrayTrait
trait NDArray[T, S <: Shape]
// end section ndarrayTrait

// start section randomnormalDef
def random_normal[S <: Shape](shape: S): NDArray[Float, S] = ???
// end section randomnormalDef

// start section multiplyDef
def multiply[T, S <: Shape](x: NDArray[T, S], y: NDArray[T, S]): NDArray[T, S] = ???
// end section multiplyDef

import scala.compiletime.ops.int.*
// start section numelementsType
type NumElements[X <: Shape] <: Int =
  X match
    case Ø => 1
    case head #: tail => head * NumElements[tail]
// end section numelementsType

// start section reshapeDef
def reshape[T, From <: Shape, To <: Shape](arr: NDArray[T, From], newshape: To)
    (implicit ev: NumElements[From] =:= NumElements[To]): NDArray[T, To] = ???
// end section reshapeDef

import scala.compiletime.ops.int.+
// start section reduceType
type ReduceAxes[S <: Shape, Axes <: None | Shape] <: Shape =
  Axes match
    case None => Ø
    case Shape => Loop[S, Axes, 0]
// end section reduceType

// start section reduceLoop
type Loop[S <: Shape, Axes <: Shape, I <: Int] <: Shape =
  S match
    case head #: tail => Contains[Axes, I] match
      case true => Loop[tail, Remove[Axes, I], I + 1]
      case false => head #: Loop[tail, Axes, I + 1]
    case Ø => Axes match
      case Ø => Ø // otherwise, do not reduce further
// end section reduceLoop

type Contains[Haystack <: Shape, Needle <: Int] <: Boolean = Haystack match {
  case head #: tail => head match {
    case Needle => true
    case _ => Contains[tail, Needle]
  }
  case Ø => false
}

type Remove[From <: Shape, Value <: Int] <: Shape = From match {
  case head #: tail => head match {
    case Value => tail
    case _ => head #: Remove[tail, Value]
  }
  case Ø => Ø
}

// start section npmeanDef
def mean[T, S <: Shape, A <: Shape](arr: NDArray[T, S], axes: A): NDArray[T, ReduceAxes[S, A]] = ???
// end section npmeanDef

object Bench {
  def main(args: Array[String]): Unit = {
    type ::[A <: Int & Singleton, B <: Shape] = #:[A, B]

    type A = (
      0 :: //X
      Ø
    )

    type Reduced = ReduceAxes[A, A]

    implicitly[Reduced =:= Ø]
  }
}

}
