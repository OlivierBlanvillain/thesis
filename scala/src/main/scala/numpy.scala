sealed trait Shape
final case class #:[H <: Int & Singleton, T <: Shape](head: H, tail: T) extends Shape
case object Ø extends Shape

// cheating a little bit here, but there's a footnote to explain it!
object SimplifiedShape {
// start section shape
enum Shape {
  case #:[H <: Int, T <: Shape](head: H, tail: T)
  case Ø
}
// end section shape
}

type Ø = Ø.type
type None = None.type

// start section datatype
trait DataType[T]
// end section datatype

// start section ndarray
trait NDArray[T, S <: Shape]
// end section ndarray

// start section randomnormal
def random_normal[S <: Shape](shape: S): NDArray[Float, S] = ???
// end section randomnormal

// start section multiply
def multiply[T, S <: Shape](x: NDArray[T, S], y: NDArray[T, S]): NDArray[T, S] = ???
// end section multiply

import scala.compiletime.ops.int.*
// start section numelements
type NumElements[X <: Shape] <: Int =
  X match {
    case Ø => 1
    case head #: tail => head * NumElements[tail]
  }
// end section numelements

// start section reshape
def reshape[T, From <: Shape, To <: Shape](arr: NDArray[T, From], newshape: To)
    (implicit ev: NumElements[From] =:= NumElements[To]): NDArray[T, To] = ???
// end section reshape

import scala.compiletime.ops.int.+
// start section reduce
type Reduce[S <: Shape, Axes <: None | Shape] <: Shape =
  Axes match {
    case None => Ø
    case Shape => Loop[S, Axes, 0]
  }

type Loop[S <: Shape, Axes <: Shape, I <: Int] <: Shape =
  S match {
    case head #: tail => Contains[Axes, I] match {
      case true => Loop[tail, Remove[Axes, I], I + 1]
      case false => head #: Loop[tail, Axes, I + 1]
    }
    case Ø => Axes match {
      case Ø => Ø
      // otherwise, do not reduce further
    }
  }
// end section reduce

type Contains[Haystack <: Shape, Needle <: Int] <: Boolean = Haystack match {
  case Ø => false
  case head #: tail => head match {
    case Needle => true
    case _ => Contains[tail, Needle]
  }
}

type Remove[From <: Shape, Value <: Int & Singleton] <: Shape = From match {
  case Ø => Ø
  case head #: tail => head match {
    case Value => Remove[tail, Value]
    case _ => head #: Remove[tail, Value]
  }
}

// start section npmean
def mean[T, S <: Shape, A <: Shape](arr: NDArray[T, S], axes: A): NDArray[T,
    Reduce[S, A]] = ???
// end section npmean
