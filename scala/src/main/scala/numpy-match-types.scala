object NumpyMatchTypes {

sealed trait Shape
final case class #:[+H <: Int, +T <: Shape](head: H, tail: T) extends Shape
case object Ø extends Shape

// cheating a little bit here, but there's a footnote to explain it!
object SimplifiedShape {
// start section shapeEnum
enum Shape {
  case #:[H <: Int, T <: Shape](head: H, tail: T)
  case Ø
}
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
  X match {
    case Ø => 1
    case head #: tail => head * NumElements[tail]
  }
// end section numelementsType

// start section reshapeDef
def reshape[T, From <: Shape, To <: Shape](arr: NDArray[T, From], newshape: To)
    (implicit ev: NumElements[From] =:= NumElements[To]): NDArray[T, To] = ???
// end section reshapeDef

import scala.compiletime.ops.int.+
// start section reduceType
type ReduceAxes[S <: Shape, Axes <: None | Shape] <: Shape =
  Axes match {
    case None => Ø
    case Shape => Loop[S, Axes, 0]
  }
// end section reduceType

// start section reduceLoop
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
    type ::[A <: Int, B <: Shape] = #:[A, B]

    type A = (
      0 #: 1 #: 2 #: 3 #: 4 #: 5 #: 6 #: 7 #: 8 #: 9 #: 10 #: 11 #: 12 #: 13 #: 14 #: 15 #: 16 #: 17 #: 18 #: 19 #: 20 #: 21 #: 22 #: 23 #: 24 #: 25 #: 26 #: 27 #: 28 #: 29 #: 30 #: 31 #: 32 #: 33 #: 34 #: 35 #: 36 #: 37 #: 38 #: 39 #: 40 #: 41 #: 42 #: 43 #: 44 #: 45 #: 46 #: 47 #: 48 #: 49 #: 50 #: 51 #: 52 #: 53 #: 54 #: 55 #: 56 #: 57 #: 58 #: 59 #: 60 #: 61 #: 62 #: 63 #: 64 #: 65 #: 66 #: 67 #: 68 #: 69 #: 70 #: 71 #: 72 #: 73 #: 74 #: 75 #: 76 #: 77 #: 78 #: 79 #: 80 #: 81 #: 82 #: 83 #: 84 #: 85 #: 86 #: 87 #: 88 #: 89 #: 90 #: 91 #: 92 #: 93 #: 94 #: 95 #: 96 #: 97 #: 98 #: 99 #: 100 #: 101 #: 102 #: 103 #: 104 #: 105 #: 106 #: 107 #: 108 #: 109 #: 110 #: 111 #: 112 #: 113 #: 114 #: 115 #: 116 #: 117 #: 118 #: 119 #: 120 #: 121 #: 122 #: 123 #: 124 #: 125 #: 126 #: 127 #: 128 #: 129 #: 130 #: 131 #: 132 #: 133 #: 134 #: 135 #: 136 #: 137 #: 138 #: 139 #: 140 #: 141 #: 142 #: 143 #: 144 #: 145 #: 146 #: 147 #: 148 #: 149 #: 150 #: Ø
      // 0 #: 1 #: 2 #: 3 #: 4 #: 5 #: 6 #: 7 #: 8 #: 9 #: 10 #: Ø
    )

    type Reduced = ReduceAxes[A, A]

    implicitly[Reduced =:= Ø]
  }
}

}
