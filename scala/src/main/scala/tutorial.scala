object Tutorial {

object explicit {
// start section explicitModulo
val modulo: Int = 3
def addm(x: Int, y: Int)(m: Int) = (x + y) % m
addm(4, 5)(modulo)
// end section explicitModulo
}

object impl {
// start section implicitModulo
implicit val modulo: Int = 3
def addm(x: Int, y: Int)(implicit m: Int) = (x + y) % m
addm(4, 5)
// end section implicitModulo

// start section ordExample
trait Ordering[T]:
  def compare(a: T, b: T): Boolean

def comp[T](x: T, y: T)(implicit ev: Ordering[T]): Boolean =
  ev.compare(x, y)

implicit def intOrdering: Ordering[Int] =
  new Ordering[Int]:
    def compare(a: Int, b: Int): Boolean = a < b

comp(1, 2)
// end section ordExample


// start section ordListExample
implicit def listOrdering[T](implicit ev: Ordering[T]): Ordering[List[T]] =
  new Ordering[List[T]]:
    def compare(a: List[T], b: List[T]): Boolean =
      (a, b) match
        case (a :: as, b :: bs) => ev.compare(a, b) && compare(as, bs)
        case (_, Nil) => false
        case (Nil, _) => true
// end section ordListExample

import scala.compiletime.ops.int.+

// start section geqDefinition
// GEQ[A, B] compiles if A is greater than B
type GEQ[A <: Int, B <: Int] = A match {
  case B => true
  case _ => GEQ[A, B + 1]
}
// end section geqDefinition

type A = GEQ[5, 1]

/*
// start section geqExample
5 <: 1? no
5 <: 1+1? no
5 <: 1+1+1? no
5 <: 1+1+1+1? no
5 <: 1+1+1+1+1? yes
// end section geqExample
*/

}

}
