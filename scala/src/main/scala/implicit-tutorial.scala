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
trait Ord[T]:
  def compare(a: T, b: T): Boolean

def comp[T](x: T, y: T)(implicit ev: Ord[T]): Boolean =
  ev.compare(x, y)

implicit def intOrd: Ord[Int] = new Ord[Int]:
  def compare(a: Int, b: Int): Boolean = a < b

comp(1, 2)
// end section ordExample

}
