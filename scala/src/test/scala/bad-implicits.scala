object example {
// start section implicitAmbiguity
implicit val number: Int = 1
implicit val modulo: Int = 3
def addm(x: Int, y: Int)(implicit m: Int) = (x + y) % m
addm(4, 5) // Error: ambiguous implicit arguments: both value number and
           // value modulo match type Int of parameter m of method addm.
// end section implicitAmbiguity
}
