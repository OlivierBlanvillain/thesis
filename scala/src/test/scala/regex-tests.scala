import annotation.experimental
import Package._

@experimental
object Tests {
  object TypeLevel {
    def check[A <: String, B](implicit ev: Lib.Compile[A] =:= B): Unit = ()
    type S = String
    type O = Option[String]

    check["(\\d{4})-(\\d{2})-(\\d{2})", (S, S, S)]
    check["(\\()-(\\d{2})-(\\d{2})", (S, S, S)]
    check["(\\d{4})?-(\\d{2})*-(\\d{2})++", (O, O, S)]
    check["((A)(B(C)))", (S, S, S, S)]
    check["((A)(B(C)))?", (O, O, O, O)]
    check["((A)(B(C))?)", (S, S, O, O)]
    check["((A)|(B(C)))", (S, O, O, O)]
    check["(B)|C", O]
    check["C|(A)", O]
    check[".*\\s(([A-Za-z]{5}(hum)?).js)\\s.*", (S, S, O)]

    check["((?:A)(B(C)))", (S, S, S)]
    check["((A)(?:B(C)))?", (O, O, O)]
    check["((A)(B(?:C))?)", (S, S, O)]
    check["(?:(A)|(B(C)))", (O, O, O)]
  }

  def assertEquals(expected: Any, actual: Any): Unit =
    assert(
      expected == actual,
      s"expected: $expected, actual: $actual")

  def main(args: Array[String]): Unit = {
    val date = Regex("(\\d{4})-(\\d{2})-(\\d{2})")
    assertEquals("2004", "2004-01-20" match
      case date(year, month, day) => year)

    val everyday = Regex(".*\\s(([A-Za-z]{5}(hum)?).js)\\s.*")
    assertEquals(("Scala.js", "Scala"),
      "Write Scala.js everyday!" match
        case everyday(a, b, None) => (a, b))

    println("OK!")
  }
}
