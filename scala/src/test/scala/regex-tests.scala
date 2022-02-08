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

  def examples: Unit = {
// start section regexDocumentation
val date = Regex("(\\d{4})-(\\d{2})-(\\d{2})")
"2004-01-20" match
  case date(y, m, d) =>
    s"$y was a good year for PLs."
// end section regexDocumentation

// start section regexRational
val rational = Regex("(\\d+)\\.?(\\d+)?")
"3.1415" match
  case rational(i, Some(f)) =>
    val n = i.size + f.size
    s"This number is $n digits long"
// end section regexRational

import Lib.{CharAt, Reverse}

// start section regexFirstIteration
import compiletime.ops.string._
import compiletime.ops.int.+

type Compile[R <: String] =
  Reverse[Loop[R, 0, Length[R], EmptyTuple]]

type Loop[R <: String, Lo <: Int, Hi <: Int, Acc <: Tuple] <: Tuple =
  Lo match
    case Hi => Acc
    case _   => CharAt[R, Lo] match
      case "("  => Loop[R, Lo + 1, Hi, String *: Acc]
      case "\\" => Loop[R, Lo + 2, Hi, Acc]
      case _ => Loop[R, Lo + 1, Hi, Acc]
// end section regexFirstIteration

def check[A <: String, B](implicit ev: Compile[A] =:= B): Unit = ()
type S = String
check["(\\d{4})-(\\d{2})-(\\d{2})", (S, S, S)]
check["(\\()-(\\d{2})-(\\d{2})", (S, S, S)]
check["((A)(B(C)))", (S, S, S, S)]
check["((B(C)))", (S, S, S)]

object NaiveIsNullable {
// start section regexNaiveIsNullable
type IsNullable[R <: String, At <: Int, Hi <: Int] <: Boolean =
  CharAt[R, At] match
    case ")" => IsMarked[R, At + 1, Hi]
    case _   => IsNullable[R, At + 1, Hi]

type IsMarked[R <: String, At <: Int, Hi <: Int] <: Boolean =
  At match
    case Hi => false
    case _ =>
      CharAt[R, At] match
        case "?" | "*" => true
        case "+" => IsMarked[R, At + 1, Hi]
        case _ => false
// end section regexNaiveIsNullable
}

object Last {
import compiletime.ops.int.Max
import compiletime.ops.int.-
import Lib.IsNullable
// start section regexLastIteration
type Loop[R <: String, Lo <: Int, Hi <: Int, Acc <: Tuple, Lvl <: Int] =
  Lo match
    case Hi => Acc
    case _   => CharAt[R, Lo] match
      case ")" => Loop[R, Lo + 1, Hi, Acc, Max[0, Lvl - 1]]
      case "(" =>
        Lvl match
          case 0 => IsNullable[R, Lo + 1, Hi, 0] match
            case true =>  Loop[R, Lo + 1, Hi, Option[String] *: Acc, 1]
            case false => Loop[R, Lo + 1, Hi, Acc, 0]
          case _ => Loop[R, Lo + 1, Hi, Option[String] *: Acc, Lvl + 1]
      case "\\" => Loop[R, Lo + 2, Hi, Acc, Lvl]
      case _ => Loop[R, Lo + 1, Hi, Acc, Lvl]
// end section regexLastIteration
}

  }

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
