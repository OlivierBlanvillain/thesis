import annotation.experimental
import Package._
import Lib.{CharAt, Reverse}
import compiletime.ops.int.Max
import compiletime.ops.int.-

@experimental
object Examples {

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

// start section regexFirstIteration
import compiletime.ops.string._
import compiletime.ops.int.+

type Compile[R <: String] =
  Reverse[Loop[R, 0, Length[R], EmptyTuple]]

type Loop[R <: String, Lo <: Int, Hi <: Int, Acc <: Tuple] <: Tuple =
  Lo match
    case Hi => Acc
    case _  => CharAt[R, Lo] match
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
import Lib.IsNullable

// start section regexLastIteration
type Loop[R <: String, Lo <: Int, Hi <: Int, Acc <: Tuple, Opt <: Int] =
  Lo match
    case Hi => Acc
    case _  => CharAt[R, Lo] match
      case ")" => Loop[R, Lo + 1, Hi, Acc, Max[0, Opt - 1]]
      case "(" =>
        Opt match
          case 0 => IsNullable[R, Lo + 1, Hi, 0] match
            case true  => Loop[R, Lo + 1, Hi, Option[String] *: Acc, 1]
            case false => Loop[R, Lo + 1, Hi, Acc, 0]
          case _ => Loop[R, Lo + 1, Hi, Option[String] *: Acc, Opt + 1]
      case "\\" => Loop[R, Lo + 2, Hi, Acc, Opt]
      case _ => Loop[R, Lo + 1, Hi, Acc, Opt]
// end section regexLastIteration

def isNullable(r: String, at: Int, hi: Int, lvl: Int): Boolean = false

// start section regexTermLvlLoop
val identity: String => Any = { x => assert(x != null); x }

def loop(r: String, lo: Int, hi: Int, acc: List[String => Any], opt: Int)
    : List[String => Any] =
  lo match
    case `hi` => acc
    case _  => r.charAt(lo) match
      case ')' => loop(r, lo + 1, hi, acc, 0.max(opt - 1))
      case '(' =>
        opt match
          case 0 => isNullable(r, lo + 1, hi, 0) match
            case true  => loop(r, lo + 1, hi, Option.apply :: acc, 1)
            case false => loop(r, lo + 1, hi, identity :: acc, 0)
          case _ => loop(r, lo + 1, hi, Option.apply :: acc, opt + 1)
      case '\\' => loop(r, lo + 2, hi, acc, opt)
      case _ => loop(r, lo + 1, hi, acc, opt)
// end section regexTermLvlLoop

// start section regexTransform
def transform[P](pattern: String, arr: Array[String]): P =
  val fs = loop(pattern, 0, pattern.length, Nil, 0).reverse
  val ts = Tuple.fromArray(arr.zip(fs).map { (x, f) => f(x) })
  assert(arr.size == fs.size, "Unexpected number of capturing groups")
  ts.asInstanceOf[P]
// end section regexTransform

}

}
