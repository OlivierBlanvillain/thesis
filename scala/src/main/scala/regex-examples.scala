package bench

import annotation.experimental
import bench._
import RegexPackage._
import Lib.Reverse
import compiletime.ops.string.Substring
import compiletime.ops.int.Max
import compiletime.ops.int.{-, +}

@experimental
object RegexExamples {

type CharAt[R <: String, At <: Int] =
  Substring[R, At, At + 1] match {
    case ")" => ')'
    case "(" => '('
    case "\\" => '\\'
    case "?" => '?'
    case "*" => '?'
    case "<" => '<'
    case "=" => '='
    case "!" => '!'
    case _ => '_'
  }

// start section elemExample
type Elem[X] = X match
  case String => Char
  case Iterable[t] => Elem[t]
  case Any => X
// end section elemExample

val rev0test: Reverse0[String *: Int *: EmptyTuple, EmptyTuple]
  = (1, "")

// start section regexIsCapturing
type IsCapturing[R <: String, At <: Int] =
  CharAt[R, At] match
    case "?" => CharAt[R, At + 1] match
      case "<" => CharAt[R, At + 2] match
        case "=" | "!" => false // lookbehinds
        case _ => true // named-capturing group
      case _ => false // other special constructs
    case _ => true // unnamed-capturing group
// end section regexIsCapturing

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

type Loop[R, Lo, Hi, Acc <: Tuple] =
  Lo match
    case Hi => Acc
    case _  => CharAt[R, Lo] match
      case '('  => Loop[R, Lo + 1, Hi, String *: Acc]
      case '\\' => Loop[R, Lo + 2, Hi, Acc]
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
type IsNullable[R <: String, At, Hi] =
  CharAt[R, At] match
    case ')'  => IsMarked[R, At + 1, Hi]
    case '\\' => IsNullable[R, At + 2, Hi]
    case _    => IsNullable[R, At + 1, Hi]

type IsMarked[R <: String, At, Hi] =
  At match
    case Hi => false
    case _ => CharAt[R, At] match
      case '?' | '*' => true
      case _ => false
// end section regexNaiveIsNullable

}

type IsMarked[R <: String, At, Hi] <: Boolean =
  At match
    case Hi => false
    case _ =>
      CharAt[R, At] match
        case '?' | '*' => true
        case _ => false

// start section regexIsNullable
type IsNullable[R <: String, At, Hi, Lvl <: Int] =
  CharAt[R, At] match
    case ')' => Lvl match
      case 0 => IsMarked[R, At + 1, Hi]
      case _ => IsNullable[R, At + 1, Hi, Lvl - 1]
    case '(' => IsNullable[R, At + 1, Hi, Lvl + 1]
    case '\\' => IsNullable[R, At + 2, Hi, Lvl]
    case _    => IsNullable[R, At + 1, Hi, Lvl]
// end section regexIsNullable

object Last {

// start section regexLastIteration
type Loop[R, Lo, Hi, Opt <: Int, Acc <: Tuple] =
  Lo match
    case Hi => Acc
    case _  => CharAt[R, Lo] match
      case ')' =>
        Loop[R, Lo+1, Hi, Acc, Max[0, Opt-1]]
      case '(' => Opt match
        case 0 => IsNullable[R, Lo+1, Hi, 0] match
          case true =>
            Loop[R, Lo+1, Hi, Option[String]*:Acc, 1]
          case false =>
            Loop[R, Lo+1, Hi, String*:Acc, 0]
        case _ => Loop[R, Lo+1, Hi,
                       Option[String]*:Acc, Opt+1]
      case '\\' => Loop[R, Lo+2, Hi, Opt, Acc]
      case _ => Loop[R, Lo+1, Hi, Opt, Acc]
// end section regexLastIteration

def isNullable(r: String, at: Int, hi: Int, lvl: Int): Boolean = false

// start section regexTermLvlLoop
val id: String => Any = { x => assert(x != null); x }
val option: String => Any = { x => Option(x) }

def loop(r: String, lo: Int, hi: Int, acc: List[String => Any], opt: Int): List[String => Any] =
  lo match
    case `hi` => acc
    case _  => r.charAt(lo) match
      case ')' =>
        loop(r, lo+1, hi, acc, 0.max(opt-1))
      case '(' => opt match
        case 0 => isNullable(r, lo+1, hi, 0) match
          case true =>
            loop(r, lo+1, hi, option::acc, 1)
          case false =>
            loop(r, lo+1, hi, id::acc, 0)
        case _ => loop(r, lo+1, hi, option::acc, opt+1)
      case '\\' => loop(r, lo+2, hi, acc, opt)
      case _ => loop(r, lo+1, hi, acc, opt)
// end section regexTermLvlLoop

// start section regexTransform
def transform[P](r: String, arr: Array[String]): P =
  val fs = loop(r, 0, r.length, Nil, 0).reverse
  val wrapped = arr.zip(fs).map { (x, f) => f(x) }
  val tuple = Tuple.fromArray(wrapped)
  assert(arr.size == fs.size)
  tuple.asInstanceOf[P]
// end section regexTransform
}

def checkLib[A <: String, B](implicit ev: Lib.Compile[A] =:= B): Unit = ()
type O = Option[String]

checkLib["(\\d{4})-(\\d{2})-(\\d{2})", (S, S, S)]
checkLib["(\\()-(\\d{2})-(\\d{2})", (S, S, S)]
checkLib["(\\d{4})?-(\\d{2})*-(\\d{2})++", (O, O, S)]
checkLib["((A)(B(C)))", (S, S, S, S)]
checkLib["((A)(B(C)))?", (O, O, O, O)]
checkLib["((A)(B(C))?)", (S, S, O, O)]
checkLib["((A)|(B(C)))", (S, O, O, O)]
checkLib["(B)|C", O]
checkLib["C|(A)", O]
checkLib[".*\\s(([A-Za-z]{5}(hum)?).js)\\s.*", (S, S, O)]

checkLib["((?:A)(B(C)))", (S, S, S)]
checkLib["((A)(?:B(C)))?", (O, O, O)]
checkLib["((A)(B(?:C))?)", (S, S, O)]
checkLib["(?:(A)|(B(C)))", (O, O, O)]
checkLib["(A)(B)|(C)(D)", (O, O, O, O)]
checkLib["(A)(B)(C)(D)|", (O, O, O, O)]
checkLib["((A)|(B)(C))(D)", (S, O, O, O, S)]

}
