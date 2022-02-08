import java.util.regex.Pattern
import annotation.experimental
import compiletime.ops.string._
import compiletime.ops.int.{+, -, Max, <}
import Package.Lib._

@experimental
object Package {

// start section regexUserLevel
object Regex:
  def apply[R <: String & Singleton](regex: R): Regex[Compile[R]] =
    new Regex(regex)

class Regex[P] private (val regex: String):
  val pattern = Pattern.compile(regex)
  def unapply(s: String): Option[P] =
    val m = pattern.matcher(s)
    if (m.matches())
      val a = Array.tabulate(m.groupCount) { i => m.group(i + 1) }
      Some(toTuple[P](regex, a))
    else
      None
// end section regexUserLevel

object Lib {

type CharAt[R <: String, At <: Int] =
  Substring[R, At, At + 1]

type Compile[R <: String] =
  Reverse[Loop[R, 0, Length[R], EmptyTuple, 0]]

type Loop[R <: String, Lo <: Int, Hi <: Int, Acc <: Tuple, Nw <: Int] <: Tuple =
  Lo match
    case Hi => Acc
    case _   => CharAt[R, Lo] match
      case "|" =>
        CharAt[R, Lo + 1] match {
          case "(" =>
            IsCapturing[R, Lo + 2] match {
              case false => Loop[R, Lo + 2, Hi, Acc, Nw + 1]
              case true  => Loop[R, Lo + 2, Hi, Option[String] *: Acc, Nw + 1]
            }
          case _ => Loop[R, Lo + 2, Hi, Acc, Nw]
        }
      case ")" =>
        Loop[R, Lo + 1, Hi, Acc, Max[0, Nw - 1]]
      case "(" =>
        Nw match
          case 0 =>
            IsNullable[R, Lo + 1, Hi, 0] match
              case true =>
                IsCapturing[R, Lo + 1] match {
                  case false => Loop[R, Lo + 1, Hi, Acc, 1]
                  case true  => Loop[R, Lo + 1, Hi, Option[String] *: Acc, 1]
                }
              case false =>
                IsCapturing[R, Lo + 1] match {
                  case false => Loop[R, Lo + 1, Hi, Acc, 0]
                  case true  => Loop[R, Lo + 1, Hi, String *: Acc, 0]
                }
          case _ =>
            IsCapturing[R, Lo + 1] match {
              case false => Loop[R, Lo + 1, Hi, Acc, Nw + 1]
              case true  => Loop[R, Lo + 1, Hi, Option[String] *: Acc, Nw + 1]
            }
      case "\\" => Loop[R, Lo + 2, Hi, Acc, Nw]
      case _ => Loop[R, Lo + 1, Hi, Acc, Nw]

// start section regexIsCapturing
type IsCapturing[R <: String, At <: Int] <: Boolean =
  CharAt[R, At] match
    case "?" => CharAt[R, At + 1] match
      case "<" => CharAt[R, At + 2] match
        case "!" => false // zero-width negative lookbehind
        case _ => true    // named-capturing group
      case _ => false     // other non-capturing special constructs
    case _ => true        // unnamed-capturing group
// end section regexIsCapturing

// start section regexIsNullable
type IsNullable[R <: String, At <: Int, Hi <: Int, Ops <: Int] <: Boolean =
  CharAt[R, At] match
    case ")" => Ops match
      case 0 => IsMarked[R, At + 1, Hi]
      case _ => IsNullable[R, At + 1, Hi, Ops - 1]
    case "(" => IsNullable[R, At + 1, Hi, Ops + 1]
    case "\\" => IsNullable[R, At + 2, Hi, Ops]
    case _    => IsNullable[R, At + 1, Hi, Ops]

type IsMarked[R <: String, At <: Int, Hi <: Int] <: Boolean =
  At match
    case Hi => false
    case _ =>
      CharAt[R, At] match
        case "?" | "*" | "|" => true
        case "+" => IsMarked[R, At + 1, Hi]
        case _ => false
// end section regexIsNullable

def toTuple[P](pattern: String, arr: Array[String]): P = {
  if (arr.size == 1)
    arr(0).asInstanceOf[P]
  else
    val fs = loop(pattern, 0, pattern.length, Nil, 0).reverse
    val ts = Tuple.fromArray(arr.zip(fs).map { (x, f) => f(x) })
    assert(arr.size == fs.size)
    ts.asInstanceOf[P]
}

def loop(r: String, lo: Int, hi: Int, acc: List[String => Any], nw: Int): List[String => Any] =
  lo match
    case `hi` => acc
    case _   => r.charAt(lo) match
      case ')'  =>
        loop(r, lo + 1, hi, acc, 0.max(nw - 1))
      case '('  =>
        nw match
          case 0 =>
            nullable(r, lo + 1, hi) match
              case true =>
                loop(r, lo + 1, hi, Option[String] :: acc, 1)
              case false =>
                loop(r, lo + 1, hi, (x => x) :: acc, 0)
          case _ =>
            loop(r, lo + 1, hi, Option[String] :: acc, nw + 1)
      case '\\' => loop(r, lo + 2, hi, acc, nw)
      case _    => loop(r, lo + 1, hi, acc, nw)


def nullmark(r: String, at: Int, hi: Int): Boolean =
  at match
    case `hi` => false
    case _ =>
      r.charAt(at) match
        case '?' | '*' | '|' => true
        case '+' => nullmark(r, at + 1, hi)
        case _ => false

def nullable(r: String, at: Int, hi: Int): Boolean =
  at match
    case 1 => nullable0(r, at, hi, 0)
    case _ => r.charAt(at - 2) match
      case '|' => true
      case _ => nullable0(r, at, hi, 0)

def nullable0(r: String, at: Int, hi: Int, ops: Int): Boolean =
  at match
    case `hi` => false
    case _ =>
      r.charAt(at) match
        case ')' => ops match
          case 0 => nullmark(r, at + 1, hi)
          case _ => nullable0(r, at + 1, hi, ops - 1)
        case '(' => nullable0(r, at + 1, hi, ops + 1)
        case '\\' => nullable0(r, at + 2, hi, ops)
        case _    => nullable0(r, at + 1, hi, ops)

// Actually, Scala's unapply does not play way with Tuple1, so we need to
// get ride of those types. Furthermore, the code responsible for
// generating _n extractors for tuples does not know about generic tuples,
// so we also manually flattern those types here...
type Reverse[L <: Tuple] = L match {
  case EmptyTuple => Unit
  case x1 *: EmptyTuple => x1
  case x1 *: x2 *: EmptyTuple => (x2, x1)
  case x1 *: x2 *: x3 *: EmptyTuple => (x3, x2, x1)
  case x1 *: x2 *: x3 *: x4 *: EmptyTuple => (x4, x3, x2, x1)
  case x1 *: x2 *: x3 *: x4 *: x5 *: EmptyTuple => (x5, x4, x3, x2, x1)
  case x1 *: x2 *: x3 *: x4 *: x5 *: x6 *: EmptyTuple => (x6, x5, x4, x3, x2, x1)
  case x1 *: x2 *: x3 *: x4 *: x5 *: x6 *: x7 *: EmptyTuple => (x7, x6, x5, x4, x3, x2, x1)
  case x1 *: x2 *: x3 *: x4 *: x5 *: x6 *: x7 *: x8 *: EmptyTuple => (x8, x7, x6, x5, x4, x3, x2, x1)
  case x1 *: x2 *: x3 *: x4 *: x5 *: x6 *: x7 *: x8 *: x9 *: EmptyTuple => (x9, x8, x7, x6, x5, x4, x3, x2, x1)
  case x1 *: x2 *: x3 *: x4 *: x5 *: x6 *: x7 *: x8 *: x9 *: x10 *: EmptyTuple => (x10, x9, x8, x7, x6, x5, x4, x3, x2, x1)
  case x1 *: x2 *: x3 *: x4 *: x5 *: x6 *: x7 *: x8 *: x9 *: x10 *: x11 *: EmptyTuple => (x11, x10, x9, x8, x7, x6, x5, x4, x3, x2, x1)
  case x1 *: x2 *: x3 *: x4 *: x5 *: x6 *: x7 *: x8 *: x9 *: x10 *: x11 *: x12 *: EmptyTuple => (x12, x11, x10, x9, x8, x7, x6, x5, x4, x3, x2, x1)
  case x1 *: x2 *: x3 *: x4 *: x5 *: x6 *: x7 *: x8 *: x9 *: x10 *: x11 *: x12 *: x13 *: EmptyTuple => (x13, x12, x11, x10, x9, x8, x7, x6, x5, x4, x3, x2, x1)
  case x1 *: x2 *: x3 *: x4 *: x5 *: x6 *: x7 *: x8 *: x9 *: x10 *: x11 *: x12 *: x13 *: x14 *: EmptyTuple => (x14, x13, x12, x11, x10, x9, x8, x7, x6, x5, x4, x3, x2, x1)
  case x1 *: x2 *: x3 *: x4 *: x5 *: x6 *: x7 *: x8 *: x9 *: x10 *: x11 *: x12 *: x13 *: x14 *: x15 *: EmptyTuple => (x15, x14, x13, x12, x11, x10, x9, x8, x7, x6, x5, x4, x3, x2, x1)
  case x1 *: x2 *: x3 *: x4 *: x5 *: x6 *: x7 *: x8 *: x9 *: x10 *: x11 *: x12 *: x13 *: x14 *: x15 *: x16 *: EmptyTuple => (x16, x15, x14, x13, x12, x11, x10, x9, x8, x7, x6, x5, x4, x3, x2, x1)
  case x1 *: x2 *: x3 *: x4 *: x5 *: x6 *: x7 *: x8 *: x9 *: x10 *: x11 *: x12 *: x13 *: x14 *: x15 *: x16 *: x17 *: EmptyTuple => (x17, x16, x15, x14, x13, x12, x11, x10, x9, x8, x7, x6, x5, x4, x3, x2, x1)
  case x1 *: x2 *: x3 *: x4 *: x5 *: x6 *: x7 *: x8 *: x9 *: x10 *: x11 *: x12 *: x13 *: x14 *: x15 *: x16 *: x17 *: x18 *: EmptyTuple => (x18, x17, x16, x15, x14, x13, x12, x11, x10, x9, x8, x7, x6, x5, x4, x3, x2, x1)
  case x1 *: x2 *: x3 *: x4 *: x5 *: x6 *: x7 *: x8 *: x9 *: x10 *: x11 *: x12 *: x13 *: x14 *: x15 *: x16 *: x17 *: x18 *: x19 *: EmptyTuple => (x19, x18, x17, x16, x15, x14, x13, x12, x11, x10, x9, x8, x7, x6, x5, x4, x3, x2, x1)
  case x1 *: x2 *: x3 *: x4 *: x5 *: x6 *: x7 *: x8 *: x9 *: x10 *: x11 *: x12 *: x13 *: x14 *: x15 *: x16 *: x17 *: x18 *: x19 *: x20 *: EmptyTuple => (x20, x19, x18, x17, x16, x15, x14, x13, x12, x11, x10, x9, x8, x7, x6, x5, x4, x3, x2, x1)
  case x1 *: x2 *: x3 *: x4 *: x5 *: x6 *: x7 *: x8 *: x9 *: x10 *: x11 *: x12 *: x13 *: x14 *: x15 *: x16 *: x17 *: x18 *: x19 *: x20 *: x21 *: EmptyTuple => (x21, x20, x19, x18, x17, x16, x15, x14, x13, x12, x11, x10, x9, x8, x7, x6, x5, x4, x3, x2, x1)
  case x1 *: x2 *: x3 *: x4 *: x5 *: x6 *: x7 *: x8 *: x9 *: x10 *: x11 *: x12 *: x13 *: x14 *: x15 *: x16 *: x17 *: x18 *: x19 *: x20 *: x21 *: x22 *: EmptyTuple => (x22, x21, x20, x19, x18, x17, x16, x15, x14, x13, x12, x11, x10, x9, x8, x7, x6, x5, x4, x3, x2, x1)
}

}

// trait Parser[T <: Tuple] {
//   def mutate(arr: Array[String]): Unit
//   val i: Int
// }

// object Parser {
//   implicit val basecase: Parser[EmptyTuple] =
//     new Parser[EmptyTuple] {
//       val i = 0
//       def mutate(arr: Array[String]): Unit = ()
//     }

//   implicit def stringcase[T <: Tuple]
//     (implicit ev: Parser[T]): Parser[String *: T] =
//       new Parser[String *: T] {
//         val i = ev.i + 1
//         def mutate(arr: Array[String]): Unit =
//           ev.mutate(arr)
//       }

//   implicit def optioncase[T <: Tuple]
//     (implicit ev: Parser[T]): Parser[Option[String] *: T] =
//       new Parser[Option[String] *: T] {
//         val i = ev.i + 1
//         def mutate(arr: Array[String]): Unit = {
//           ev.mutate(arr)
//           arr(i) = Option(arr(i))
//         }
//       }
// }

// val p: Parser[String *: Option[String] *: String *: EmptyTuple] = implicitly[Parser[String *: Option[String] *: String *: EmptyTuple]](
//   stringcase(optioncase(stringcase(basecase())))

//   val ev =     new Parser[String *: T] {
//     val size =
//       new Parser[EmptyTuple] {
//           def mutate(arr: Array[String]): Unit = ()
//           val size = 0
//         }
//       .size + 1
//     def mutate(arr: Array[String]): Unit = ()
//   }
//   new Parser[Option[String] *: T] {
//     val size = ev.size + 1
//     def mutate(arr: Array[String]): Unit =
//       ev.mutate(arr)
//       arr(size) = Option(arr(size))
//   }
// )
// val arr = Array("a", "b", "c")
// p.mutate(arr)
// assert(arr == Array("a", Option("b"), "c"))

}
