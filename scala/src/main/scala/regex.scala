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
      val a = Array.tabulate(m.groupCount)(i => m.group(i + 1))
      Some(transform[P](regex, a))
    else
      None
// end section regexUserLevel

// start section regexSanitizerTypeClass
abstract class Sanitizer[T](val i: Int):
  def mutate(arr: Array[Any]): Unit

object Sanitizer:
  implicit val basecase: Sanitizer[EmptyTuple] =
    new Sanitizer[EmptyTuple](0):
      def mutate(arr: Array[Any]): Unit = ()

  implicit def stringcase[T <: Tuple]
    (implicit ev: Sanitizer[T]): Sanitizer[String *: T] =
      new Sanitizer[String *: T](ev.i + 1):
        def mutate(arr: Array[Any]): Unit =
          assert(arr(arr.size - i) != null)
          ev.mutate(arr)

  implicit def optioncase[T <: Tuple]
    (implicit ev: Sanitizer[T]): Sanitizer[Option[String] *: T] =
      new Sanitizer[Option[String] *: T](ev.i + 1):
        def mutate(arr: Array[Any]): Unit =
          arr(arr.size - i) = Option(arr(arr.size - i))
          ev.mutate(arr)
// end section regexSanitizerTypeClass

  implicit val tuple1stringcase: Sanitizer[String] =
    new Sanitizer[String](1):
      def mutate(arr: Array[Any]): Unit =
        assert(arr(0) != null)

  implicit val tuple1optioncase: Sanitizer[Option[String]] =
    new Sanitizer[Option[String]](1):
      def mutate(arr: Array[Any]): Unit =
        arr(0) = Option(arr(0))

object Regex2:
  def apply[R <: String & Singleton](regex: R)(implicit san: Sanitizer[Compile[R]]): Regex2[Compile[R]] =
    new Regex2(regex, san)

class Regex2[P] private (val regex: String, val san: Sanitizer[P]):
  val pattern = Pattern.compile(regex)
  def unapply(s: String): Option[P] =
    val m = pattern.matcher(s)
    if (m.matches())
      val arr = Array.tabulate[Any](m.groupCount)(i => m.group(i + 1))
      assert(arr.size == san.i, "Unexpected number of capturing groups")
      san.mutate(arr)
      if (arr.size == 1)
        Some(arr(0).asInstanceOf[P])
      else
        Some(Tuple.fromArray(arr).asInstanceOf[P])
    else
      None

object Lib {

type CharAt[R <: String, At <: Int] =
  Substring[R, At, At + 1]

type Compile[R <: String] =
  Reverse[Loop[R, 0, Length[R], EmptyTuple, IsPiped[R, 0, Length[R], 0]]]

type Loop[R <: String, Lo <: Int, Hi <: Int, Acc <: Tuple, Opt <: Int] <: Tuple =

  Lo match
    case Hi => Acc
    case _  => CharAt[R, Lo] match
      case ")" =>
        Loop[R, Lo + 1, Hi, Acc, Max[0, Opt - 1]]
      case "(" =>
        Opt match
          case 0 =>
            IsNullable[R, Lo + 1, Hi, 0] match
              case true =>
                IsCapturing[R, Lo + 1] match {
                  case false => Loop[R, Lo + 1, Hi, Acc, 1]
                  case true  => Loop[R, Lo + 1, Hi, Option[String] *: Acc, 1]
                }
              case false =>
                IsCapturing[R, Lo + 1] match {
                  case false => Loop[R, Lo + 1, Hi, Acc, IsPiped[R, Lo + 1, Hi, 0]]
                  case true  => Loop[R, Lo + 1, Hi, String *: Acc, IsPiped[R, Lo + 1, Hi, 0]]
                }
          case _ =>
            IsCapturing[R, Lo + 1] match {
              case false => Loop[R, Lo + 1, Hi, Acc, Opt + 1]
              case true  => Loop[R, Lo + 1, Hi, Option[String] *: Acc, Opt + 1]
            }
      case "\\" => Loop[R, Lo + 2, Hi, Acc, Opt]
      case _ => Loop[R, Lo + 1, Hi, Acc, Opt]

// start section regexIsCapturing
type IsCapturing[R <: String, At <: Int] <: Boolean =
  CharAt[R, At] match
    case "?" => CharAt[R, At + 1] match
      case "<" => CharAt[R, At + 2] match
        case "=" | "!" => false // lookbehinds
        case _ => true          // named-capturing group
      case _ => false           // other special constructs
    case _ => true              // unnamed-capturing group
// end section regexIsCapturing

// start section regexIsNullable
type IsNullable[R <: String, At <: Int, Hi <: Int, Lvl <: Int] <: Boolean =
  CharAt[R, At] match
    case ")" => Lvl match
      case 0 => IsMarked[R, At + 1, Hi]
      case _ => IsNullable[R, At + 1, Hi, Lvl - 1]
    case "(" => IsNullable[R, At + 1, Hi, Lvl + 1]
    case "\\" => IsNullable[R, At + 2, Hi, Lvl]
    case _    => IsNullable[R, At + 1, Hi, Lvl]
// end section regexIsNullable

type IsMarked[R <: String, At <: Int, Hi <: Int] <: Boolean =
  At match
    case Hi => false
    case _ => CharAt[R, At] match
      case "?" | "*" => true
      case "{" => CharAt[R, At + 1] match
        case "0" => true
        case _ => false
      case "+" => IsMarked[R, At + 1, Hi]
      case _ => false

type IsPiped[R <: String, At <: Int, Hi <: Int, Lvl <: Int] <: Int =
  At match
    case Hi => 0
    case _ => CharAt[R, At] match
      case "|" => Lvl match
        case 0 => 1
        case _ => IsPiped[R, At + 1, Hi, Lvl]
      case ")" => Lvl match
        case 0 => 0
        case _ => IsPiped[R, At + 1, Hi, Lvl - 1]
      case "(" => IsPiped[R, At + 1, Hi, Lvl + 1]
      case "\\" => IsPiped[R, At + 2, Hi, Lvl]
      case _    => IsPiped[R, At + 1, Hi, Lvl]

def transform[P](pattern: String, arr: Array[String]): P =
  val fs = compile(pattern)
  val ts = arr.zip(fs).map { (x, f) => f(x) }
  assert(arr.size == fs.size, "Unexpected number of capturing groups")
  if (arr.size == 1)
    ts(0).asInstanceOf[P]
  else
    Tuple.fromArray(ts).asInstanceOf[P]

def compile(r: String): List[String => Any] =
  loop(r, 0, r.length, Nil, isPiped(r, 0, r.length, 0)).reverse

def loop(r: String, lo: Int, hi: Int, acc: List[String => Any], opt: Int): List[String => Any] =
  lo match
    case `hi` => acc
    case _  => r.charAt(lo) match
      case ')' =>
        loop(r, lo + 1, hi, acc, 0.max(opt - 1))
      case '(' =>
        opt match
          case 0 =>
            isNullable(r, lo + 1, hi, 0) match
              case true =>
                isCapturing(r, lo + 1) match {
                  case false => loop(r, lo + 1, hi, acc, 1)
                  case true  => loop(r, lo + 1, hi, Option.apply :: acc, 1)
                }
              case false =>
                isCapturing(r, lo + 1) match {
                  case false => loop(r, lo + 1, hi, acc, isPiped(r, lo + 1, hi, 0))
                  case true  => loop(r, lo + 1, hi, { x => assert(x != null); x } :: acc, isPiped(r, lo + 1, hi, 0))
                }
          case _ =>
            isCapturing(r, lo + 1) match {
              case false => loop(r, lo + 1, hi, acc, opt + 1)
              case true  => loop(r, lo + 1, hi, Option.apply :: acc, opt + 1)
            }
      case '\\' => loop(r, lo + 2, hi, acc, opt)
      case _ => loop(r, lo + 1, hi, acc, opt)

// start section regexisCapturing
def isCapturing(r: String, at: Int): Boolean =
  r.charAt(at) match
    case '?' => r.charAt(at + 1) match
      case '<' => r.charAt(at + 2) match
        case '=' | '!' => false // lookbehinds
        case _ => true          // named-capturing group
      case _ => false           // other special constructs
    case _ => true              // unnamed-capturing group
// end section regexisCapturing

// start section regexisNullable
def isNullable(r: String, at: Int, hi: Int, lvl: Int): Boolean =
  r.charAt(at) match
    case ')' => lvl match
      case 0 => isMarked(r, at + 1, hi)
      case _ => isNullable(r, at + 1, hi, lvl - 1)
    case '(' => isNullable(r, at + 1, hi, lvl + 1)
    case '\\' => isNullable(r, at + 2, hi, lvl)
    case _    => isNullable(r, at + 1, hi, lvl)
// end section regexisNullable

def isMarked(r: String, at: Int, hi: Int): Boolean =
  at match
    case `hi` => false
    case _ =>
      r.charAt(at) match
        case '?' | '*' => true
        case '{' => r.charAt(at + 1) match
          case '0' => true
          case _ => false
        case '+' => isMarked(r, at + 1, hi)
        case _ => false

def isPiped(r: String, at: Int, hi: Int, lvl: Int): Int =
  at match
    case `hi` => 0
    case _ => r.charAt(at) match
      case '|' => lvl match
        case 0 => 1
        case _ => isPiped(r, at + 1, hi, lvl)
      case ')' => lvl match
        case 0 => 0
        case _ => isPiped(r, at + 1, hi, lvl - 1)
      case '(' => isPiped(r, at + 1, hi, lvl + 1)
      case '\\' => isPiped(r, at + 2, hi, lvl)
      case _    => isPiped(r, at + 1, hi, lvl)

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

}
