import java.util.regex.Pattern
import annotation.experimental
import compiletime.ops.string.{+ => ++, _}
import compiletime.ops.int.{+, -, Max, <}
import Lib._

@experimental
object Regex {
  def apply[R <: String & Singleton](regex: R): Regex[Compile[R]] =
    new Regex(regex)

  type Compile[R <: String] =
    FlattenReverse[Loop[R, 0, Length[R], HNil, 0]]

  type Charat[R <: String, At <: Int] =
    Substring[R, At, At + 1]

  type Nullmark[R <: String, At <: Int, Hi <: Int] =
    At match
      case Hi => false
      case _ =>
        Charat[R, At] match
          case "?" | "*" | "|" => true
          case "+" => Nullmark[R, At + 1, Hi]
          case _ => false

  type Nullable[R <: String, At <: Int, Hi <: Int] =
    At match
      case 1 => Nullable0[R, At, Hi, 0]
      case _ => Charat[R, At - 2] match
        case "|" => true
        case _ => Nullable0[R, At, Hi, 0]

  type Nullable0[R <: String, At <: Int, Hi <: Int, Ops <: Int] =
    At match
      case Hi => false
      case _ =>
        Charat[R, At] match
          case ")" => Ops match
            case 0 => Nullmark[R, At + 1, Hi]
            case _ => Nullable0[R, At + 1, Hi, Ops - 1]
          case "(" => Nullable0[R, At + 1, Hi, Ops + 1]
          case _   => Nullable0[R, At + 1, Hi, Ops]

  type Loop[R <: String, Lo <: Int, Hi <: Int, Acc <: HList, Nw <: Int] <: HList =
    Lo match
      case Hi => Acc
      case _   => Charat[R, Lo] match
        case ")"  =>
          Loop[R, Lo + 1, Hi, Acc, Max[0, Nw - 1]]
        case "("  =>
          Nw match
            case 0 =>
              Nullable[R, Lo + 1, Hi] match
                case true =>
                  Loop[R, Lo + 1, Hi, Option[String] :: Acc, 1]
                case false =>
                  Loop[R, Lo + 1, Hi, String :: Acc, 0]
            case _ =>
              Loop[R, Lo + 1, Hi, Option[String] :: Acc, Nw + 1]
        case "\\" =>
          Lo + 1 match
            case Hi => Acc
            case _ => Loop[R, Lo + 2, Hi, Acc, Nw]
        case _    => Loop[R, Lo + 1, Hi, Acc, Nw]

  // trait Parser[T <: HList] {
  //   def mutate(arr: Array[String]): Unit
  //   val i: Int
  // }

  // object Parser {
  //   implicit val basecase: Parser[HNil] =
  //     new Parser[HNil] {
  //       val i = 0
  //       def mutate(arr: Array[String]): Unit = ()
  //     }

  //   implicit def stringcase[T <: HList]
  //     (implicit ev: Parser[T]): Parser[String :: T] =
  //       new Parser[String :: T] {
  //         val i = ev.i + 1
  //         def mutate(arr: Array[String]): Unit =
  //           ev.mutate(arr)
  //       }

  //   implicit def optioncase[T <: HList]
  //     (implicit ev: Parser[T]): Parser[Option[String] :: T] =
  //       new Parser[Option[String] :: T] {
  //         val i = ev.i + 1
  //         def mutate(arr: Array[String]): Unit = {
  //           ev.mutate(arr)
  //           arr(i) = Option(arr(i))
  //         }
  //       }
  // }

  // val p: Parser[String :: Option[String] :: String :: HNil] = implicitly[Parser[String :: Option[String] :: String :: HNil]](
  //   stringcase(optioncase(stringcase(basecase())))

  //   val ev =     new Parser[String :: T] {
  //     val size =
  //       new Parser[HNil] {
  //           def mutate(arr: Array[String]): Unit = ()
  //           val size = 0
  //         }
  //       .size + 1
  //     def mutate(arr: Array[String]): Unit = ()
  //   }
  //   new Parser[Option[String] :: T] {
  //     val size = ev.size + 1
  //     def mutate(arr: Array[String]): Unit =
  //       ev.mutate(arr)
  //       arr(size) = Option(arr(size))
  //   }



  // )
  // val arr = Array("a", "b", "c")
  // p.mutate(arr)
  // assert(arr == Array("a", Option("b"), "c"))




  def transform(pattern: String, arr: Array[String]): Array[Any] = {
    val fns = loop(pattern, 0, pattern.length, Nil, 0).reverse
    assert(arr.size == fns.size)
    arr.zip(fns).map { (x, f) => f(x) }
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
                  loop(r, lo + 1, hi, (Option.apply[String] _) :: acc, 1)
                case false =>
                  loop(r, lo + 1, hi, (x => x) :: acc, 0)
            case _ =>
              loop(r, lo + 1, hi, Option[String] :: acc, nw + 1)
        case '\\' =>
          lo + 1 match
            case `hi` => acc
            case _ => loop(r, lo + 2, hi, acc, nw)
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
          case _   => nullable0(r, at + 1, hi, ops)
}

@experimental
class Regex[P] private (val regex: String) {
  val pattern = Pattern.compile(regex)
  def unapply(s: String): Option[P] = {
    val m = pattern.matcher(s)
    if (m.matches())
      Some {
        val a = Array.tabulate(m.groupCount) { i => m.group(i + 1) }
        Tuple.fromArray(Regex.transform(regex, a)).asInstanceOf[P]
      }
    else
      None
  }
}

object Lib {
  sealed trait HList
  case class ::[+H, +T <: HList](h: H, t: T) extends HList
  case object HNil extends HList
  type HNil = HNil.type

  type FlattenReverse[L <: HList] = L match {
    case HNil => Unit
    case x1 :: HNil => x1
    case x1 :: x2 :: HNil => (x2, x1)
    case x1 :: x2 :: x3 :: HNil => (x3, x2, x1)
    case x1 :: x2 :: x3 :: x4 :: HNil => (x4, x3, x2, x1)
    case x1 :: x2 :: x3 :: x4 :: x5 :: HNil => (x5, x4, x3, x2, x1)
    case x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: HNil => (x6, x5, x4, x3, x2, x1)
    case x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: HNil => (x7, x6, x5, x4, x3, x2, x1)
    case x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: x8 :: HNil => (x8, x7, x6, x5, x4, x3, x2, x1)
    case x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: x8 :: x9 :: HNil => (x9, x8, x7, x6, x5, x4, x3, x2, x1)
    case x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: x8 :: x9 :: x10 :: HNil => (x10, x9, x8, x7, x6, x5, x4, x3, x2, x1)
    case x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: x8 :: x9 :: x10 :: x11 :: HNil => (x11, x10, x9, x8, x7, x6, x5, x4, x3, x2, x1)
    case x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: x8 :: x9 :: x10 :: x11 :: x12 :: HNil => (x12, x11, x10, x9, x8, x7, x6, x5, x4, x3, x2, x1)
    case x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: x8 :: x9 :: x10 :: x11 :: x12 :: x13 :: HNil => (x13, x12, x11, x10, x9, x8, x7, x6, x5, x4, x3, x2, x1)
    case x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: x8 :: x9 :: x10 :: x11 :: x12 :: x13 :: x14 :: HNil => (x14, x13, x12, x11, x10, x9, x8, x7, x6, x5, x4, x3, x2, x1)
    case x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: x8 :: x9 :: x10 :: x11 :: x12 :: x13 :: x14 :: x15 :: HNil => (x15, x14, x13, x12, x11, x10, x9, x8, x7, x6, x5, x4, x3, x2, x1)
    case x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: x8 :: x9 :: x10 :: x11 :: x12 :: x13 :: x14 :: x15 :: x16 :: HNil => (x16, x15, x14, x13, x12, x11, x10, x9, x8, x7, x6, x5, x4, x3, x2, x1)
    case x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: x8 :: x9 :: x10 :: x11 :: x12 :: x13 :: x14 :: x15 :: x16 :: x17 :: HNil => (x17, x16, x15, x14, x13, x12, x11, x10, x9, x8, x7, x6, x5, x4, x3, x2, x1)
    case x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: x8 :: x9 :: x10 :: x11 :: x12 :: x13 :: x14 :: x15 :: x16 :: x17 :: x18 :: HNil => (x18, x17, x16, x15, x14, x13, x12, x11, x10, x9, x8, x7, x6, x5, x4, x3, x2, x1)
    case x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: x8 :: x9 :: x10 :: x11 :: x12 :: x13 :: x14 :: x15 :: x16 :: x17 :: x18 :: x19 :: HNil => (x19, x18, x17, x16, x15, x14, x13, x12, x11, x10, x9, x8, x7, x6, x5, x4, x3, x2, x1)
    case x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: x8 :: x9 :: x10 :: x11 :: x12 :: x13 :: x14 :: x15 :: x16 :: x17 :: x18 :: x19 :: x20 :: HNil => (x20, x19, x18, x17, x16, x15, x14, x13, x12, x11, x10, x9, x8, x7, x6, x5, x4, x3, x2, x1)
    case x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: x8 :: x9 :: x10 :: x11 :: x12 :: x13 :: x14 :: x15 :: x16 :: x17 :: x18 :: x19 :: x20 :: x21 :: HNil => (x21, x20, x19, x18, x17, x16, x15, x14, x13, x12, x11, x10, x9, x8, x7, x6, x5, x4, x3, x2, x1)
    case x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: x8 :: x9 :: x10 :: x11 :: x12 :: x13 :: x14 :: x15 :: x16 :: x17 :: x18 :: x19 :: x20 :: x21 :: x22 :: HNil => (x22, x21, x20, x19, x18, x17, x16, x15, x14, x13, x12, x11, x10, x9, x8, x7, x6, x5, x4, x3, x2, x1)
  }
}
