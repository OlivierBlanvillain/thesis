import java.lang.{IllegalArgumentException => IAE}

// ----------------------------------------------------------------------------
// Spark API
// ----------------------------------------------------------------------------
// From https://spark.apache.org/docs/2.3.0/api/java/index.html?org/apache/spark/sql/Dataset.html
// with type DataFrame = Dataset[Row]
abstract class DataFrame {
  /** Selects a set of column based expressions. */
  def select(col: String, cols: String*): DataFrame

  /** Filters rows using the given SQL expression. */
  def where(condition: String): DataFrame

  /** Inner join with another DataFrame on the given column. */
  def join(right: DataFrame, col: String): DataFrame
}


// // ----------------------------------------------------------------------------
// // Implicitly typed API
// // ----------------------------------------------------------------------------
// sealed trait HList
// trait HNil extends HList
// trait HCons[+H, +T <: HList] extends HList

// trait Find[K, L <: HList] { type Out }
// object Find {
//   implicit def i0[K <: String, V, L <: HList]:
//     Find[K, HCons[IColumn[K, V], L]] { type Out = V } = null

//   implicit def i1[K <: String, V, H, T <: HList]
//     (implicit f: Find[K, T] { type Out = V }):
//       Find[K, HCons[H, T]] { type Out = V } = null
// }

// trait Remove[K <: String, L <: HList] { type Out <: HList }
// object Remove {
//   implicit def i0[K <: String, V, T <: HList]:
//     Remove[K, HCons[IColumn[K, V], T]] { type Out = T } = null

//   implicit def i1[K <: String, R <: HList, H, T <: HList]
//     (implicit r: Remove[K, T] { type Out = R }):
//       Remove[K, HCons[H, T]] { type Out = HCons[H, R] } = null
// }

// trait Concat[L1 <: HList, L2 <: HList] { type Out <: HList }
// object Concat {
//   implicit def i0[L <: HList]:
//     Concat[HNil, L] { type Out = L } = null

//   implicit def i1[H, T <: HList, L <: HList, O <: HList]
//     (implicit c: Concat[T, L] { type Out = O }):
//       Concat[HCons[H, T], L] { type Out = HCons[H, O] } = null
// }

// trait Select[CS <: HList, L <: HList] { type Out <: HList }
// object Select extends SelectLowPrio {
//   implicit def i0[L <: HList]:
//     Select[HNil, L] { type Out = HNil } = null

//   implicit def i1[K <: String, V, T <: HList, L <: HList, O <: HList]
//     (implicit s: Select[T, L] { type Out = O }):
//       Select[HCons[K, T], HCons[IColumn[K, V], L]] { type Out = HCons[IColumn[K, V], O] } = null
// }
// trait SelectLowPrio {
//   implicit def i2[CS <: HList, H, T <: HList, O <: HList]
//     (implicit s: Select[CS, T] { type Out = O }):
//       Select[CS, HCons[H, T]] { type Out = O } = null
// }

// case class IColumn[S <: String, T](s: S)

// class ITable[T <: HList] {
//   def apply[S <: String, O](col: S)(implicit f: Find[S, T] { type Out = O }): IColumn[S, O] =
//     IColumn(col)

//   def select[CS <: HList, O <: HList](cols: CS)(implicit s: Select[CS, T] { type Out = O }): ITable[O] =
//     new ITable()

//   def where(condition: IColumn[_, Boolean]): ITable[T] =
//     new ITable()

//   def join[U <: HList, S <: String, TR <: HList, UR <: HList, C <: HList, S1](right: ITable[U], col: S)
//     (implicit
//       i0: Remove[col.type, T] { type Out = TR },
//       i1: Remove[col.type, U] { type Out = UR },
//       i2: Concat[TR, UR]      { type Out = C },
//       i3: Find[col.type, T]   { type Out = S1 }
//     ): ITable[HCons[IColumn[col.type, S1], C]] =
//       new ITable()
// }

// ----------------------------------------------------------------------------
// Dependently typed API
// ----------------------------------------------------------------------------

dependent case class DColumn[V](k: String) {
  dependent override def equals(other: Any): Boolean =
    other.isInstanceOf[DColumn[_]] && other.asInstanceOf[DColumn[_]].k == this.k
}

sealed trait Lst {
  dependent def find(e: Any): Any =
    if (this.isInstanceOf[Cons]) {
      val head = this.asInstanceOf[Cons].head
      val tail = this.asInstanceOf[Cons].tail
    // case Cons(head, tail) =>
      if (e.equals(head)) head
      else tail.find(e)
    } else {
      throw new IAE(s"$e not found in $this")
    }

  dependent def remove(e: Any): Lst =
    if (this.isInstanceOf[Cons]) {
      val head = this.asInstanceOf[Cons].head
      val tail = this.asInstanceOf[Cons].tail
    // case Cons(head, tail) =>
      if (e.equals(head)) tail
      else Cons(head, tail.remove(e))
    } else {
      throw new IAE(s"$e not found in $this")
    }

  dependent def concat(other: Lst): Lst =
    if (this.isInstanceOf[Cons]) {
      val head = this.asInstanceOf[Cons].head
      val tail = this.asInstanceOf[Cons].tail
    // case Cons(head, tail) =>
      Cons(head, tail.concat(other))
    } else {
    // case _ =>
      other
    }

  dependent def map(f: Any => Any): Lst =
    if (this.isInstanceOf[Cons]) {
      val head = this.asInstanceOf[Cons].head
      val tail = this.asInstanceOf[Cons].tail
    // case Cons(head, tail) =>
      Cons(f(head), tail.map(f))
    } else {
      Nil()
    }
}
dependent case class Cons(head: Any, tail: Lst) extends Lst
dependent case class Nil() extends Lst

dependent case class DTable(schema: Lst) {
  dependent def apply(col: String) =
    schema.find(DColumn(col))

  dependent def select(cols: Cons): DTable =
    DTable(cols.map { case s: String => this.apply })

  dependent def where(condition: DColumn[Boolean]): DTable =
    DTable(schema)

  dependent def join(right: DTable, col: DColumn[_]): DTable = {
    val s1 = this.schema.remove(col)
    val s2 = right.schema.remove(col)
    DTable(Cons(col, s1.concat(s2)))
  }
}

// object Bench {
//   dependent val s1 =
//     Cons(1, //X
//       Cons(42,
//         HNil()
//       )
//     ) //X

//   dependent val s2 =
//     Cons(2, //X
//       Cons(42,
//         HNil()
//       )
//     ) //X

//   val joinded = join(42, s1, s2)
// }
