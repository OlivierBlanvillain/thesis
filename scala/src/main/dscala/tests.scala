object Test {
  def the[X](implicit x: X): x.type = x

  def dependentFindTest: Unit = {
    dependent def schema =
      Cons(DColumn[Int]("a"), Cons(DColumn[Double]("b"), Cons(DColumn[Boolean]("c"), Nil())))

    dependent def a = schema.find(DColumn("a"))
    val aa: DColumn[Int] = a
    dependent def b = schema.find(DColumn("b"))
    val bb: DColumn[Double] = b
    dependent def c = schema.find(DColumn("c"))
    val cc: DColumn[Boolean] = c
  }

  def implicitFindTest: Unit = {
    type Schema = HCons[Column["a", Int], HCons[Column["b", Double], HCons[Column["c", Boolean], HNil]]]
    val a = the[Find["a", Schema]]
    val aa: Find["a", Schema] { type Out = Int } = a
    val b = the[Find["b", Schema]]
    val bb: Find["b", Schema] { type Out = Double } = b
    val c = the[Find["c", Schema]]
    val cc: Find["c", Schema] { type Out = Boolean } = c
  }

  def dependentRemoveTest: Unit = {
    dependent def schema =
      Cons(DColumn[Int]("a"), Cons(DColumn[Double]("b"), Cons(DColumn[Boolean]("c"), Nil())))

    dependent def a = schema.remove(DColumn("a"))
    val aa: { Cons(DColumn[Double]("b"), Cons(DColumn[Boolean]("c"), Nil())) } = a
    dependent def b = schema.remove(DColumn("b"))
    val bb: { Cons(DColumn[Int]("a"), Cons(DColumn[Boolean]("c"), Nil())) } = b
    dependent def c = schema.remove(DColumn("c"))
    val cc: { Cons(DColumn[Int]("a"), Cons(DColumn[Double]("b"), Nil())) } = c
  }

  def implicitRemoveTest: Unit = {
    type Schema = HCons[Column["a", Int], HCons[Column["b", Double], HCons[Column["c", Boolean], HNil]]]
    val a = the[Remove["a", Schema]]
    val aa: Remove["a", Schema] { type Out = HCons[Column["b", Double], HCons[Column["c", Boolean], HNil]] } = a
    val b = the[Remove["b", Schema]]
    val bb: Remove["b", Schema] { type Out = HCons[Column["a", Int], HCons[Column["c", Boolean], HNil]] } = b
    val c = the[Remove["c", Schema]]
    val cc: Remove["c", Schema] { type Out = HCons[Column["a", Int], HCons[Column["b", Double], HNil]] } = c
  }

  def dependentConcatTest: Unit = {
    dependent def schema =
      Cons(DColumn[Int]("a"), Cons(DColumn[Double]("b"), Cons(DColumn[Boolean]("c"), Nil())))

    dependent def sa1 = Cons(DColumn[Int]("a"), Nil())
    dependent def sa2 = Cons(DColumn[Double]("b"), Cons(DColumn[Boolean]("c"), Nil()))
    dependent def sb1 = Cons(DColumn[Int]("a"), Cons(DColumn[Double]("b"), Nil()))
    dependent def sb2 = Cons(DColumn[Boolean]("c"), Nil())
    dependent def a = sa1.concat(sa2)
    val aa: { schema } = a
    dependent def b = sb1.concat(sb2)
    val bb: { schema } = b
    dependent def c = schema.concat(Nil())
    val cc: { schema } = c
    dependent def d = Nil().concat(schema)
    val dd: { schema } = d
  }

  def implicitConcatTest: Unit = {
    type Schema = HCons[Column["a", Int], HCons[Column["b", Double], HCons[Column["c", Boolean], HNil]]]
    type Sa1 = HCons[Column["a", Int], HNil]
    type Sa2 = HCons[Column["b", Double], HCons[Column["c", Boolean], HNil]]
    type Sb1 = HCons[Column["a", Int], HCons[Column["b", Double], HNil]]
    type Sb2 = HCons[Column["c", Boolean], HNil]
    val a = the[Concat[Sa1, Sa2]]
    val aa: Concat[Sa1, Sa2] { type Out = Schema } = a
    val b = the[Concat[Sb1, Sb2]]
    val bb: Concat[Sb1, Sb2] { type Out = Schema } = b
    val c = the[Concat[Schema, HNil]]
    val cc: Concat[Schema, HNil] { type Out = Schema } = c
    val d = the[Concat[HNil, Schema]]
    val dd: Concat[HNil, Schema] { type Out = Schema } = d
  }


  def dependentMapTest: Unit = {
    // TODO...
  }

  def implicitSelectTest: Unit = {
    type Schema = HCons[Column["a", Int], HCons[Column["b", Double], HCons[Column["c", Boolean], HNil]]]
    val a = the[Select[HCons["a", HNil], Schema]]
    val aa: Select[HCons["a", HNil], Schema] { type Out = HCons[Column["a", Int], HNil] } = a
    val b = the[Select[HCons["b", HNil], Schema]]
    val bb: Select[HCons["b", HNil], Schema] { type Out = HCons[Column["b", Double], HNil] } = b
    val c = the[Select[HCons["c", HNil], Schema]]
    val cc: Select[HCons["c", HNil], Schema] { type Out = HCons[Column["c", Boolean], HNil] } = c
    val ac = the[Select[HCons["a", HCons["c", HNil]], Schema]]
    val acac: Select[HCons["a", HCons["c", HNil]], Schema] { type Out = HCons[Column["a", Int], HCons[Column["c", Boolean], HNil]] } = ac
  }
}
