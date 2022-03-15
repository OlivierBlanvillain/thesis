def bestNeighborhood(phoneBook: TypedDataFrame[PhoneBookEntry], cityMap: TypedDataFrame[CityMapEntry]) =
  phoneBook
    .innerJoin(cityMap).using('address)     :TypedDataFrame[(Address, String, Double, String)]
    .select('_2, '_4)                       :TypedDataFrame[(String, String)]
    .as[Family]()                           :TypedDataFrame[Family]
    .flatMap(f => f.residents.split(' ').map(r => Person(r, f.neighborhood)))
                                            :TypedDataFrame[Person]
    .filter(p => NLPLib.uniqueName(p.name)) :TypedDataFrame[Person]
    .groupBy('neighborhood).count()         :TypedDataFrame[(String, Long)]
    .as[NeighborhoodCount]()                :TypedDataFrame[NeighborhoodCount]
    .sortDesc('count)                       :TypedDataFrame[NeighborhoodCount]
    .select('neighborhood)                  :TypedDataFrame[Tuple1[String]]
    .head._1



phoneBook                             : DF[PhoneBookEntry]
  .innerJoin(cityMap).using('address) : DF[(Address, String, String, ...)]
  .select('_2, '_3).as[Family]()      : DF[Family]
  .filter(x => uniqueName(x.name))    : DF[Family]
  .groupBy('neighborhood).count()     : DF[(String, Long)]
  .sortDesc('_2)                      : DF[(String, Long)]
  .head._1                            : String


phoneBook                                           : DataFrame
  .innerJoin(cityMap, col("address"))               : DataFrame
  .select(col("name"), col("neighborhood"))         : DataFrame
  .filter(x => uniqueName(x.getAs[String]("name"))) : DataFrame
  .groupBy(col("neighborhood")).count()             : DataFrame
  .sortDesc(col("count"))                           : DataFrame
  .head.getAs[String](0)                            : String



val captures: Option[Seq[String]] = rational.unapply("3.1415")
captures match
  case Some(seq) if seq(1) != null =>
    var n = seq(0).length + seq(1).length
    println(s"This number is $n digits long")
  case _ =>
    println("Not a number")

val rational = Regex("""(\d+)(?:\.(\d+))?""")
"3.1415" match
  case rational(captures: Seq[String]) =>
    val i = captures(0)
    val j = captures(1)

val rational = TypedRegex("""(\d+)(?:\.(\d+))?""")
"3.1415" match
  case rational(captures: (String, Option[String])) =>
    val i = captures._1
    val j = captures._2
