import creator._
object sheet1 {
   val dist = List(("A",200),("B",800), ("C",0))
   val rvg : RandomValueGenerator[String] = new RandomValueGenerator(dist) with SerialUniformGenerator
   val res = ((1 to 2000).map((i: Int) => rvg.getNext())).toList.groupBy(identity).mapValues(_.size)
}