import creator._
object sheet1 {
   val dist = List(("A",1.0),("B",4.0), ("D",5.0))
   val rvg : RandomValueGenerator[String] = new RandomValueGenerator(dist) with SerialUniformGenerator
   for (i<-1 to 11)
   {
      println(i.toString() + " : " + rvg.getNext())
   }
   //val res = ((1 to 20).map((i: Int) => rvg.getNext()))//.toList.groupBy(identity).mapValues(_.size)
}