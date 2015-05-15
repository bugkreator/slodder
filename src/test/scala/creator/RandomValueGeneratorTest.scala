package creator

import org.scalatest.FunSuite

/**
 * Created by orotem on 5/15/2015.
 */
class RandomValueGeneratorTest extends FunSuite {
   private def AreMapsEqual(m1: Map[String, Int], m2: Map[String, Int]) : Boolean =
   {
      (m1.size == m2.size) && (m1.forall({case (k,v) => m2(k)==v}))
   }

   private def TestDistributionResults(Distribution: List[(String, Int)], NumValues: Int) : Map[String, Int] =
   {
      val rvg : RandomValueGenerator[String] = new RandomValueGenerator(Distribution) with SerialUniformGenerator
      // return frequencies from NumValues application of rvg
      ((1 to NumValues).map((i: Int) => rvg.getNext())).toList.groupBy(identity).mapValues(_.size)
   }

   private def assertDist(Distribution: List[(String, Int)], NumValues: Int, ExpectedResults : Map[String,Int]) : Unit =
   {
      assert (AreMapsEqual(ExpectedResults, TestDistributionResults(Distribution, NumValues)))
   }

   test ("Single Value") {
      assertDist(
         List(("A",1000)),
         1000,
         Map("A"->1000)
      )
   }

   test ("Two Equal Values") {
      assertDist(
         List(("A",500),("B",500)),
         1000,
         Map("A"->500, "B"->500)
      )
   }

   test ("Two Unequal Values") {
      assertDist(
         List(("A",200),("B",800)),
         5000,
         Map("A"->1000, "B"->4000)
      )
   }

   test ("A zero probability value") {
      assertDist(
         List(("A",200),("B",800), ("C",0)),
         5000,
         Map("A"->1000, "B"->4000)
      )
   }

   test ("Repeated Values") {
      assertDist(
         List(("A",200),("B",600), ("B",200)),
         4000,
         Map("A"->800, "B"->3200)
      )
   }

   test ("Multiple Values") {
      assertDist(
         List(("A",100),("B",300), ("C",600)),
         10000,
         Map("A"->1000, "B"->3000, "C"->6000)
      )
   }

}
