package creator

import org.scalatest.FunSuite

/**
 * Created by orotem on 5/15/2015.
 */
class RandomValueGeneratorTest extends FunSuite {
   private def AreMapsEqual(m1: Map[String, Double], m2: Map[String, Double]) : Boolean =
   {
      (m1.size == m2.size) && (m1.forall({case (k,v) => m2(k)==v}))
   }

   private def TestDistributionResults(Distribution: List[(String, Double)], NumValues: Int) : Map[String, Double] =
   {
      val rvg : RandomValueGenerator[String] = new RandomValueGenerator(Distribution) with SerialUniformGenerator
      // return frequencies from NumValues application of rvg
      ((1 to NumValues).map((i: Int) => rvg.getNext())).toList.groupBy(identity).mapValues(_.size)
   }

   private def assertDist(Distribution: List[(String, Double)], NumValues: Int, ExpectedResults : Map[String,Double]) : Unit =
   {
      assert (AreMapsEqual(ExpectedResults, TestDistributionResults(Distribution, NumValues)))
   }

   test ("Single Value") {
      assertDist(
         List(("A",1)),
         1000,
         Map("A"->1000)
      )
   }

   test ("Two Equal Values") {
      assertDist(
         List(("A",1),("B",1)),
         1000,
         Map("A"->500, "B"->500)
      )
   }

   test ("Two Unequal Values") {
      assertDist(
         List(("A",1),("B",4)),
         2500,
         Map("A"->500, "B"->2000)
      )
   }

   test ("A zero probability value") {
      assertDist(
         List(("A",1),("B",4), ("C",0)),
         2500,
         Map("A"->500, "B"->2000)
      )
   }

   test ("Repeated Values") {
      assertDist(
         List(("A",1),("B",3), ("B",1)),
         2500,
         Map("A"->500, "B"->2000)
      )
   }

   test ("Multiple Values") {
      println(TestDistributionResults(List(("A",1),("B",3), ("C",6)), 10000)) // this one fails - precision issue?
      assertDist(
         List(("A",1),("B",3), ("C",6)),
         10000,
         Map("A"->1000, "B"->3000, "C"->6000)
      )
   }

}
