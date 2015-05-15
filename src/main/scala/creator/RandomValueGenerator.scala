package creator

/**
 * Created by orotem on 5/8/2015.
 */

trait UniformGenerator {
   protected def maxValue : Int = 1000
   def getNextUniformValue() : Int // returns values between 1 and maxValue, uniformly distributed
}

trait RandomUniformGenerator extends UniformGenerator
{
   val rand = scala.util.Random
   override def getNextUniformValue() : Int = 1 + rand.nextInt(maxValue)
}

// this is an implementation for testing the correctness of RandomValueGenerator
trait SerialUniformGenerator extends UniformGenerator
{
   private var currValue : Int  = 0
   override def getNextUniformValue() : Int = {
      currValue+=1
      if (currValue>maxValue) {
         currValue = 1
      }
      currValue
   }
}

class RandomValueGenerator[T](Distribution: List[(T,Int)]) extends UniformGenerator with RandomUniformGenerator {
   // sum the weights
   if (Distribution.map(_._2).sum!=maxValue) {throw new Exception("Total of weights must be " + maxValue.toString())}
   val dummyElement: T = null.asInstanceOf[T]
   // accumDistribution -> running cumulative weights, for easier selection
   val accumDistribution : List[(T, Int)] = {
      Distribution.scanLeft((dummyElement,0))( (prev: (T, Int), curr: (T, Int)) => (curr._1, curr._2+prev._2)).tail
   }

   def getNext(): T = {
      val nextWeightValue : Int  = getNextUniformValue()
      // find the first element with accum weight >= weight, and return its value
      accumDistribution.filter(_._2>=nextWeightValue).head._1
   }
   override def toString() = Distribution.mkString("<",",",">")
}

