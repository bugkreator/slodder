package creator

/**
 * Created by orotem on 5/8/2015.
 */

trait UniformGenerator {
   def getNextUniformValue() : Double
}

trait RandomUniformGenerator extends UniformGenerator
{
   val rand = scala.util.Random
   override def getNextUniformValue() : Double = rand.nextDouble()
}

// this is an implementation for testing the correctness of RandomValueGenerator
trait SerialUniformGenerator extends UniformGenerator
{
   private val numSteps : Int = 100
   private val delta : Double = 1.0/numSteps
   private var currPointer : Int  = numSteps-1
   private val values = (0 to numSteps-1).map(_*delta)
   override def getNextUniformValue() : Double = {
      currPointer+=1
      if (currPointer>=numSteps) {
         currPointer = 0
      }
      println (currPointer, values(currPointer))
      values(currPointer)
   }
}

class RandomValueGenerator[T](Distribution: List[(T,Double)]) extends UniformGenerator with RandomUniformGenerator {
   // sum the weights
   val totalWeight = Distribution.map(_._2).sum
   //val (dummyElement, _) : (T, Double)  = Distribution.head
   val dummyElement: T = null.asInstanceOf[T]
   // accumDistribution -> running cumulative weights, for easier selection
   val accumDistribution : List[(T, Double)] = {
      Distribution.scanLeft((dummyElement,0.0))( (prev: (T, Double), curr: (T, Double)) => (curr._1, curr._2+prev._2)).tail
   }
   println(accumDistribution)

   def getNext(): T = {
      val nextWeightValue : Double  = getNextUniformValue() *totalWeight
      println("nextWeightValue : " + nextWeightValue.toString())
      // find the first element with accum prob >= weight, and return its value
      accumDistribution.filter(_._2>=nextWeightValue).head._1
   }
   override def toString() = Distribution.mkString("<",",",">")
}

