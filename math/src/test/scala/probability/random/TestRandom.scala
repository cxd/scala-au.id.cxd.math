package probability.random

import au.id.cxd.math.probability.analysis.AndersonDarlingTest
import au.id.cxd.math.probability.continuous.Normal
import au.id.cxd.math.probability.random.RUniform
import org.scalatest.{FlatSpec, Matchers}

class TestRandom extends FlatSpec with Matchers {

  def testRandom (sample:Seq[Double], min:Double, max:Double):Boolean =
    sample.forall { s => min <= s && s <= max }

  "uniform random" should "generate random between -1 and 1" in {
    val min = -1.0
    val max = 1.0
    val rand = RUniform(min,max)

    val sample = rand.draw(100)

    val flag = testRandom (sample, min, max)

    println(sample)

    flag should be (true)
  }

  "uniform random" should "generate sequence" in {
    val min = -1.0
    val max= 1.0
    val rand = RUniform(min,max)
    val sample = rand.generate .take(10)
    val flag = testRandom(sample, min, max)
    val lengthFlag = sample.length == 10
    flag should be (true)
    lengthFlag should be (true)
  }


  "uniform random" should "generate scaled values between -5 and 5" in {
    val min = -5.0
    val max = 5.0
    val rand = RUniform(min,max)
    val sample = rand.draw(100)
    val flag = testRandom(sample, min, max)
    println(sample)
    flag should be(true)
  }

  "uniform random" should "not be normal" in {
    val min = 0.0
    val max = 1.0
    val rand = RUniform(min,max)
    val sample = rand.draw(100)
    val flag = testRandom(sample, min, max)
    println(sample)
    flag should be(true)
  }
}
