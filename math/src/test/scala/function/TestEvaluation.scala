package function

trait TestEvaluation {

  /**
    * evaluate a function taking one parameter
    * @param fn
    * @param params
    * @param results
    * @param threshold
    * @return
    */
  def evaluate1(fn:Double => Double, params:List[Double], results:List[Double], threshold:Double) = {
    val output = params.map { parm => {
      // allow break point
      val result = fn(parm)
      result
    }}
    val tests = results.zip(output)
    tests.foreach {
      pair => println(s"${pair._1} ~ ${pair._2}")
    }
    tests.forall {
      pair => Math.abs(pair._1 - pair._2) < threshold
    }
  }

  /**
    * evaluate a function with 2 parameters
    * @param fn
    * @param params
    * @param results
    * @param threshold
    * @return
    */
  def evaluate2(fn:(Double, Double) => Double, params:List[(Double,Double)], results:List[Double], threshold:Double) = {
    val output = params.map { pair => {
      // allow break point
      val result = fn(pair._1, pair._2)
      result
    }}
    val tests = results.zip(output)
    tests.foreach {
      pair => println(s"${pair._1} ~ ${pair._2}")
    }
    tests.forall {
      pair => Math.abs(pair._1 - pair._2) < threshold
    }
  }
}
