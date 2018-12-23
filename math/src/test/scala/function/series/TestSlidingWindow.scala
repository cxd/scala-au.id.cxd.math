package function.series

import au.id.cxd.math.function.series.{Sliding, WindowedMatrix}
import org.scalatest.{FlatSpec, Matchers}

class TestSlidingWindow extends FlatSpec with Matchers {

  "sliding window" should "have rows: N - M + 1" in new Sliding {

    def process (i:Int, j:Int, state:List[Int], item:Int):List[Int] = {
      val newState = state :+ item
      newState
    }

    val data = 1 to 100
    val stride:Int = 10
    val rows = data.size / stride
    val newSize = rows * (data.length - stride + 1)
    val newState = window(List[Int](), data, stride) (process (_,_,_,_))
    newState.size should be(newSize)
  }


  "sliding window" should "convert to matrix" in new WindowedMatrix {
    val data = (1.0 to 100.0 by 1.0)
    val cols:Int = 10
    val rows = data.length - cols + 1
    val newState = windowedMatrix(data, cols)
    newState.rows should be(rows)
    newState.cols should be(cols)
    println(newState)
  }

}
