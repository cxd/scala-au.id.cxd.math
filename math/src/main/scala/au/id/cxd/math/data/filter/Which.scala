package au.id.cxd.math.data.filter

/**
  * given a sequence return the indices that match a predicate
  * @tparam T
  */
class Which[T] {

  def op(data:Seq[T], predicate:T => Boolean):Seq[Int] = {
    /**
    val pairs = data.foldLeft((-1, Seq[Int]())) {
      (accum, d) => if (predicate(d)) (accum._1 + 1, accum._2 :+ accum._1 + 1)
                    else (accum._1 + 1, accum._2)
    }**/
    data.zipWithIndex.map {
      pair => (pair._2, predicate(pair._1))
    }.filter {
      pair => pair._2
    }.map {
      pair => pair._1
    }
  }

}

object Which {
  def apply[T](data:Seq[T], predicate:T => Boolean) =
    new Which[T]().op(data, predicate)
}
