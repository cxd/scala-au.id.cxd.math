package au.id.cxd.math.function.series

trait Sliding {

  /**
    * given an initial state S, a series of T and a stride width
    * create a sliding window of width by and apply the supplied function.
    * Returning the state resulting from the supplied function.
    * @param state
    * @param series
    * @param stride
    * @param by
    * @param fn
    * @tparam T
    * @tparam S
    * @return
    */
  def window[T,S](state:S, series:Seq[T], stride:Int, by:Int=1)(fn:(Int,Int,S,T) => S):S =
    series.iterator
      .sliding(stride, step=by)
      .foldLeft (0,0,state) {
      (accum, slice) => {
        val (i,j,s1) = accum
        val updateS = slice.foldLeft (j,s1) {
          (innerAccum, item) => {
            val newS = fn(i,innerAccum._1,innerAccum._2, item)
            (innerAccum._1+by,newS)
          }
        }
        (i + 1, 0, updateS._2)
      }
    }._3

}
