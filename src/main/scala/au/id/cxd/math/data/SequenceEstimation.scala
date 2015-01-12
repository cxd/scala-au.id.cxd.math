package au.id.cxd.math.data

/**
 * Created by cd on 12/01/15.
 */
class SequenceEstimation {

  /**
   * extract states from the supplied data
   * @param dataSet
   */
  def extractStateSequence (dataSet:List[List[String]]):List[String] = {
    dataSet.foldLeft(List[String]()) {
      (accum, data) =>{
        val state = data.reverse.head
        state :: accum
      }
    }.distinct
      .sorted
  }


  /**
   * extract all sequences whose last "term" (the item before the state) is equal to the end term.
   * @param dataSet
   * @param endTerm
   * @return
   */
  def extractEndTermSeq (dataSet:List[List[String]]) (endTerm:String): List[List[String]] = {
    dataSet.foldLeft(List[List[String]]()) {
      (accum, data) => {
        val state = data.reverse.head
        val terms = data.reverse.tail.reverse
        state match {
          case endTerm => terms :: accum
          case _ => accum
        }
      }
    }
  }

  /**
   * for each state in states count the frequency and return a vector
   * this will add 1 to every state so there are no 0 values
   * @param dataSet
   * @param states
   * @return
   */
  def countStateFreq (dataSet:List[List[String]]) (states:List[String]) : List[(Double, String)] = {
    states.map {
      state => {
        dataSet.foldLeft (1.0, state) {
          (pair, data) => {
            val (n, state2) = pair
            val other = data.reverse.head
            other == state match {
              case true => (n + 1.0, state2)
              case _ => (n, state2)
            }
          }
        }
      }
    }
  }

  /**
   * calculate the priors for the states parameter pi
   * @param dataSet
   * @param states
   * @return
   */
  def statePriors (dataSet:List[List[String]]) (states:List[String]) : List[(Double, String)] = {
    val frequencies = countStateFreq(dataSet)(states)
    val sum = frequencies.foldLeft (0.0) {
      (total, pair) => {
        val (n, state) = pair
        n + total
      }
    }
    frequencies.map {
      pair => {
        val (n, state) = pair
        (n / sum, state)
      }
    }
  }

  /**
   * test whether a string matches b string
   * @param a
   * @param b
   * @return
   */
  def matches (a:String) (b:String) = {
    a.equalsIgnoreCase(b)
  }

  /**
   * count the number of items different between lists
    listA should be length = listB length
   * @param listA
   * @param listB
   * @return
   */
  def countDelta (listA:List[String]) (listB:List[String]) : Double = {
    val len = listA.length
    val test = listB.take(len)
    val pairs = listA.zip(test)
    pairs.foldLeft(0.0) {
      (n, pair) => {
        val (itemA, itemB) = pair
        matches (itemA) (itemB) match {
          case true => n
          case false => n + 1.0
        }
      }
    }
  }

  /**
   * count the number of items different between lists
    listA should be length <= listB length
    This includes the state class and removes it
   * @param listA
   * @param listB
   * @return
   */
  def countDeltaState (listA:List[String]) (listB:List[String]) : Double = {
    val arr = listA.toIndexedSeq
    val test = listB.indices.map {
      i => {
        if (i >= (listA.length - 1)) ""
        else arr.apply(i)
      }
    }
    val pairs = test.zip(listB)
    pairs.foldLeft (0.0) {
      (n, pair) => {
        val (itemA, itemB) = pair
        matches (itemA) (itemB) match {
          case true => n + 1.0
          case _ => n
        }
      }
    }
  }


  /**
   * count transitions from stateA to stateB
    this will always add 1 to every transition so there is no 0 values
   * @param dataSet
   * @param stateA
   * @param stateB
   * @return
   */
  def countTransitionFreq (dataSet:List[List[String]]) (stateA:String) (stateB:String) : Double = {
    0.0
  }

}
