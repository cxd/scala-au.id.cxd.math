package au.id.cxd.math.data

import breeze.linalg._
import breeze.math._
import breeze.numerics._

/**
 * Created by cd on 12/01/15.
 */
class SequenceEstimation {

  /**
   * extract states from the supplied data
   * @param dataSet
   */
  def extractStateSequence(dataSet: List[List[String]]): List[String] = {
    dataSet.foldLeft(List[String]()) {
      (accum, data) => {
        val state = data.reverse.head
        state :: accum
      }
    }.distinct
      .sorted
  }

  /**
   * extract states from the supplied data
   * @param dataSet
   */
  def extractStateSeq(dataSet: List[List[String]])(state: String): List[List[String]] = {
    dataSet.foldLeft(List[List[String]]()) {
      (accum, data) => {
        val state2 = data.reverse.head
        state2.equalsIgnoreCase(state) match {
          case true => data :: accum
          case _ => accum
        }
      }
    }.distinct
  }

  /**
   * extract all sequences whose last "term" (the item before the state) is equal to the end term.
   * @param dataSet
   * @param endTerm
   * @return
   */
  def extractEndTermSeq(dataSet: List[List[String]])(endTerm: String): List[List[String]] = {
    dataSet.foldLeft(List[List[String]]()) {
      (accum, data) => {
        val state = data.reverse.head
        val terms = data.reverse.tail.reverse
        state match {
            // matching against parameter
          case `endTerm` => terms :: accum
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
  def countStateFreq(dataSet: List[List[String]])(states: List[String]): List[(Double, String)] = {
    states.map {
      state => {
        dataSet.foldLeft(1.0, state) {
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
  def statePriorPairs(dataSet: List[List[String]])(states: List[String]): List[(Double, String)] = {
    val frequencies = countStateFreq(dataSet)(states)
    val sum = frequencies.foldLeft(0.0) {
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
   * return the state priors without the tuple for state label
   * @param dataSet
   * @param states
   * @return
   */
  def statePriors(dataSet:List[List[String]])(states:List[String]):List[Double] = {
    statePriorPairs (dataSet)(states).map {
      (pair) => pair._1
    }
  }

  /**
   * test whether a string matches b string
   * @param a
   * @param b
   * @return
   */
  def matches(a: String)(b: String) = {
    a.equalsIgnoreCase(b)
  }

  /**
   * count the number of items different between lists
    listA should be length = listB length
   * @param listA
   * @param listB
   * @return
   */
  def countDelta(listA: List[String])(listB: List[String]): Double = {
    val len = listA.length
    val test = listB.take(len)
    val pairs = listA.zip(test)
    pairs.foldLeft(0.0) {
      (n, pair) => {
        val (itemA, itemB) = pair
        matches(itemA)(itemB) match {
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
  def countDeltaState(listA: List[String])(listB: List[String]): Double = {
    val arr = listA.toIndexedSeq
    val test = listB.indices.map {
      i => {
        if (i >= (listA.length - 1)) ""
        else arr(i)
      }
    }
    val pairs = test.zip(listB)
    pairs.foldLeft(0.0) {
      (n, pair) => {
        val (itemA, itemB) = pair
        matches(itemA)(itemB) match {
          case true => n
          case _ => n + 1.0
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
  def countTransitionFreq(dataSet: List[List[String]])(stateA: String)(stateB: String): Double = {
    // find sequences in the data set that end in the supplied state
    def findSeq(state: String) = {
      dataSet.foldLeft(List[List[String]]()) {
        (accum, data) => {
          val otherState = data.reverse.head
          otherState.equalsIgnoreCase(state) match {
            case true => data :: accum
            case _ => accum
          }
        }
      }
    }

    val startSeqs = findSeq(stateA)
    val endSeqs = findSeq(stateB)

    val freqs = startSeqs.map {
      startSeq => {
        // find others 1 longer than start
        val otherSeqs = endSeqs.filter {
          end => end.length == (startSeq.length + 1)
        }
        otherSeqs.foldLeft(1.0) {
          (n, otherSeq) => {
            val delta = countDeltaState(startSeq)(otherSeq)
            delta > 2 match {
              case false => n + 1.0
              case _ => n
            }
          }
        }
      }
    }
    freqs.length == 0 match {
      case true => 1.0
      case _ => freqs.sum
    }
  }

  /**
   * count transitions between states
   * from stateA (i) to stateB (j)
   * @param dataSet
   * @param states
   * @return
   */
  def countTransitions(dataSet: List[List[String]])(states: List[String]): DenseMatrix[Double] = {
    val stateArr = states.toIndexedSeq
    DenseMatrix.tabulate(states.length, states.length) {
      (i, j) => {
        val stateA = stateArr(i)
        val stateB = stateArr(j)
        countTransitionFreq(dataSet)(stateA)(stateB)
      }
    }
  }

  /**
   * determine the probability for the state transitions

    from stateA (i) to stateB (j)

    a_ij = P(x_j | x_i)

    Note each row will sum to 1.
   * @param dataSet
   * @param states
   * @return
   */
  def stateTransitions(dataSet: List[List[String]])(states: List[String]): DenseMatrix[Double] = {
    val mat = countTransitions(dataSet)(states)
    // normalise each row of the matrix to 1.0
    normalize(mat(*, ::), 1.0)
  }

  /**
   * count the occurance of the term in the sequence
    this will always add 1.0 to every term count so there are no 0 values.
   * @param items
   * @param term
   * @return
   */
  def countTermInSeq(items: List[String])(term: String): Double = {
    items.foldLeft(1.0) {
      (n, item) => {
        item.equalsIgnoreCase(term) match {
          case true => n + 1.0
          case _ => n
        }
      }
    }
  }

  /**
   * determine whether the end term matches the supplied term from the sequence

    The sequence is assumed to contain

    [term1; term2; ...; termN; state]

    This will reverse the term list and remove the state
    it will then determine whether termN matches the supplied term.

   * @param items
   * @param term
   * @return
   */
  def countTermInEndOfSeq(items: List[String])(term: String): Double = {
    val rev = items.reverse
    val endTerm = rev.tail.head
    endTerm.equalsIgnoreCase(term) match {
      case true => 2.0
      case _ => 1.0
    }
  }

  /**
   * count the frequency of the term in the sequence
    return a tuple (double * double)
    where _1 = term frequency for supplied state
    _2 = total term frequency in subseq

    This method will always start with 1.0 adding 1.0 to every term count
    so that there is no 0.0 values
   * @param subseq
   * @param term
   * @param state
   * @return
   */
  def countTermInState(subseq: List[List[String]])(term: String)(state: String): (Double, Double) = {
    val stateSeq = extractStateSeq(subseq)(state)
    val stateCount =
      stateSeq.foldLeft(1.0) {
        (n, items) => {
          n + countTermInSeq(items)(term)
        }
      }
    val totalCount =
      subseq.foldLeft(1.0) {
        (n, items) => {
          n + countTermInSeq(items)(term)
        }
      }
    (stateCount, totalCount)
  }

  /**
   * this will count the total number of times a term appears at the end of a sequence
    for the supplied state label

    It will then count the total number of occurances of the term in all sequences

    and will return the tuple

    (endOfSeqCount, totalCount)
   * @param subseq
   * @param term
   * @param state
   * @return
   */
  def countTermAtEndOfState(subseq: List[List[String]])(term: String)(state: String): (Double, Double) = {
    val stateSeq = extractStateSeq(subseq)(state)
    val stateCount =
      stateSeq.foldLeft(1.0) {
        (n, items) => {
          n + countTermInEndOfSeq(items)(term)
        }
      }
    val totalCount =
      subseq.foldLeft(1.0) {
        (n, items) => {
          n + countTermInSeq(items)(term)
        }
      }
    (stateCount, totalCount)
  }

  /**
   * determine the prior evidence matrix P(e_i | x_j)
    for state transitions

    each row will normalise to 1.
   * @param data
   * @param terms
   * @param states
   * @return
   */
  def priorEvidence(data: List[List[String]])(terms: List[String])(states: List[String]): DenseMatrix[Double] = {
    val m = terms.length
    val n = states.length
    val termSeq = terms.toIndexedSeq
    val stateSeq = states.toIndexedSeq
    val mat = DenseMatrix.tabulate(m, n) {
      (i, j) => {
        val (freq, totalFreq) = countTermInState(data)(termSeq.apply(i))(stateSeq.apply(j))
        freq / totalFreq
      }
    }
    // normalise each row of the matrix to 1.0
    normalize(mat(*, ::), 1.0)
  }

  /**
   * calculate the prior evidences for all data sets
   * @param data
   * @param terms
   * @param states
   * @return
   */
  def priorEvidences(data: List[List[String]])(terms: List[String])(states: List[String]) = {
    terms.map {
      (term) => {
        val data1 = extractEndTermSeq(data)(term)
        priorEvidence(data1)(terms)(states)
      }
    }
  }

  /**
   * calculate the prior evidences for all data sets
   * assume each sample is independently and identically distributed
   * return the average prior evidences
   * @param data
   * @param terms
   * @param states
   * @return
   */
  def avgPriorEvidences(data: List[List[String]])(terms: List[String])(states: List[String]) = {
    val m = states.length
    val n = terms.length

    def innerPriorEvidence(data: List[List[String]])(terms: List[String])(states: List[String]):(DenseVector[Double], DenseMatrix[Double]) = {
      val stateArr = states.toIndexedSeq
      val termArr = terms.toIndexedSeq
      val result =
        (0 to (n - 1)).foldLeft(List[((Int, Int), (Double, Double))]()) {
          (accum, i) => {
            (0 to (m - 1)).foldLeft(accum) {
              (arr, j) => {
                val (freq, totalFreq) = countTermAtEndOfState(data)(termArr(i))(stateArr(j))

                ((i, j), (freq, totalFreq)) :: arr
              }
            }
          }
        }
      val totalVec = DenseVector.tabulate(n) {
        (i) => {
          result.foldLeft (0.0) {
            (n1, pair) => {
              val ((a, b), (f, t)) = pair
              (i == a) match {
                case true => {
                  n1 + t
                }
                case _ => n1
              }
            }
          }
        }
      }

      val mat = DenseMatrix.tabulate(n, m) {
        (i, j) => {
          val total = totalVec.apply(i)
          val freq =
            result.foldLeft (0.0) {
              (n1, pair) => {
                val ((a, b), (f, t)) = pair
                ((i == a) && (j == b)) match {
                  case true => f
                  case _ => n1
                }
              }
            }
          freq
        }
      }
      (totalVec, mat)
    }

    val pairs = innerPriorEvidence(data) (terms) (states)
    val V = pairs._1
    val B = pairs._2
    DenseMatrix.tabulate(n, m) {
      (i, j) => {
        val t = V(i)
        val f = B(i, j)
        f / t
      }
    }
  }

  /**
   * retrieve the indices for supplied index
   * @param modelSeq
   * @param examples
   * @return
   */
  def indices (modelSeq:List[String]) (examples:List[String]) = {
    examples.map {
      (item) => modelSeq.indexWhere { p => p.equalsIgnoreCase(item) }
    }
  }

}

object SequenceEstimation {
  def apply() = new SequenceEstimation()
}
