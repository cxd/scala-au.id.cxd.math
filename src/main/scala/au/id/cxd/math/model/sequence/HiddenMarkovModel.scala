package au.id.cxd.math.model.sequence

import au.id.cxd.math.data.SequenceEstimation
import au.id.cxd.math.model.entity.hmm.{InputModel, Model, Prediction}
import breeze.linalg._
import breeze.math._
import breeze.numerics._
import breeze.linalg.operators._
import scala.collection.mutable._

/**
 * HiddenMarkovModel implementation, based on F# implementation in test-hmm.
 *
 *
 * Created by cd on 10/01/15.
 */
class HiddenMarkovModel {

  /**
   * forward equation

    T = time T
    pi = P(x_i)
    a_ij = P(x_j | x_i)
   b_ij = P(x_j | e_i)

   T represents i current state we need to predict j next state

   V - indices of evidence variables observed up to time T
   * @param T
   * @param pi
   * @param A
   * @param B
   * @param stateCount
   * @param evidenceCount
   * @param V
   * @return
   */
  def alpha(T: Int)(pi: List[Double])(A: DenseMatrix[Double])(B: DenseMatrix[Double])(stateCount: Int)(evidenceCount: Int)(V: List[Int]): DenseMatrix[Double] = {
    val localMap = Map[Int, DenseMatrix[Double]]()
    /**
     * local cache
     * @param i
     * @param methodFn
     * @return
     */
    def cache(i: Int)(methodFn: () => DenseMatrix[Double]) = {
      localMap.contains(i) match {
        case true => {
          localMap.get(i)
        }
        case _ => {
          val (mat: DenseMatrix[Double]) = methodFn()
          localMap += (i -> mat)
          mat
        }
      }
    }

    /**
     * recurse on
        time T
        with priors pi
        and state transition matrix A
        with evidence matrix B
        and index of transitions V
        accumulate in matrix accum
     */
    def alphaInner(T: Int)(pi: List[Double])(A: DenseMatrix[Double])(B: DenseMatrix[Double])(V: List[Int])(accum: DenseMatrix[Double]): DenseMatrix[Double] = {

      /**
       * compute new probabilities for evidence var at vk, at time t and update the row t in matrix accum

            \alpha_j(t) = b_{jk}v(t)\sum_{i=1}^c \alpha_i(t-1)a_{ij}

            vk = current evidence variable in V
            t = index of state at time t
       * @param vk
       * @param t
       * @param accum
       * @return
       */
      def inner(vk: Int)(t: Int)(accum: DenseMatrix[Double]): DenseMatrix[Double] = {

        val (alpha2: DenseMatrix[Double]) = cache(t - 1) { () => alphaInner(t - 1)(pi)(A)(B)(V)(accum)}


        val totals = DenseMatrix.tabulate(accum.rows, accum.cols) {
          (i, j) => {
            alpha2.apply(i, t - 1) * A.apply(t, j)
          }
        }
        // sum columns
        val sums = sum(totals(::, *))

        val total = DenseVector.tabulate(totals.cols) {
          (j) => sums.apply(0, j) * B.apply(vk, j)
        }
        accum(vk, ::) := total
        accum
      }
      T match {
        case 0 => accum
        case _ => {
          V.foldLeft(accum) {
            (accum1, k) => {
              val alpha1 = inner(k)(T)(accum)
              // scaling \hat{\alpha_t} = \frac{1}{\sum_i \bar{\alpha_t}(i)
              val scale = sum(sum(alpha1(::, *)))
              val alpha2 = alpha1 / scale
              alpha2
            }
          }
        }
      }
    }

    val accum = DenseMatrix.tabulate(evidenceCount, stateCount) {
      (t, j) => {
        val vlen = V.length
        (t < vlen) match {
          case true => {
            val idx = V.apply(t)
            pi.apply(j) * B.apply(idx, j)
          }
          case _ => pi.apply(j)
        }
      }
    }
    val accum1 = DenseMatrix.tabulate(evidenceCount, stateCount) {
      (i, j) => {
        val row = accum(i, ::)
        val c = sum(row)
        (c == 0.0) match {
          case true => accum.apply(i, j)
          case _ => accum.apply(i, j) / c
        }
      }
    }

    val alphaResult = alphaInner(T)(pi)(A)(B)(V)(accum1)
    localMap.empty
    // normalize rows
    normalize(alphaResult(*, ::), 1.0)
  }


  /**
   * forward equation

    T = time T
    pi = P(x_i)
    a_ij = P(x_j | x_i)
   b_ij = P(x_j | e_i)

   T represents i current state we need to predict j next state

   V - indices of evidence variables observed up to time T
   * @param T
   * @param pi
   * @param A
   * @param B
   * @param stateCount
   * @param evidenceCount
   * @param V
   * @return
   */
  def alphaUnscaled(T: Int)(pi: List[Double])(A: DenseMatrix[Double])(B: DenseMatrix[Double])(stateCount: Int)(evidenceCount: Int)(V: List[Int]): DenseMatrix[Double] = {

    // locally scoped cache
    val localMap = Map[Int, DenseMatrix[Double]]()

    /**
     * local cache
     * @param i
     * @param methodFn
     * @return
     */
    def cache(i: Int)(methodFn: () => DenseMatrix[Double]) = {
      localMap.contains(i) match {
        case true => {
          localMap.get(i)
        }
        case _ => {
          val (mat: DenseMatrix[Double]) = methodFn()
          localMap += (i -> mat)
          mat
        }
      }
    }

    val finalT = T

    /**
     * recurse on
        time T
        with priors pi
        and state transition matrix A
        with evidence matrix B
        and index of transitions V
        accumulate in matrix accum
     */
    def alphaInner(T: Int)(pi: List[Double])(A: DenseMatrix[Double])(B: DenseMatrix[Double])(V: List[Int])(accum: DenseMatrix[Double]): DenseMatrix[Double] = {

      /**
       * compute new probabilities for evidence var at vk, at time t and update the row t in matrix accum

            \alpha_j(t) = b_{jk}v(t)\sum_{i=1}^c \alpha_i(t-1)a_{ij}

            vk = current evidence variable in V
            t = index of state at time t
       * @param vk
       * @param t
       * @param accum
       * @return
       */
      def inner(vk: Int)(t: Int)(accum: DenseMatrix[Double]): DenseMatrix[Double] = {

        val (alpha2: DenseMatrix[Double]) = cache(t - 1) {
          () => alphaInner(t - 1)(pi)(A)(B)(V)(accum)
        }


        val totals = DenseMatrix.tabulate(accum.rows, accum.cols) {
          (i, j) => {
            alpha2.apply(i, t - 1) * A.apply(t, j)
          }
        }
        // sum columns
        val sums = sum(totals(::, *))

        val total = DenseVector.tabulate(totals.cols) {
          (j) => sums.apply(0, j) * B.apply(vk, j)
        }
        accum(vk, ::) := total
        accum
      }
      T match {
        case 0 => accum
        case _ => {
          V.foldLeft(accum) {
            (accum1, k) => {
              val alpha1 = inner(k)(T)(accum)
              (T == finalT) match {
                case true => {
                  // the unscaled equation will not scale the last iteration resulting in \bar{\alpha} rather than \hat{\alpha}
                  alpha1
                }
                case _ => {
                  // scaling \hat{\alpha_t} = \frac{1}{\sum_i \bar{\alpha_t}(i)
                  val scale = sum(sum(alpha1(::, *)))
                  val alpha2 = alpha1 / scale
                  alpha2
                }
              }
            }
          }
        }
      }
    }

    val accum = DenseMatrix.tabulate(evidenceCount, stateCount) {
      (t, j) => {
        val vlen = V.length
        (t < vlen) match {
          case true => {
            val idx = V.apply(t)
            pi.apply(j) * B.apply(idx, j)
          }
          case _ => pi.apply(j)
        }
      }
    }
    val accum1 = DenseMatrix.tabulate(evidenceCount, stateCount) {
      (i, j) => {
        val row = accum(i, ::)
        val c = sum(row)
        (c == 0.0) match {
          case true => accum.apply(i, j)
          case _ => accum.apply(i, j) / c
        }
      }
    }

    val alphaResult = alphaInner(T)(pi)(A)(B)(V)(accum1)
    localMap.empty
    // normalize rows
    normalize(alphaResult(*, ::), 1.0)
  }

  /**
   * this is the time reversed algorithm of alpha
    moving from t = 1 to T

    T = time T
    pi = P(x_i)
    a_ij = P(x_j | x_i)
   b_ij = P(x_j | e_i)

   T represents i current state we need to predict j next state

   V - indices of evidence variables observed up to time T
   * @param T
   * @param pi
   * @param A
   * @param B
   * @param stateCount
   * @param evidenceCount
   * @param V
   * @return
   */
  def beta(T: Int)(pi: List[Double])(A: DenseMatrix[Double])(B: DenseMatrix[Double])(stateCount: Int)(evidenceCount: Int)(V: List[Int]): DenseMatrix[Double] = {
    // locally scoped cache
    val localMap = Map[Int, DenseMatrix[Double]]()

    /**
     * local cache
     * @param i
     * @param methodFn
     * @return
     */
    def cache(i: Int)(methodFn: () => DenseMatrix[Double]) = {
      localMap.contains(i) match {
        case true => {
          localMap.get(i)
        }
        case _ => {
          val (mat: DenseMatrix[Double]) = methodFn()
          localMap += (i -> mat)
          mat
        }
      }
    }

    val alpha0 = alphaUnscaled(T)(pi)(A)(B)(stateCount)(evidenceCount)(V)

    def betaInner(T: Int)(t: Int)(pi: List[Double])(A: DenseMatrix[Double])(B: DenseMatrix[Double])(V: List[Int])(accum: DenseMatrix[Double]): DenseMatrix[Double] = {
      // perform the calculation
      /*
        \beta_i(t) = \sum_{j=1}^C \beta_j(t+1)a_{ij}b_{jk}v(t+1)
      */
      def inner(vk: Int)(t: Int)(accum: DenseMatrix[Double]): DenseMatrix[Double] = {
        val (beta2: DenseMatrix[Double]) = cache(t + 1) { () => betaInner(T)(t + 1)(pi)(A)(B)(V)(accum)}
        // calculate the scaling factor D_t = \prod_{\t=1}^T c_t
        val d = alpha0.toArray.reduce { (a, b) => a + b}
        // vk = i, t
        // \beta_i(t) = \sum_{j=1}^C \beta_j(t+1)a_{ij}b_{jk}v(t+1)
        val totals = DenseMatrix.tabulate(accum.rows, accum.cols) {
          (i, j) => {
            beta2.apply(i, t + 1) * A.apply(t, j) * B.apply(vk, t + 1)
          }
        }
        // sum columns
        val total = sum(totals(::, *))
        (d == 0.0) match {
          case true => accum
          case false => {
            accum(vk, *) := total / d
            accum
          }
        }
      }
      // general and recursive case
      (t >= (T - 1)) match {
        case true => accum
        case false =>
          V.foldLeft(accum) { (accum, k) => inner(k)(t + 1)(accum)}
      }
    }
    // initialising beta matrix with priors
    val accum = DenseMatrix.tabulate(evidenceCount, stateCount) {
      (t, j) => {
        val vlen = V.length
        (t < vlen) match {
          case true => {
            val idx = V.apply(t)
            pi.apply(j) * B.apply(idx, j)
          }
          case false => {
            pi.apply(j) / (evidenceCount.toDouble)
          }
        }
      }
    }

    val accum1 = DenseMatrix.tabulate(evidenceCount, stateCount) {
      (i, j) => {
        val row = accum(i, ::)
        val c = sum(row)
        (c == 0.0) match {
          case false => accum.apply(i, j) / c
          case true => accum.apply(i, j)
        }
      }
    }

    val betaResult = betaInner(T)(0)(pi)(A)(B)(V)(accum1)
    localMap.empty
    normalize(betaResult(*, ::), 1.0)
  }


  /**
   * use the viterbi algortihm to determine the most likeli state sequence
  for the evidence sequence
  based on pseudo code from: http://en.wikipedia.org/wiki/Viterbi_algorithm
   * @param model
   * @param evidenceSequence
   * @return
   */
  def viterbiPredict(model: Model)(evidenceSequence: List[String]): List[Prediction] = {
    // locally scoped cache
    val localMap = Map[Int, DenseMatrix[Double]]()

    /**
     * local cache
     * @param i
     * @param methodFn
     * @return
     */
    def cache(i: Int)(methodFn: () => DenseMatrix[Double]) = {
      localMap.contains(i) match {
        case true => {
          localMap.get(i)
        }
        case _ => {
          val (mat: DenseMatrix[Double]) = methodFn()
          localMap += (i -> mat)
          mat
        }
      }
    }

    val pi = model.pi
    val A = model.A
    val B = model.B

    val V = SequenceEstimation().indices(model.evidence)(evidenceSequence)
    val K = model.states.length
    val T = V.length
    val T1 = DenseMatrix.tabulate(K, T) { (i, j) => 0.0}
    val T2 = DenseMatrix.tabulate(K, T) { (i, j) => 0.0}
    // initialisation
    val T1p = (0 to (K - 1)).foldLeft(T1) {
      (t1, i) => {
        val j = V.apply(0)
        val p = pi.apply(i) * B.apply(j, i)
        t1(i, 0) = p
        t1
      }
    }
    // calculate scores for time T
    val (t1, t2) = (1 to (T - 1)).foldLeft(T1p, T2) {
      (pair, t) => {
        (0 to (K - 1)).foldLeft(pair) {
          (pair, j) => {
            val (t1, t2) = pair
            val v = V.apply(t)
            val args = DenseVector.tabulate(K) {
              k => {
                t1.apply(k, t - 1) * A.apply(k, j) * B.apply(v, j)
              }
            }
            val max = max(args)
            val amax = argmax(args)
            t1(j, t) = max
            t2(j, t) = amax.toDouble
            (t1, t2)
          }
        }
      }
    }
    // back track from T.. T-1
    val lastT = t1(::, (T - 1))
    val amax = argmax(lastT)
    val max = max(lastT)
    val Z = DenseVector.tabulate(T) { i => 0.0}
    val S = DenseVector.tabulate(T) { i => 0.0}
    Z(T - 1) = amax.toDouble
    S(T - 1) = max
    val sT = model.states.apply(amax)
    val v = V.apply(T - 1)
    val accum = List(Prediction(max, sT, model.evidence.apply(v), T, true))
    // accumulate
    (1 to (T - 1)).reverse.foldLeft(accum) {
      (accum, i) => {
        val index = Z.apply(i).toInt
        Z(i - 1) = t2(index, i)
        S(i - 1) = S(i)
        val stIndex = Z(i - 1).toInt
        val sT = model.states(stIndex)
        val v = V(i - 1)
        val predict = Prediction(t1(index, i), sT, model.evidence(v), i, true)
        predict :: accum
      }
    }
  }


  /**
   * the training method makes use of the forward backward
    algorithm.

      input: the input model to start training
    trainSequences: the training sequences to present for learning, the last item in each sequence is considered to be the target state
  Note that training sequences need only be the set of complete sequences for each set of transitions.

  theta: the threshold to use until convergence
    maxEpochs: the maximum epochs to run if convergence is not met

   * @param input
   * @param trainSequences
   * @param theta
   * @param maxEpochs
   * @return
   */
  def train(input: InputModel)(trainSequences: List[List[String]])(theta: Double)(maxEpochs: Int) = {
    val A = input.A
    val Bk = input.Bk
    val pi = input.pi
    val stateCount = input.states.length
    val evidenceCount = input.evidence.length

    val matA = DenseMatrix.tabulate(A.rows, A.cols) {
      (i, j) => A(i, j)
    }
    val firstB = Bk.head
    // the new B matrix will be multiplied against each sequence in Bk
    // so it is initialised to 1.
    val matB = DenseMatrix.tabulate(firstB.rows, firstB.cols) { (i, j) => 1.0}
    val totalSequences = trainSequences.length

    /**
     * inner training function
     * @param epoch
     * @param error
     * @param data
     * @return
     */
    def innerTrain(epoch: Int)(error: Double)(data: (List[Double], DenseMatrix[Double], DenseMatrix[Double])): (Int, Double, List[Double], DenseMatrix[Double], DenseMatrix[Double]) = {
      val (matPi, matA, matB) = data
      (epoch >= maxEpochs) match {
        case true => (epoch, error, matPi, matA, matB)
        case _ => {
          val (newPi, newA, newB) =
            trainSequences.foldLeft(matPi, matA, matB) {
              (oldData, trainSeq) => {
                val example = trainSeq.take(trainSeq.length - 1)
                val V = SequenceEstimation().indices(input.evidence)(example)

                // for each step in V we compute \hat{a_ij} and \hat{b_ij} first compute alpha and beta
                val T = example.length - 1
                // the denominator is calculated for all B sequences

                // the numerator is calculated only for the current e_t+1 in V for A
                // and only for the current e_t in V for B

                // calculate the denominator

                // firstly the normalisation factor P(O^k|model) this is the joint of the probability of the sequence in row B.[t, ]
                val range = (T <= 1) match {
                  case true => List()
                  case _ => (0 to (T - 1))
                }
                // for each matrix in B
                val (newPi, newA, newB) =
                  Bk.foldLeft(oldData) {
                    (oldData, B) => {
                      val (oldPi, oldA, oldB) = oldData
                      // since we are supplying a sequence B of matrice we will attempt to regularise
                      // the B matrix by multiplying it with oldB
                      val Bjoint = DenseMatrix.tabulate(B.rows, B.cols) {
                        (i, j) => {
                          B(i, j) * oldB(i, j)
                        }
                      }
                      normalize(Bjoint(*, ::))

                      // calculate the normalising factor from the joint distribution of V in B accross all states
                      // for each index in V calculate the joint probability in all states
                      // multiply it with the joint probability of the other indices in V
                      val P = V.foldLeft(0.0) {
                        (n, t) => {
                          val p =
                            (0 to Bjoint.cols).map {
                              (i) => {
                                Bjoint.apply(t, i)
                              }
                            }.reduce { (a, b) => a * b}
                          n + p
                        }
                      }
                      val p = P match {
                        case 0.0 => 1.0
                        case _ => 1.0 / P
                      }
                      // calculate alpha and beta
                      val alpha1 = alpha(stateCount - 1)(pi)(oldA)(Bjoint)(stateCount)(evidenceCount)(V)
                      val beta1 = beta(stateCount - 1)(pi)(oldA)(Bjoint)(stateCount)(evidenceCount)(V)

                      // fold over time t 2 times
                      // first to calculate the denominator
                      // this is the sum over all evidence vars e from 0 to T-1
                      // in the matrix A (the rows of A 0..T-1
                      val (denomA1, denomB1) =
                        (0 to (T - 1)).foldLeft((0.0, 0.0)) {
                          (pair, t_index) => {
                            val e_t = V(t_index)
                            val e_tplus1 = V(t_index + 1)
                            (0 to oldA.rows).foldLeft(pair) {
                              (pair1, i) => {
                                val (a, b) = pair1
                                val alpha_i = alpha1(e_t, i)
                                val beta_i = beta1(e_tplus1, i)
                                val a1 = a + (alpha_i * beta_i)
                                val b1 = b + (alpha_i * beta_i)
                                (a1, b1)
                              }
                            }
                          }
                        }
                      val (denomA, denomB) = (denomA1 * p, denomB1 * p)

                      /**
                       * restimating \bar{\pi}

                       expected frequency in state i at time 1
                       $$
                       \bar{pi_i} = \gamma_1 (i)
                       $$
                       **/
                      def piNew(lpi: List[Double])(lA: DenseMatrix[Double])(lB: DenseMatrix[Double]): List[Double] = {
                        (0 to (alpha1.cols - 1)).map {
                          (i) => {
                            val denom =
                              (0 to (alpha1.cols - 1)).map {
                                j => {
                                  alpha1(0, j) * beta1(0, j)
                                }
                              }.sum
                            val num = alpha1(0, i) * beta1(0, i)
                            (num / denom)
                          }
                        }
                      }.toList
                      //
                      val newPi = piNew(oldPi)(oldA)(oldB)
                      //
                      // now fold over time t to calculate the numerators
                      // this is the probability in the sequence V (the range) from 0 to T-1
                      val newA = (0 to (oldA.rows - 1)).foldLeft(oldA) {
                        (newA, i) => {
                          (0 to (oldA.cols - 1)).foldLeft(newA) {
                            (newA1, j) => {
                              // fold accross t in range (0..T-1)
                              val a = (T <= 1) match {
                                case true => 0.0
                                case _ => {
                                  range.foldLeft(0.0) {
                                    (n, t_index) => {
                                      val e_t = V(t_index)
                                      val e_tplus1 = V(t_index + 1)
                                      val alpha_i = alpha1(e_t, i)
                                      val beta_j = beta1(e_tplus1, j)
                                      val a1 = n + (alpha_i * matA(i, j) * Bjoint(e_tplus1, j) * beta_j)
                                      a1
                                    }
                                  }
                                }
                              }
                              newA1(i, j) = (denomA == 0.0) match {
                                case true => newA1(i, j)
                                case _ => newA1(i, j) + (a / denomA)
                              }
                              newA1
                            }
                          }
                        }
                      }

                      // fold over time T-1 calculate the normalising factor
                      // of e_t in time A
                      // now from i to j we update a_ij
                      val newB = (0 to (B.rows - 1)).foldLeft(Bjoint) {
                        (newB, i) => {
                          (0 to (B.cols - 1)).foldLeft(newB) {
                            (newB1, j) => {
                              val a = (T <= 1) match {
                                case true => 0.0
                                case _ => {
                                  range.foldLeft(0.0) {
                                    (n, t_index) => {
                                      val e_t = V(t_index)
                                      val e_tplus1 = V(t_index + 1)
                                      val alpha_j = alpha1(e_t, j)
                                      val beta_i = beta1(e_tplus1, i)
                                      val a1 = n + (alpha_j * beta_i)
                                      a1
                                    }
                                  }
                                }
                              }
                              newB1(i, j) = (denomB == 0.0) match {
                                case true => newB1(i, j)
                                case _ => {
                                  newB1(i, j) + (a / denomB)
                                }
                              }
                              newB1
                            }
                          }
                        }
                      }
                      (newPi, newA, newB)
                    }
                  }
                  (newPi, newA, newB)
              }
            }


          normalize(newA(*,::))
          normalize(newB(*, ::))

          val deltaA = newA - matA
          val deltaB = newB - matB
          val tempA = abs(deltaA)
          val absA = sum(sum(tempA(*, ::)))
          val tempB = abs(deltaB)
          val absB = sum(sum(tempB(*, ::)))
          val max = (absA > absB) match {
            case true => absA
            case _ => absB
          }
          (max <= theta) match {
            case true => (epoch, max, newPi, newA, newB)
            case _ => innerTrain (epoch + 1)(max)(newPi, newA, newB)
          }
        }
      }
    }
    // train sequences
    val (epoch, error, newPi, newA, newB) = innerTrain(1)(0.0)(pi, matA, matB)
    Model(newPi, newA, newB, states, evidence, epoch, error)
  }

}


