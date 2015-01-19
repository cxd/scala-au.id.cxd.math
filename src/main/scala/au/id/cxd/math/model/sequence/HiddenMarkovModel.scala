package au.id.cxd.math.model.sequence

import breeze.linalg._
import breeze.math._
import breeze.numerics._
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
          V.foldLeft(accum) { k => inner(k)(t + 1)(accum)}
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





}


