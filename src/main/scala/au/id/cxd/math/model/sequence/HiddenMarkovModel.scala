package au.id.cxd.math.model.sequence

import au.id.cxd.math.data.SequenceEstimation
import au.id.cxd.math.model.entity.hmm.{InputModel, Model, Prediction}
import breeze.linalg._
import breeze.math._
import breeze.numerics._
import breeze.linalg.operators._
import scala.collection.mutable._

/**
  * ##import MathJax
  *
  * The HMM is calculated out of two processes, a forward and backward process.
  *
  * The model itself consists of two matrices and a prior matrix.
  *
  * A state transition matrix $A$ of $M \times M$ representing the likelihood of transitioning
  * to state $X_{t+1}$ at time $t+1$ from the previous state $X_t$ at time $t$.
  * The values $a_{ij}$ represent the transition from $X_t$ to $X_{t+1}$ hence $i = t$ and $j = t + 1$.
  * The table forms the likelihood $P(X_{t+1}|X_t)$ such that
  *
  * $$
  * a_{ij} = P(x_j | x_i)
  * $$
  *
  * The second matrix $B$ represents a state evidence transition matrix. Given $M$ states there are $N$
  * evidence variables, hence $B$ is an $M \times N$ matrix where
  * $$
  * b_{ij} = P(x_j | e_i)
  * $$
  *
  * Additionally there is a prior probability for each state $X$ provided by a vector $\pi$ of length $M$ where
  *
  * $$
  * \pi_i = P(x_i)
  * $$
  *
  * The Hidden Markov model comprises of
  *
  * $$
  * HMM = \lambda = (A, B, \pi)
  * $$
  *
  * The procedure of learning the hidden markov model is to seek to learn the posterior matrices for $B$
  * given an initial condition. This implementation addresses the discrete model, rather than the continuous model
  * and seeks to learn the matrix $A$ relating state $X$ to the other states $x$ and the matrix $B$ relating
  * the state $X$ to the evidence variable $e$ give the stream of previous events in time $1 : t$ it seeks to
  * use the posterior model to predice the next state at time $t + k + 1$ and the prior states from the
  * sequential model in time $t + k$.
  *
  * $$
  * P(X_{t+k+1}|e_{1:t}) = \sum_{x_t+k} P(X_{t+k+1}|x_{t+k})P(x_{t+k}|e_{1:t})
  * $$
  *
  * Note that both $A$ and $B$ are as the likelihood model, and $\pi$ is initialised as the prior.
  *
  * This is estimated from the training data. The raw format of the training data is a jagged array
  * which consists of a
  *
  * {{{
  *   List[List[String]]
  * }}}
  *
  * Where the last item in the list represents a discrete state label for the domain.
  * Each item preceding that represents a discrete token for an evidence label in the domain.
  *
  * So each row in the jagged array contains a variable sequence of evidence labels and terminates with a state label
  * which is emitted by the sequence.
  *
  * Both the domain of evidence labels and state labels are deterministic in this model.
  * For example, if we have a simple weather world and we observe two evidence variables:
  *
  * {{{
  * Umbrella, NoUmbrella
  * }}}
  *
  * And we have two state variables,
  *
  * {{{
  * Raining,Sunny
  * }}}
  *
  * We can describe a sequence that leads up to Raining as
  *
  * {{{
  * Umbrella,Raining
  * }}}
  *
  * Or
  *
  * {{{
  * Umbrella,Umbrella,Raining
  * }}}
  *
  * We may potentially have a sequence that terminates in Sunny as
  *
  * {{{
  * Umbrella,Umbrella,NoUmbrella,Sunny
  * }}}
  *
  * For example.
  *
  * The matrix $A$ is estimated by identifying the sequences of length $n$ and $n+1$ and then counting the frequency
  * of the transition between $x_n$ and $x_{n+1}$.
  *
  * $$
  * a_{ij} = \frac{c_{ij}}{\sum_k c_{ik}}
  * $$
  *
  * The evidence matrix $B$ is estimated by identifying the frequency of $b_{m}$ terminating in $x_{n+1}$
  * for rows in the traning data $M$ and terminal states in $N$ lines ending in state $x_n$.
  *
  * $$
  * b_{ij} = \frac{ c(e_i, x_j) }{ \sum_k c(e_i, x_k) }
  * $$
  *
  * The class [[au.id.cxd.math.data.SequenceEstimation]] is used in this library to estimate these original likelihoods
  * and the prior distribution for $X$.
  *
  * $$
  * \pi_i = \frac{c_i}{\sum_k c_{ik}}
  * $$
  *
  *
  *
  * <br/>
  *
  * The procedure for learning is performed using a "forward backward" algorithm, there are two stages.
  *
  * The backward variable represents the probability that the model is in state $x_i(t)$ at time $t$.
  *
  * A matrix $\beta$ is defined to store the transition model at time $t$ such that
  *
  * $$
  * \beta_i(t) = 0 \text{ if } x_i \ne x_0 \text{ and } t = T
  * $$
  * $$
  * \beta_i(t) = 1 \text{ if } x_i = x_0 \text{ and } t = T
  * $$
  * $$
  * \beta_i(t) = \sum_j^N a_{ij}b_j(e_{t+1})\beta_j(t + 1) \text{ otherwise }
  * $$
  *
  * this matrix represents the probability that the sequence of evidence variables seen up until time $T$
  * from $1:t \rightarrow T$ will generate the state $x_i$
  *
  * $$
  * \beta_i(t) = P(e_{t+1}, e_{t+2}, ..., e_T | x_i, \lambda)
  * $$
  *
  * The second part of the algorithm is to compute the forward variable $\alpha$ a matrix that is defined
  * to represent the probability that the model is in state $x_i$ given the sequence of evidence variables
  * observed so far.
  *
  * $$
  * \alpha_j(t) = P(e_1, e_2, ..., e_t, x_i | \lambda)
  * $$
  *
  * it is defined as follows
  *
  * $$
  * \alpha_j(t) = 0 \text{ if } t = 0 \text{ and } x_j \ne x_0
  * $$
  * $$
  * \alpha_j(t) = 1 \text{ if } t = 0 \text{ and } x_j = x_0
  * $$
  * $$
  * \alpha_j(t) = \left[\sum_i^N \alpha_i(t-1)a_{ij}\right]b_j(e_t) \text{ otherwise }
  * $$
  *
  * as both are recursive procedures a dynamic programming technique is required to compute the values.
  *
  * The model for $A$ is iteratively updated for each transition at time $t$ and $t + 1$ by estimating the
  * probability of transition from state $x_i$ to state $x_j$ at time $t$ and $t+1$ given the current model $\lambda$
  * and the set of evidence variables $V_{t+1} = e_1,e_2,e_3,...,e_{t+1}$ the new estimates are stored
  * in a matrix $\epsilon$.
  *
  * The equation for $\epsilon_{ij}$ is given as:
  *
  * $$
  * \epsilon_{ij} = P(x_i(t), x_j(t+1)|V_{t+1}, \lambda)
  * $$
  *
  * $$
  * \frac{\alpha_i(t)b_j(e_{t+1})\beta_j(t+1)}{P(V_{t+1}|\lambda)}
  * $$
  *
  * $$
  * \frac{\alpha_i(t)b_j(e_{t+1})\beta_j(t+1)}{\sum_{i=1} \sum_{j=1} \alpha_i(t)a_{ij}b_j(e_{t+1})\beta_j(t+1)}
  * $$
  *
  * The expected number of times that state $x_i$ transitions to state $x_j$ is given by summing over the values
  * of $\epsilon_{ij}$.
  *
  * $$
  * \gamma_i(t) = \sum_{j=1}^T \epsilon_t(i,j)
  * $$
  *
  * The total number of expected times a state $x_i$ is visited is given by summing over $\gamma_i$ for all time $T$.
  *
  * $$
  * \sum_{t=1}^T \gamma_i(t)
  * $$
  *
  * The matrix $A$ can be updated with the new estimates for the state transitions as follows:
  *
  * $$
  * \hat{a_{ij}} = \frac{\gamma_i(t)}{\sum_{i=1}^T \gamma_i(t)}
  * $$
  *
  * The matrix $B$ can be updated with new estimates for the evidence variables and states by calculating
  * the ratio between the frequency that a particular evidence variable $e_k$ is produced and any evidence variable is produced.
  *
  * $$
  * \hat{b_{ij}} = \frac{\sum_{t=1,e_k} \gamma_k(t) }{\sum_{t=1} \gamma_i(t)}
  * $$
  *
  * The learing process repeats until convergence where the changes in the previous values of
  * $a_{ij}$ and $b_{ij}$ decrease below a threshold $\theta$.
  *
  * When working with multiple sequences $B_k$ the sequences are assumed to be independent of each other
  * and a normalisation factor is introduced in the estimate of $\hat{A}$.
  *
  * $$
  * c = \sum_{k=1}^K \frac{1}{P_k}
  * $$
  *
  * which is the marginal distribution of observation sequence k from the set of observation sequences
  * $O = e_1,...,e_n$ for each of the sequences in the input sample b. Assuming independence
  *
  * $$
  * P(O|\lambda) = \prod_{k=1} P(O_k|\lambda)
  * $$
  *
  * The update rules for $\hat{A}$ and $\hat{B}$ are changed as follows.
  *
  * $$
  * \bar{a_{ij}} = \frac{c \sum_{t=1} \alpha_{k,i}(t)b_j(e_{k,t+1})\beta_{k,j}(t+1)}{c \sum_{t=1} \alpha_{k,i}(t)\beta_{k,i}(t+1)}
  * $$
  *
  * $$
  * \bar{b_{ij}} = \frac{c \sum_{t=1,e_j} \alpha_{k,i}(t)\beta_{k,ij}(t) }{c \sum_{k=1} \alpha_{k,i}(t)\beta_{k,i}(t) }
  * $$
  *
  * the summations above are from $t=1$ to $T-1$ in both numerator and denominator.
  *
  * __Example Usage__
  *
  * The library is used in combination with the [[au.id.cxd.math.data.SequenceReader ]] and
  * [[au.id.cxd.math.data.SequenceEstimation]] classes the purpose of these classes are to
  * read data from CSV and then generate the initial likelihood for matrices $A$, $B$ and
  * the prior vector $\pi$.
  *
  * The test class "TestTrainRainModel" in the test cases for the project provides an example
  * of the usage and the class TestRainExample provides an example of the data format. See also
  * the example in TestTrainCtiModel for the use of CSV training data.
  *
  * The classes are used to create a [[au.id.cxd.math.model.entity.hmm.InputModel]]
  * which contains the initialisation parameters and are fed to the hmm algorithm.
  *
  * Intiialising the model for example:
  *
  * {{{
  * val fileName = "data/example_train_data.csv"
  * val file = new File(fileName)
  * val reader = SequenceReader()
  * // the data set (jagged array)
  * val data = reader.readSequences(file)
  * // unique set of states
  * val states =  reader.readStates (data)
  * // the unique labels for evidence variables
  * val evidenceVars = reader.readEvidenceVars (data)
  *
  * }}}
  *
  * After which the sequence estimator is used to generate the initial parameters of the model.
  *
  * {{{
  * val estimator = SequenceEstimation()
  *
  * val pi = estimator.statePriors(data)(states)
  *
  * val A = estimator.stateTransitions(data)(states)
  *
  * val Bk = estimator.avgPriorEvidences(data)(evidenceVars)(states)
  *
  * val input = InputModel(pi, A, List(Bk), states, evidenceVars)
  *
  * }}}
  *
  * The input model is then used to train the HMM
  *
  * {{{
  * val model = HiddenMarkovModel.train(input)(data)(0.00001)(50)
  * }}}
  *
  * The parameters include the threshold for convergence and maximum iterations.
  *
  * Once trained the model can be used for prediction.
  *
  * The example below is from the "CTI" world where telephony events result in a call state.
  * Refer also to the TestTrainRainModel for the weather world example.
  *
  * {{{
  *   val test = List("Ringing(inbound)",
         "UserEvent(Start)",
         "UserEvent(Stop)",
         "OffHook",
         "Established",
         "Held",
         "Dialing(Consult)");

       val predict1 = HiddenMarkovModel.viterbiPredict (model) (test)

  * }}}
  *
  * The string of events that are sent to the predict model do not include the end state.
  *
  * The prediction model will return a result for each evidence transition in the input chain.
  *
  * For example, from the input above, each step in the sequence above is categorised
  * with a corresponding state label, and the probability of that state label.
  *
  * {{{
  *   List({
        prob: 6.60507635396296E-44,
        state: OnCall
        evidence: Ringing(inbound)
        T: 1
        success: true
}, {
        prob: 7.758309697332233E-70,
        state: Started
        evidence: UserEvent(Start)
        T: 2
        success: true
}, {
        prob: 3.2302220856424765E-94,
        state: Paused
        evidence: UserEvent(Stop)
        T: 3
        success: true
}, {
        prob: 1.6782307523456403E-112,
        state: OnCall
        evidence: OffHook
        T: 4
        success: true
}, {
        prob: 1.1980508062616898E-131,
        state: OnCall
        evidence: Established
        T: 5
        success: true
}, {
        prob: 1.0950613538482917E-145,
        state: OnHold
        evidence: Held
        T: 6
        success: true
}, {
        prob: 1.0950613538482917E-145,
        state: Consult
        evidence: Dialing(Consult)
        T: 7
        success: true
})
  * }}}
  *
  * For further reading refer to:
  *
  * Norvig, Peter. Russell, Stuart. (2003) $Artificial\ Intelligence\ A\ Modern\ Approach.\ Second\ Edition.$ USA: Pearson Education, Inc. pp537-583, 724-733.
  *
  * Bishop, Christopher M. (2006) $Pattern\ Recognition\ and\ Machine\ Learning$ USA: Springer Science + Business Media, LLC. pp607-635.
  *
  * Rabiner, Lawrence R. (1989) ``A Tutorial on Hidden Markov Models and Seleted Applications in Speech Recognition''. USA: Proceedings of the IEEE, VOL 77, NO.2 pp257-286
  *
  * Created by cd on 10/01/15.
  */
object HiddenMarkovModel {

  /**
    * forward equation
    * *
    * $$
    * T = time_T
    * $$
    * $$
    * pi = P(x_i)
    * $$
    * $$
    * a_ij = P(x_j | x_i)
    * $$
    * $$
    * b_ij = P(x_j | e_i)
    * $$
    * *
    * $T$ represents $i$ current state we need to predict $j$ next state
    * *
    * $V$ - indices of evidence variables observed up to time $T$
    *
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
      *
      * @param i
      * @param methodFn
      * @return
      */
    def cache(i: Int)(methodFn: () => DenseMatrix[Double]): DenseMatrix[Double] = {
      localMap.contains(i) match {
        case true => {
          localMap.get(i).get
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
      * time T
      * with priors pi
      * and state transition matrix A
      * with evidence matrix B
      * and index of transitions V
      * accumulate in matrix accum
      */
    def alphaInner(T: Int)(pi: List[Double])(A: DenseMatrix[Double])(B: DenseMatrix[Double])(V: List[Int])(accum: DenseMatrix[Double]): DenseMatrix[Double] = {

      /**
        * compute new probabilities for evidence var at vk, at time t and update the row t in matrix accum
        * $$
        * \alpha_j(t) = b_{jk}v(t)\sum_{i=1}^c \alpha_i(t-1)a_{ij}
        * $$
        * v_k = current evidence variable in V
        * t = index of state at time t
        *
        *
        * @param vk
        * @param t
        * @param accum
        * @return
        */
      def inner(vk: Int)(t: Int)(accum: DenseMatrix[Double]): DenseMatrix[Double] = {

        val (alpha2: DenseMatrix[Double]) = cache(t - 1) { () => alphaInner(t - 1)(pi)(A)(B)(V)(accum) }


        val totals = DenseMatrix.tabulate(accum.rows, accum.cols) {
          (i, j) => {
            alpha2(i, t - 1) * A(t, j)
          }
        }
        // sum columns
        val sums = sum(totals(::, *))

        val total = DenseVector.tabulate(totals.cols) {
          (j) => sums(0, j) * B(vk, j)
        }
        accum(vk, ::) := total.t
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
            val idx = V(t)
            pi(j) * B(idx, j)
          }
          case _ => pi(j)
        }
      }
    }
    val accum1 = DenseMatrix.tabulate(evidenceCount, stateCount) {
      (i, j) => {
        val row = accum(i, ::)
        val c = sum(row.t)
        (c == 0.0) match {
          case true => accum(i, j)
          case _ => accum(i, j) / c
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
    * $$
    * T = time_T
    * pi = P(x_i)
    * a_ij = P(x_j | x_i)
    * b_ij = P(x_j | e_i)
    * $$
    * T represents i current state we need to predict j next state
    * *
    * V - indices of evidence variables observed up to time T
    *
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
      *
      * @param i
      * @param methodFn
      * @return
      */
    def cache(i: Int)(methodFn: () => DenseMatrix[Double]): DenseMatrix[Double] = {
      localMap.contains(i) match {
        case true => {
          localMap.get(i).get
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
      * time T
      * with priors pi
      * and state transition matrix A
      * with evidence matrix B
      * and index of transitions V
      * accumulate in matrix accum
      */
    def alphaInner(T: Int)(pi: List[Double])(A: DenseMatrix[Double])(B: DenseMatrix[Double])(V: List[Int])(accum: DenseMatrix[Double]): DenseMatrix[Double] = {

      /**
        * compute new probabilities for evidence var at vk, at time t and update the row t in matrix accum
        * $$
        * \alpha_j(t) = b_{jk}v(t)\sum_{i=1}^c \alpha_i(t-1)a_{ij}
        * $$
        * $v_k$ = current evidence variable in V
        * $t$ = index of state at time t
        *
        *
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
            alpha2(i, t - 1) * A(t, j)
          }
        }
        // sum columns
        val sums = sum(totals(::, *))

        val total = DenseVector.tabulate(totals.cols) {
          (j) => sums(0, j) * B(vk, j)
        }
        accum(vk, ::) := total.t
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
            val idx = V(t)
            pi(j) * B(idx, j)
          }
          case _ => pi(j)
        }
      }
    }
    val accum1 = DenseMatrix.tabulate(evidenceCount, stateCount) {
      (i, j) => {
        val row = accum(i, ::)
        val c = sum(row.t)
        (c == 0.0) match {
          case true => accum(i, j)
          case _ => accum(i, j) / c
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
    * moving from t = 1 to T
    * $$
    * T = time T
    * pi = P(x_i)
    * a_ij = P(x_j | x_i)
    * b_ij = P(x_j | e_i)
    * $$
    * T represents i current state we need to predict j next state
    * *
    * V - indices of evidence variables observed up to time T
    *
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
      *
      * @param i
      * @param methodFn
      * @return
      */
    def cache(i: Int)(methodFn: () => DenseMatrix[Double]): DenseMatrix[Double] = {
      localMap.contains(i) match {
        case true => {
          localMap.get(i).get
        }
        case _ => {
          val (mat: DenseMatrix[Double]) = methodFn()
          localMap += (i -> mat)
          mat
        }
      }
    }

    val alpha0 = alphaUnscaled(T)(pi)(A)(B)(stateCount)(evidenceCount)(V)
    /**
      * $$
      *
      * \beta_i(t) = \sum_{j=1}^C \beta_j(t+1)a_{ij}b_{jk}v(t+1)
      * $$
      * @param T
      * @param t
      * @param pi
      * @param A
      * @param B
      * @param V
      * @param accum
      * @return
      */
    def betaInner(T: Int)(t: Int)(pi: List[Double])(A: DenseMatrix[Double])(B: DenseMatrix[Double])(V: List[Int])(accum: DenseMatrix[Double]): DenseMatrix[Double] = {
      // perform the calculation

      def inner(vk: Int)(t: Int)(accum: DenseMatrix[Double]): DenseMatrix[Double] = {
        val (beta2: DenseMatrix[Double]) = cache(t + 1) { () => betaInner(T)(t + 1)(pi)(A)(B)(V)(accum) }
        // calculate the scaling factor D_t = \prod_{\t=1}^T c_t
        val d = alpha0.toArray.reduce { (a, b) => a + b }
        // vk = i, t
        // \beta_i(t) = \sum_{j=1}^C \beta_j(t+1)a_{ij}b_{jk}v(t+1)
        val totals = DenseMatrix.tabulate(accum.rows, accum.cols) {
          (i, j) => {
            beta2(i, t + 1) * A(t, j) * B(vk, t + 1)
          }
        }
        // sum columns
        val (total: DenseVector[Double]) = (sum(totals(::, *))).toDenseVector
        (d == 0.0) match {
          case true => accum
          case false => {
            val (total1: DenseVector[Double]) = DenseVector.tabulate(total.size) {
              a => (total(a) / d)
            }
            accum(vk, ::) := total1.t
            accum
          }
        }
      }
      // general and recursive case
      (t >= (T - 1)) match {
        case true => accum
        case false =>
          V.foldLeft(accum) { (accum, k) => inner(k)(t + 1)(accum) }
      }
    }
    // initialising beta matrix with priors
    val accum = DenseMatrix.tabulate(evidenceCount, stateCount) {
      (t, j) => {
        val vlen = V.length
        (t < vlen) match {
          case true => {
            val idx = V(t)
            pi(j) * B(idx, j)
          }
          case false => {
            pi(j) / (evidenceCount.toDouble)
          }
        }
      }
    }

    val accum1 = DenseMatrix.tabulate(evidenceCount, stateCount) {
      (i, j) => {
        val row = accum(i, ::)
        val c = sum(row.t)
        (c == 0.0) match {
          case false => accum(i, j) / c
          case true => accum(i, j)
        }
      }
    }

    val betaResult = betaInner(T)(0)(pi)(A)(B)(V)(accum1)
    localMap.empty
    normalize(betaResult(*, ::), 1.0)
  }


  /**
    * use the viterbi algortihm to determine the most likeli state sequence
    * for the evidence sequence
    * based on pseudo code from: http://en.wikipedia.org/wiki/Viterbi_algorithm
    *
    * @param model
    * @param evidenceSequence
    * @return
    */
  def viterbiPredict(model: Model)(evidenceSequence: List[String]): List[Prediction] = {
    // locally scoped cache
    val localMap = Map[Int, DenseMatrix[Double]]()

    /**
      * local cache
      *
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
    val T1 = DenseMatrix.tabulate(K, T) { (i, j) => 0.0 }
    val T2 = DenseMatrix.tabulate(K, T) { (i, j) => 0.0 }
    // initialisation
    val T1p = (0 to (K - 1)).foldLeft(T1) {
      (t1, i) => {
        val j = V(0)
        val p = pi(i) * B(j, i)
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
            val v = V(t)
            val args = DenseVector.tabulate(K) {
              k => {
                t1(k, t - 1) * A(k, j) * B(v, j)
              }
            }
            val max1 = max(args)
            val amax = argmax(args)
            t1(j, t) = max1
            t2(j, t) = amax.toDouble
            (t1, t2)
          }
        }
      }
    }
    // back track from T.. T-1
    val lastT = t1(::, (T - 1))
    val amax = argmax(lastT)
    val max1 = max(lastT)
    val Z = DenseVector.tabulate(T) { i => 0.0 }
    val S = DenseVector.tabulate(T) { i => 0.0 }
    Z(T - 1) = amax.toDouble
    S(T - 1) = max1
    val sT = model.states(amax)
    val v = V(T - 1)
    val accum = List(Prediction(max1, sT, model.evidence(v), T, true))
    // accumulate
    (1 to (T - 1)).reverse.foldLeft(accum) {
      (accum, i) => {
        val index = Z(i).toInt
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
    * algorithm.
    * *
    * input: the input model to start training
    * trainSequences: the training sequences to present for learning, the last item in each sequence is considered to be the target state
    * Note that training sequences need only be the set of complete sequences for each set of transitions.
    * *
    * theta: the threshold to use until convergence
    * maxEpochs: the maximum epochs to run if convergence is not met
    *
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
    val matB = DenseMatrix.tabulate(firstB.rows, firstB.cols) { (i, j) => 1.0 }
    val totalSequences = trainSequences.length

    /**
      * inner training function
      *
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
                            (0 to (Bjoint.cols - 1)).map {
                              (i) => {
                                Bjoint(t, i)
                              }
                            }.reduce { (a, b) => a * b }
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
                            (0 to (oldA.rows - 1)).foldLeft(pair) {
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
                        * *
                        * expected frequency in state i at time 1
                        * $$
                        * \bar{pi_i} = \gamma_1 (i)
                        * $$
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
                                      val alpha_j = alpha1(e_t, j)
                                      val beta_i = beta1(e_t, j)
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


          normalize(newA(*, ::))
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
            case _ => innerTrain(epoch + 1)(max)(newPi, newA, newB)
          }
        }
      }
    }
    // train sequences
    val (epoch, error, newPi, newA, newB) = innerTrain(1)(0.0)(pi, matA, matB)
    Model(newPi, newA, newB, input.states, input.evidence, epoch, error)
  }

}


