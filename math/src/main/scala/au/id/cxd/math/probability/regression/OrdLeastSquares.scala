package au.id.cxd.math.probability.regression

import java.io._

import au.id.cxd.math.function.matrix.PseudoInverse
import au.id.cxd.math.probability.analysis.{CriticalValue, StatisticalTest, TestResult, UpperTail}
import au.id.cxd.math.probability.continuous.{FDistribution, Normal}
import breeze.linalg._
import breeze.numerics.pow

import scala.collection.immutable.Stream

/**
  * ##import MathJax
  *
  * Least squares is a simpler form of regression approximating $Y$ as
  *
  * $$
  * \hat{Y} = \beta_0 + \sum X_i \beta_j
  * $$
  *
  * $$
  * \hat{Y} = X\beta
  * $$
  *
  * The parameter $\beta$ is estimated using the sample instances and the sample outputs as shown
  *
  * $$
  * \hat{\beta} = (X'X)^{-1}X'Y
  * $$
  *
  * $\hat{\beta}$ is assumed to have a normal distribution with mean $\beta$
  *
  * and variance $Q \sigma^2$
  *
  * where $Q = (X'X)^{-1}$.
  *
  * The residual squared error can be calculated as:
  *
  * $$
  * RSS(\beta) = \sum (Y_i - \hat{Y_i})^2
  * $$
  *
  * The residuals $\epsilon$ are assumed distributed as $N(0,\sigma^2)$
  *
  * Inference on $\beta$ can be performed using the standardised coefficient z-score for $\beta$.
  *
  * $$
  * z_j = \frac{\hat{\beta_j}}{\hat{\sigma}\sqrt{v_j}}
  * $$
  *
  * The value for $v_j$ is derived from the $jth$ position on diagonal from the matrix $(X'X)^{-1}$.
  *
  * The Z-score from the normal distribution at a corresponding alpha level can be used to form
  * the associated confidence interval for $\beta$ at $p-value = z$ at the $1 - \alpha$ level .
  *
  *
  * $$
  * \hat{\beta_j} \pm z^{(1-\alpha)} \sqrt{v} \hat{\sigma}
  * $$
  *
  * We can test against the null hypothesis that $\beta_j = 0$ and therefore the corresponding $X_{ij}$ does
  * not contribute to the target variable (given the coefficient is 0).
  * A t-distribution of N - p - 1 degrees of freedom can be used forming the confidence interval
  * however as the sample size increases the difference between the t-distribution and normal distribution
  * becomes negligable (see Hastie), so the normal distribution is used defining the confidence interval
  *
  * $$
  * \left(\beta_j - z_{1-\alpha}v&#94;{1/2}\hat{\sigma}, \beta_j - z_{1-\alpha}v&#94;{1/2}\hat{\sigma}\right)
  * $$
  *
  * at the critical $\alpha$ level for example 0.05 for a 95% confidence interval.
  * Those values with a very small p-value would reject the null hypothesis or if (|Z| > z_{1-\alpha}).
  *
  *
  * $$
  * H_0: \beta_j = 0
  * $$
  * $$
  * H_1: \beta_j != 0
  * $$
  *
  *
  * As $\beta$ defines the coefficients of the $pth$ attribute in $X$ it is possible to test whether
  * the $kth$ coefficient can be set to $0$ (in which case the contribution of $X_k$ to estimating $Y$ is not significant)
  * by using an F-score.
  * Let $k_1$ equal the $k$ parameters and $k_0$ be the a smaller model where $k_1 - k_0$ parameters are set to $0$
  * the F-score can be calculated as
  *
  * $$
  * F = \frac{(RSS_0 - RSS_1)/(k_1 - k_0)}{RSS_1/(N - k_1 - 1)}
  * $$
  *
  * this statistic can be used to determine if the residual sum of squares error is changed significantly
  * by setting the $k_1 - k_0$ parameters to 0. If the RSS decreases, and the F-score can be tested
  * against a corresponding p-value for an associated $\alpha$ level to determine if the improvement
  * is significant change. If so, the corresponding attributes contribution in determining $Y$ is marginal.
  *
  * For further details refer to
  *
  * Hastie, T. Tibshirani, R. Friedman, J. The Elements of Statistical Learning, Second Ed. Springer 2009.
  *
  * In this case the degree of the polynomial is supplied to the constructure
  * and the input vector has features added for the degree of the polynomial during training and prediction.
  */
@SerialVersionUID(100L)
class OrdLeastSquares(var X: DenseMatrix[Double], var Y: DenseVector[Double], val m: Int = 1, val alpha: Double = 0.05)
  extends Serializable {

  /**
    * The beta parameter is approximated by
    * $$
    * \hat{\beta} = (X'X)^{-1}X'Y
    * $$
    *
    * Note we assume that $\beta$ is normally distributed as
    *
    *
    *
    * $$
    * \hat{\beta} = N( \beta, Q \sigma^2 )
    * $$
    *
    * where $Q = (X'X)^{-1}$
    *
    * $\hat{\beta}$ is the Maximum likelihood estimate of $\beta$.
    *
    *
    * Created by cd on 28/06/2014.
    */
  var Beta = DenseMatrix.ones[Double](m + 1, 1)

  /**
    * The variance of beta is given  by
    *
    * $$
    * Var(\beta) = Q \sigma^2
    * $$
    *
    * with $Q = (X'X)^{-1}$
    *
    * The variance parameter for $\sigma^2$ is approximated as
    *
    * $$
    * \frac{1}{N - p - 1} \sum (y_i - \hat{y_i})^2
    * $$
    *
    * This value is computed during the training cycle.
    */
  var betaVariance: DenseMatrix[Double] = DenseMatrix.zeros(X.cols, X.cols)

  /**
    * the beta z score
    */
  var betaZScore: DenseMatrix[Double] = DenseMatrix.zeros(m + 1, 1)

  /**
    * the critical value for beta to use when testing at the alpha level supplied
    * this is the reject region
    * { betaZ > criticalBetaZValue }
    * meaning to reject that beta equals 0
    */
  var criticalBetaZValue: Double = 0.0

  /**
    * the p-value that corresponds to the critical beta z value
    *
    *
    * in this case we reject the null hypothesis that $\Beta = 0$
    * when the pvalue for $f(\Beta_z) < f(Z_\alpha)$
    */
  var criticalPValue: Double = 0.0

  /**
    * the set of p-values for the corresponding betaZScore
    *
    */
  var betaPValue: DenseMatrix[Double] = DenseMatrix.zeros(m + 1, 1)


  /**
    * The variance parameter for $\sigma^2$ is approximated as
    *
    * $$
    * \frac{1}{N - p - 1} \sum (y_i - \hat{y_i})^2
    * $$
    *
    * This value is computed during the training cycle.
    */
  var variance: Double = 0.0

  /**
    * the pseudo inverse of  the covariance variance matrix
    */
  var pseudoInverse: DenseMatrix[Double] = DenseMatrix.zeros(X.cols, X.cols)

  /**
    * the residuals resulting from the training process.
    */
  var residuals: DenseVector[Double] = DenseVector.zeros(Y.length)


  /**
    * the mse of the regression model
    */
  var mse: Double = 0.0

  val powers = for {
    i <- 0 to m
  } yield i.toDouble

  var P = createPolynomial(X, m)

  var Y1 = DenseMatrix.tabulate(Y.length, 1) { case (i, j) => Y(i) }


  /**
    * the degrees of freedom based on the input data.
    * this is used during the update function
    */
  var df:Int = P.rows - P.cols

  /**
    * first determine how many features will be generated
    * for the number of columns.
    *
    * For example if we have cols 2 and degree m = 2
    * (a, b) will have (a, b, a&#94;2, 2ab, b&#94;2)  which is 3 extra features
    *
    * $$
    * (a, b, c)&#94;n = (a, b, b, a&#94;2, ab, b&#94;2, bc, c&#94;2, x&#94;3, x&#94;2b, x&#94;2c, ab&#94;2, abc, ac&#94;2, b&#94;3, c&#94;3, ..., )
    * $$
    *
    * We can use the multinomial theorem to do the expansion
    *
    * the coefficients up to n can be determined using the rule
    *
    * $$
    * \left( n \choose {k_1, k_2, k_3, ..., k_m} \right)
    * $$
    *
    * where for any combination of $k$ $\sum_{i=1}&#94;m k$ must equal $n$.
    *
    * $m$ represents the number of columns in matrix $X$ and has
    *
    */

  def createPolynomial(X1: DenseMatrix[Double], m: Int): DenseMatrix[Double] = {
    m match {
      case 1 => {
        // create matrix where first column is 1
        val M = DenseMatrix.tabulate[Double](X1.rows, X1.cols + 1) {
          case (i, j) => j match {
            case 0 => 1
            case n => X1(i, n - 1)
          }
        }
        M
      }
      case _ => {
        val M1 = PolynomialExpansion(X1, m)
        val M2 = DenseMatrix.tabulate[Double](M1.rows, M1.cols + 1) {
          case (i, j) => j match {
            case 0 => 1.0
            case n => M1(i, n - 1)
          }
        }
        M2
      }
    }
  }

  /**
    *
    *
    * http://brisbanepowerhouse.org/events/2016/06/30/daas-near-death-experience/
    * multiply the factor vector by the
    *
    * @param B  beta matrix.
    * @param X1 matrix of factors
    */
  def multWeights(B: DenseMatrix[Double], X1: DenseMatrix[Double]): DenseMatrix[Double] = {
    println(s"B dim = ${B.rows} x ${B.cols}")
    println(s"X dim = ${X1.rows} x ${X1.cols}")

    B * X1.t
  }

  /**
    * calculate the squared error between the target column vector and the function column vector
    * vectors must have same dimension
    *
    * @param T
    * @param Y
    */
  def squaredError(T: DenseMatrix[Double], Y: DenseMatrix[Double]): Double = {
    val difference = T.rows == Y.rows && T.cols == Y.cols match {
      case true => Y - T
      case _ => {
        val T2 = T.t
        T2.rows == Y.rows && T2.cols == Y.cols match {
          case true => Y - T2
          case _ => throw new Exception(s"Cannot calculate squaredError dimensions do not agree T dim = (${T.rows}, ${T.cols}) Y dim = (${Y.rows}, ${Y.cols}")
        }
      }
    }
    val delta = pow(difference, 2.0)
    val total = sum(delta)
    total.asInstanceOf[Double] * 0.5
  }

  /**
    * update the beta parameter using the inverse of the covariance matrix and the residuals
    *
    * The beta parameter is approximated by
    * $$
    * \hat{\beta} = (X'X)^{-1}X'Y
    * $$
    **/
  def updateWeights(F: DenseMatrix[Double], Y: DenseMatrix[Double]): DenseMatrix[Double] = {
    val Cov = F.t * F
    pseudoInverse = PseudoInverse(Cov)
    val B = (pseudoInverse * F.t)
    println(s"B dim ${B.rows} x ${B.cols}")
    println(s"Y dim ${Y.rows} x ${Y.cols}")

    Y.t * B.t
  }

  /**
    * train the least squares model.
    */
  def train(): (DenseMatrix[Double], Double) = {
    Beta = updateWeights(P, Y1)
    val yHat = multWeights(Beta, P)

    println(s"Y1 dim (${Y1.rows} x ${Y1.cols}) yHat dim (${yHat.rows} x ${yHat.cols})")
    residuals = (Y1 - yHat.t).toDenseVector
    mse = squaredError(yHat, Y1)

    // form the estimate of the beta variance
    variance = 1.0 / (Y.size - X.cols - 1) * mse
    betaVariance = variance * pseudoInverse
    val sigma = Math.sqrt(variance)
    // take the diagonal of the pseudoInverse (X'X)^-1
    // note that the pseudoinverse actually may be negative, the square root may infact be imaginary.
    val vj = DenseVector.tabulate(pseudoInverse.cols) {
      j => pseudoInverse(j, j)
    }
    /*
    * $$
      * z_j = \frac{\hat{\beta_j}}{\hat{\sigma}\sqrt{v_j}}
    * $$
    *
    * Since we are predicting a 1 column Y vector the beta matrix is always a single column matrix
    *
    * The row in beta corresponds to the coefficient of the ith column in X.
    *
    */
    betaZScore = DenseMatrix.tabulate[Double](Beta.rows, Beta.cols) {
      case (i, j) => Beta(i, j) / sigma * Math.sqrt(vj(i))
    }

    /**
      * the corresponding p-value for beta_j can be calculated
      * from the T distribution with N - p - 1 degrees of freedom.
      * The null hypothesis is that beta_j is equal to 0, and therefore the corresponding X value does not contribute
      * to the prediction.
      *
      * Testing against the hypothesis we reject the hypothesis if { |Z| > 0 }
      */
    val stdNorm = Normal(0.0)(1.0)
    betaPValue = DenseMatrix.tabulate[Double](betaZScore.rows, betaZScore.cols) {
      case (i, j) => stdNorm.pdf(betaZScore(i, j))
    }

    // the critical value is taken from the normal distribution with mean 0 and sd 1
    // we multiply sd * 5 to create the upper tail range of the normal distribution
    // between 0 and 5
    val criticalValue = CriticalValue.upperCriticalValue(0.0, 5.0, stdNorm)
    // the critical value is a two tailed test.
    criticalBetaZValue = criticalValue.value(alpha/2.0)
    criticalPValue = stdNorm.pdf(criticalBetaZValue)

    (yHat, mse)
  }

  /**
    * having trained the model predict a value for the new x input.
    *
    * @param x
    */
  def predict(x: Double) = {
    // convert to a polynomial
    // original column size: X1.cols
    val cols = P.cols
    val X1 = DenseMatrix.tabulate[Double](1, cols) {
      case (i, j) => Math.pow(x, i)
    }
    // TODO: check use of single value
    multWeights(Beta, X1)
  }

  /**
    * predicts a single column sequence for X
    * the length of X represents the rows of each sample
    *
    * @param x
    * @return
    */
  def predictSeq(x: DenseVector[Double]) = {
    val M = DenseMatrix.tabulate[Double](x.length, 1) {
      case (i, j) => x(i)
    }
    val M1 = createPolynomial(M, m)
    multWeights(Beta, M1)
  }

  /**
    * predict the result of multiplying the beta estimater against a new vector X
    *
    * $$
    * \hat{Y} = \beta_0 + \sum X_i \beta_j
    * $$
    *
    * $$
    * \hat{Y} = X\beta
    * $$
    *
    * @param X1
    * @return
    */
  def predict(X1: DenseMatrix[Double]) = {
    val M = createPolynomial(X1, m)
    multWeights(Beta, M)
  }


}

object OrdLeastSquares {

  /**
    * generate a new ols model for supplied X and Y values at the corresponding degree m
    *
    * @param X
    * @param Y
    * @param m
    * @return
    */
  def apply(X: DenseMatrix[Double], Y: DenseVector[Double], m: Int, alpha:Double = 0.05) = {
    val ols = new OrdLeastSquares(X, Y, m, 0.05)
    ols
  }

  /**
    * serialize the model to binary file for later use.
    * When serialized in this way the model contains all state values
    * and can be used to calculate residuals and other properties.
    * as opposed to just serializing the beta parameter matrix.
    *
    * @param ols
    * @param file
    */
  def serialize(ols: OrdLeastSquares)(file: File) = {
    val oos = new ObjectOutputStream(new FileOutputStream(file))
    oos.writeObject(ols)
    oos.flush()
    oos.close()
  }

  /**
    * read a binary serialized model from file for later use.
    *
    * @param file
    * @return
    */
  def deserialize(file: File): OrdLeastSquares = {
    val ois = new ObjectInputStream(new FileInputStream(file))
    val obj = ois.readObject().asInstanceOf[OrdLeastSquares]
    ois.close()
    obj
  }

  /**
    * save the beta parameter for later use.
    *
    * @param ols
    * @param file
    */
  def saveWeights(ols: OrdLeastSquares)(file: File) = {
    csvwrite(file, ols.Beta)
  }

  /**
    * load the model from a matrix on disc and supply the degree to use for estimation.
    * The model when loaded from CSV can only be used for prediction
    *
    * @param weightMatrixFile
    * @param degree
    */
  def loadWeights(weightMatrixFile: File, degree: Int) = {
    val weights = csvread(weightMatrixFile)
    val ols = new OrdLeastSquares(DenseMatrix.zeros[Double](1, 1), DenseVector.zeros[Double](1), degree)
    ols.Beta = weights
    ols
  }

}
