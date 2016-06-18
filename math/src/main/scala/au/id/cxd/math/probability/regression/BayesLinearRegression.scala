package au.id.cxd.math.probability.regression

import au.id.cxd.math.function.PseudoInverse
import au.id.cxd.math.probability.analysis.CriticalValue
import au.id.cxd.math.probability.continuous.Normal
import breeze.linalg.{DenseMatrix, DenseVector}

/**
  * ##import MathJax
  *
  * <h3>The Bayesian linear regression for assumed uniform variance.</h3>
  *
  * <p>
  * Bayesian Linear Regression extends the Ordinary Least Squares Regression by accounting for the uncertainty about the parameters within the model to generate a posterior distribution for the parameters and a posterior predictive distribution for the target variable. The bayesian regression model makes specific assumptions about the location and variance parameters and about the distributions of each and provides update rules for adjusting the model in light of new examples.
  * </p>
  *
  * The method extends the standard linear regression where
  *
  * $$
  * p(Y|\beta,\sigma&#94;2) \sim N(\mu = X\beta, \Sigma&#94;{-1} = \sigma&#94;2 I)
  * $$
  *
  * Box and Tiao show the distribution to be
  *
  * $$
  * p(Y|\beta,\sigma&#94;2) = \left( \frac{1}{\sqrt{2\pi}} \right)&#94;n \sigma&#94;{-n} \exp{ \left[ -\frac{1}{2\sigma&#94;2} \left[ vs&#94;2 + (\beta - \hat{\beta})'X'X(\beta - \hat{\beta}) \right]  \right] }
  * $$
  *
  * with
  *
  * $$
  * v = n - k
  * $$
  * $$
  * s&#94;2 = (1/v)(Y - \hat{Y})'(Y-\hat{Y})
  * $$
  *
  * $$
  * \hat{\beta} = (X'X)&#94;{-1}X'Y
  * $$
  *
  * $$
  * \hat{Y} = X\hat{\beta}
  * $$
  *
  * The uniform variance $\sigma&#94;2$ estimates the error such that
  *
  * $$
  * p(\epsilon|\sigma&#94;2) \sim N(0, \sigma&#94;2 I)
  * $$
  *
  * The conditional distribution for $Y$ is expressed as
  *
  * $$
  * p(Y|\beta, \sigma&#94;2, X) \sim N(X\beta, \sigma&#94;2I)
  * $$
  *
  * The joint uninformative prior distribution for the parameters $\beta$ and $\sigma&#94;2$ with uniform variance is given as
  *
  * $$
  * p(\beta, \sigma&#94;2 | X) \propto  \sigma&#94;{-2}
  * $$
  *
  * <h3>Posterior distribution for parameters</h3>
  *
  * The posterior distribution of the parameters is defined as
  *
  * $$
  * p(\beta,\sigma&#94;2|Y) \propto f(Y|\beta,\sigma&#94;2) \times p(\beta,\sigma&#94;2)
  * $$
  *
  * under uniform variance this is
  *
  * $$
  * \propto \sigma&#94;{-(v+1)} \exp{ \left[ -\frac{1}{2\sigma&#94;2} \left[vs&#94;2 + (\beta - \hat{\beta})'(X'X)(\beta - \hat{\beta}) \right]  \right] }
  * $$
  *
  * Gelman demonstrates that the posterior factorises into
  *
  * $$
  * \propto   \exp{ \left[ (\beta - \hat{\beta})'(X'X)(\beta - \hat{\beta}) \right] } \times \sigma&#94;{-(v+1)} \exp{ \left[ \frac{vs&#94;2}{2\sigma&#94;2} \right]}
  * $$
  *
  * $$
  * N \left(\hat{\beta}, \sigma&#94;2(X'X)&#94;{-1} \right) \times Inverse-\chi&#94;2 (v,s&#94;2)
  * $$
  *
  * Note that $X$ will have the constraint that the number of rows in $X$, $n$ will be greater than the number of columns $k$ and that the columns of $X$ should be linearly independent.
  *
  * <h3>Joint Posterior predictive distribution</h3>
  *
  * The joint posterior predictive distribution for new examples $\tilde{Y}$ and $\tilde{X}$ is dependent on the previous conditional distribution of the parameters and the
  * previous data $Y$ and $X$.
  *
  * $$
  * p(\tilde{Y}|\beta,\sigma&#94;2,\tilde{X},Y,X) = f(\tilde{Y}|\beta,\sigma&#94;2,\tilde{X}) \times p(\beta,\sigma&#94;2|Y,X)
  * $$
  *
  * Consider that the conditional distribution of
  *
  * $$
  * p(\tilde{Y}|\beta,\sigma&#94;2,\tilde{X},Y,X) \sim N(\tilde{X}\beta,\sigma&#94;2 I)
  * $$
  *
  * Then the likelihood $\times$ the posterior distribution for the parameters is
  *
  * $$
  * \propto \sigma&#94;{-n} \exp{ \left[ - \frac{1}{2\sigma&#94;2}(\tilde{Y} - \tilde{X}\beta)'(\tilde{Y} - \tilde{X}\beta)  \right] } \times \sigma&#94;{-(n+1)}\exp{\left[ -\frac{1}{2\sigma&#94;2}(Y - X\beta)'(Y - X\beta) \right]  }
  * $$
  *
  * $$
  * \propto \sigma&#94;{-(n + \tilde{n} + 1)} \exp{\left[ -\frac{1}{2\sigma&#94;2} \left( (\tilde{Y} - \tilde{X}\beta)'(\tilde{Y} - \tilde{X}\beta) + (Y-X\beta)'(Y-X\beta)  \right)  \right]}
  * $$
  *
  * In order to obtain the marginal distribution of $\tilde{Y}$, Gelman uses two steps firstly by marginalising $\beta$ and then by marginalising $\sigma$.
  *
  * Using the conditional expectation $\beta$ the parameters for $\mu$ and $\sigma$ of the distribution for $\tilde{Y}$ are derived as
  *
  * $$
  * E\left[\tilde{Y}|\sigma,Y\right] = E\left[ E\left[ \tilde{Y}|\beta,\sigma,Y\right]\right]
  * $$
  *
  * $$
  * E\left[\tilde{Y}|\sigma,Y\right] = E\left[ \tilde{X} \beta | \sigma, Y \right]
  * $$
  *
  * $$
  * \mu = \tilde{X} \hat{\beta}
  * $$
  *
  * And for the variance
  *
  * $$
  * Var\left[\tilde{Y}|\sigma,Y\right] = E\left[ Var\left[\tilde{Y}|\beta,\sigma,Y\right] \right] + Var\left[ E\left[\tilde{Y}|\beta,\sigma,Y\right]  \right]
  * $$
  * $$
  * Var\left[\tilde{Y}|\sigma,Y\right] = E[\sigma&#94;2I|\sigma,Y] + Var[\tilde{X}\beta|\sigma,Y]
  * $$
  * $$
  * Var\left[\tilde{Y}|\sigma,Y\right] = (I + \tilde{X}V_\beta\tilde{X}')\sigma&#94;2
  * $$
  * where $V_\beta = (X'X)&#94;{-1}$
  *
  * The posterior density conditioned on $\sigma$ is
  *
  * $$
  * p(\tilde{Y}|\sigma,Y\tilde{X},X) \sim N\left( \tilde{X}\hat{\beta},  (I + \tilde{X}V_\beta\tilde{X}')\sigma&#94;2 \right)
  * $$
  *
  * The next step is to marginalise over $\sigma$ to obtain the predictive density.
  *
  * $$
  * p(\tilde{Y}|\tilde{X},Y,X) \propto \int p(\tilde{Y}|\sigma,\tilde{X},Y,X) \times p(\sigma|Y,X) d\sigma
  * $$
  * *
  * $$
  * p(\tilde{Y}|\tilde{X},Y,X) \propto \int \sigma&#94;{-(n - k + 1)} \exp{\left[ \frac{\nu s&#94;2}{2\sigma&#94;2} \right]} \times
  * $$
  * *
  * $$
  * (\sigma&#94;2)&#94;{-n/2}\left|I + \tilde{X}V\tilde{X}'\right|&#94;{-n/2}\exp{\left[-\frac{1}{2\sigma&#94;2}(\tilde{X} - \tilde{X}\hat{\beta})'\left[I + \tilde{X}V\tilde{X}' \right]&#94;{-1}(\tilde{X} - \tilde{X}\hat{\beta}) \right]} d\sigma
  * $$
  * *
  * $$
  * \propto \int \sigma&#94;{-(n - k + 1)/2}\left|I + \tilde{X}V\tilde{X}'\right|&#94;{-n/2} \exp{\left[ -\frac{1}{2\sigma&#94;2}\left( \nu s&#94;2  + (\tilde{X} - \tilde{X}\hat{\beta})'\left[I + \tilde{X}V\tilde{X}' \right]&#94;{-1}(\tilde{X} - \tilde{X}\hat{\beta}) \right) \right]} d\sigma
  * $$
  * $$
  * \propto \left|I + \tilde{X}V\tilde{X}'\right|&#94;{-n/2} \int \sigma&#94;{-(\nu + 1)/2} \exp{\left[-\frac{1}{2\sigma&#94;2}A \right]} d\sigma&#94;2
  * $$
  * Where $A = \left( \nu s&#94;2  + (\tilde{X} - \tilde{X}\hat{\beta})'\left[I + \tilde{X}V\tilde{X}' \right]&#94;{-1}(\tilde{X} - \tilde{X}\hat{\beta}) \right)$ and we substitute $z = \frac{A}{2\sigma&#94;2}$ as described in "Quadratic form statistics" on wikipedia (see below) to produce
  * $$
  * p(\tilde{Y}|Y,\tilde{X},X) \propto A&#94;{-\frac{\nu + 1}{2}}\int z&#94;{(\nu - 1)/2}\exp(-z)dz
  * $$
  * which evaluates to
  * $$
  * \left[ 1 + \frac{(\tilde{X} - \tilde{X}\hat{\beta})'\left[I + \tilde{X}V\tilde{X}' \right]&#94;{-1}(\tilde{X} - \tilde{X}\hat{\beta})}{\nu s&#94;2} \right]&#94;{-\frac{1}{2}(\nu + 1)}
  * $$
  * which is a multivariate t-distribution centered at $\tilde{X}\hat{\beta}$ and scaled by $[I+\tilde{X}V\tilde{X}'] s&#94;2$ with $\nu = n - k$ degrees of freedom.
  *
  * The posterior predictive density can be used to draw samples in performing simulation of the distribution of $\tilde{Y}$.
  *
  * <h3>Credible Intervals and Highest Posterior Density</h3>
  *
  * Note that earlier the joint posterior distributions of the parameters were derived for $p(\beta|\sigma&#94;2,Y,X)$ and $p(\sigma&#94;2|Y,X)$.
  * Note also that once $\sigma&#94;2$ is marginalised the posterior distribution for $p(\beta|Y,X)$ is
  *
  * $$
  * p(\beta|Y,X) \propto \left[ 1 + \frac{(\beta - \hat{\beta})'X'X(\beta-\hat{\beta})}{\nu s&#94;2} \right] &#94; {-\frac{1}{2}(\nu + k)}
  * $$
  *
  * which is a multivariate t-distribution centred at $\hat{\beta}$ and scaled by $X'Xs&#94;2$ with $\nu = n - k$ degrees of freedom.
  *
  * Using the posterior distributions it is possible to obtain credible intervals for the parameters and for the distribution of the expected values of the target variable
  * at a given critical level $\alpha$.
  *
  * The intervals of interest include credible intervals for the mean coefficient parameters $\beta$ under the conditional distiribution $p(\beta|Y,X)$
  * and the credible intervals for the uniform variance $\sigma&#94;2$ given $Y$, $p(\sigma&#94;2|Y)$.
  *
  * When seeking credible intervals for $\beta$ using the posterior multivariate t-distribution $p(\beta|Y,X)$ when variance is assumed known we can make use of the
  * t values for the $1-\alpha$ level at $\nu = n - k$ degrees of freedom scaled by $(X'X)s&#94;2$.
  *
  * $$
  * \hat{\beta} \pm t_{\nu,\alpha/2}\sqrt{(X'X)s&#94;2}
  * $$
  *
  * Since $\sigma&#94;2$ is takne to be of uniform variance it is taken as a univariate parameter. The credible interval is for the univeriate $Inverse-\chi&#94;2$ distribution with degree of freedom $\nu$ and scale $s&#94;2$.
  * The distribution of $W = \frac{s&#94;2}{\sigma&#94;2}$ has the $\chi&#94;2$ distribution of $\nu$ degrees of freedom, the credible intervals are calculated by
  * inverting this quantity such that
  *
  * $$
  * P\left(u < \frac{s&#94;2}{\sigma&#94;2} < l\right) = 1 - \alpha
  * $$
  *
  * $$
  * P \left(\frac{s&#94;2}{l} < \sigma&#94;2 < \frac{s&#94;2}{u} \right) = 1 - \alpha
  * $$
  * with $u$ for the upper and $l$ for the lower tail areas.
  * This can be expressed as
  *
  * $$
  * \sigma&#94;2 \pm \frac{s&#94;2}{\chi&#94;2_{\nu,\alpha2}}
  * $$
  *
  * <h3>Highest Posterior Density</h3>
  *
  * The credible interval is extended by the "Highest Posterior Density" (Box and Tiao) which proides a representation of the contours of a multivariate distribution
  * at a number of significant levels for $1 - \alpha$. The highest posterior density is a useful tool for the visualisation of credible regions of the parameter space and
  * for visual comparison of parameter estimates. The parameter space may be partitioned such that
  *
  * $$
  * \beta = \begin{bmatrix}
  * \beta_1 \\
  * \beta_2
  * \end{bmatrix} \begin{matrix}
  * r \\
  * k - r
  * \end{matrix}
  * $$
  * $$
  * \hat{\beta} = \begin{bmatrix}
  * \hat{\beta}_1 \\
  * \hat{\beta}_2
  * \end{bmatrix} \begin{matrix}
  * r \\
  * k - r
  * \end{matrix}
  * $$
  * *
  * $$
  * X'X = \begin{bmatrix}
  * ~ & r & k - r \\
  * r & X_1'X_1 & X_1'X_2 \\
  * k - r & X_2'X_1 & X_2'X_2
  * \end{bmatrix}
  * $$
  * *
  * $$
  * (X'X)&#94;{-1} = C = \begin{bmatrix}
  * ~ & r & k - r \\
  * r & C_{11} & C_{12} \\
  * k - r & C_{21} & C_{22}
  * \end{bmatrix}
  * $$
  * *
  * where $r < k$. The subset $\theta_i$ has the property of being normally distributed
  * $$
  * p(\theta_i|\sigma&#94;2,Y,X) \sim N\left(\hat{\theta_i}, \sigma&#94;2c_{ii}\right)
  * $$
  * where $c_ii$ is the $ith$ diagonal element of $C$, and the conditional distribution of $p(\theta_2|\theta_1,\sigma&#94;2,Y,X)$ is normally distributed such that
  * $$
  * p(\theta_2|\theta_1,\sigma&#94;2,Y,X) \sim N\left(\hat{\theta_{2,1}}, \sigma&#94;2(X_2'X)&#94;{-1}\right)
  * $$
  * where $\hat{\theta_{2,1}} = \hat{\theta_2} + (X_2'X_2)&#94;{-1}X_2X_1(\theta_1 - \hat{\theta_1})$.
  * Once partitioned the regions of the highest posterior density can be computed for the parameters $\theta_1$ and $\theta_2$ and charted for comparison.
  *
  * <h3>Update Rules</h3>
  *
  *
  * The bayesian method allows the model to be adjusted in light of new observations by using the posterior distribution as the prior distribution. This allows the parameters to be adjusted given new evidence. The posterior distribution for the parameters factors into
  * $$
  * p(\theta,\sigma&#94;2|Y,X) \propto p(\theta|\sigma&#94;2,Y,X) \times p(\sigma&#94;2|Y,X)
  * $$
  * $$
  * p(\theta,\sigma&#94;2|Y,X) \propto N\left(\hat{\beta},\sigma&#94;2V_\beta\right) \times Inverse-\chi&#94;2(\nu,s&#94;2)
  * $$
  * For the uniform variance it is possible to update first the distribution of $\sigma&#94;2$ and then the distribution of $\beta|\sigma&#94;2$.
  * Since in the posterior distribution of $\sigma&#94;2$ we have two parameters the degrees of freedom $\nu = n - k$ and
  * the scale $s&#94;2 = \frac{1}{\nu}(Y - X\hat{\beta})'(Y-X\hat{\beta})$.
  * When faced with a new set of evidence and observation variables $X_{new}$  with rank $k$ and $Y_{new}$ having variance $\sigma&#94;2$
  * we consider the distribution of the variance in the new data to also be the $Inverse-\chi&#94;2$ distribution with $n_{new} - k$ degrees of freedom
  * and scale $s'&#94;2 = \frac{1}{n_{new} - k}(Y_{new} - X_{new}\hat{\beta})'(Y_{new} - X_{new}\hat{\beta})$.
  * As this is the conjugate prior the update rules (from Boltstad) are as follows.
  * $$
  * s_{n}&#94;2 = s&#94;2 + \frac{1}{n_{new} - k}(Y_{new} - X_{new}\hat{\beta})'(Y_{new} - X_{new}\hat{\beta})
  * $$
  * and the degree of freedom
  * $$
  * \nu_{n} = \nu + (n_{new} - k)
  * $$
  * This gives the updated distribution for $\sigma&#94;2$ as $Inverse-\chi&#94;2(\nu_{n}, s_{n}&#94;2)$.
  * The normal distribution for $p(\theta|\sigma&#94;2,Y,X)$ has the initial parameters
  * $$
  * \mu_0 = \hat{\beta} = V_\beta X'y
  * $$
  * $$
  * \Lambda_0 = V_\beta
  * $$
  * where $V_\beta = (X'X)&#94;{-1}$.
  * When performing the update with the new data we consider the new data to be normally distributed with uniform variance such that $p(\mu|\Sigma,Y_{new},X_{new}) \sim N\left(\mu,\Sigma/n_{new}\right)$.
  * The normal is the conjugate prior so that the update rules can be applied as follows
  * $$
  * \Lambda_n = \Lambda_0&#94;{-1} + \Sigma&#94;{-1}
  * $$
  * $$
  * \mu_n = (\Lambda_0&#94;{-1} + \Sigma&#94;{-1})&#94;{-1}(\Lambda_0&#94;{-1}\mu_0 + \Sigma&#94;{-1}\bar{Y}_{new})
  * $$
  * where $\Sigma = (X_{new}'X_{new})$. The updated distribution for $\beta$ is
  * $$
  * p(\beta|\sigma&#94;2,Y_{new},Y,X_{new},X) \sim N\left(\mu_n, \sigma&#94;2\Lambda_n\right)
  * $$
  * The updated joint distribution becomes
  * $$
  * p(\beta,\sigma&#94;2|Y_{new},Y,X_{new},X) \propto  N\left(\mu_n, \sigma&#94;2\Lambda_n\right) \times Inverse-\chi&#94;2(\nu_n, s_n&#94;2)
  * $$
  *
  *
  *
  *
  * <h3>References</h3>
  *
  * For further details see:
  *
  * Box George E P,Tiao George C, <i>Bayesian Inference in Statistical Analysis</i>. Wiley 1992.
  *
  * Gelman Andrew, Carlin John B, Stern Hal S, Dunson David B, Vehtari Aki, Rubin Donald B, <i>Bayesian Data Analysis</i>. Chapman and Hall/CRC Press Taylor and Francis Group 3rd edition, 2013
  *
  * Bishop Christopher M. <i>Pattern Recognition and Machine Learning</i>. Springer Science + Business Media LLC 2006
  *
  * Bolstad William M, <i>Introduction to Bayesian Statistics</i>. Wiley 2nd edition, 2007.
  *
  * "Quadratic form (statistics)", <i>Wikipedia</i> 29th July 2015, 5 Nov 2015. <a href="https://en.wikipedia.org/wiki/Quadratic_form_(statistics)">https://en.wikipedia.org/wiki/Quadratic_form_(statistics)</a>
  *
  *
  * <h3>Implementation and Usage</h3>
  *
  * This class implements the bayes linear regression for uniform variance.
  * It extends the OLS method and adds an Update method in order to update the generated model in light of new data
  * using the update rules above.
  *
  *
  * This class
  *
  * Created by cd on 17/04/2016.
  */
class BayesLinearRegression(@transient var inX: DenseMatrix[Double], @transient var inY: DenseVector[Double], override val m: Int = 1)
  extends OrdLeastSquares(inX, inY, m) {


  /**
    * implement the update rules for the linear regression model after already having trained the model.
    *
    * This will update the parameter matrix Beta and the variance.
    *
    * it will not predict against the data, after updating use the predict methods to get a prediction and calculate residuals.
    *
    * @param newX
    * @param newY
    */
  def update(newX: DenseMatrix[Double], newY: DenseVector[Double]) = {
    val newP = createPolynomial(newX, m)

    val newY1 = DenseMatrix.tabulate[Double](newY.length, 1) { case (i, j) => newY(i) }

    val prevB = Beta
    val prevSigma = variance

    val lastdf = P.rows - P.cols
    val nu = lastdf + (newP.rows - newP.cols)

    val prevMu = (1.0 / Y.length) * Y.foldLeft(0.0) { (accum, y) => accum + y }

    val mu = (1.0 / newY.length) * newY.foldLeft(0.0) { (accum, y) => accum + y }

    val prevSd = (1.0 / (Y.length - 1)) * Y.foldLeft(0.0) { (accum, y) => accum + Math.pow((y - prevMu), 2) }

    val sd = (1.0 / (newY.length - 1)) * newY.foldLeft(0.0) { (accum, y) => accum + Math.pow((y - mu), 2) }

    val muC = (prevMu + mu) / 2.0

    // the new uniform variance
    val sigma = (P.rows * (prevSd + Math.pow(prevMu - muC, 2)) + newP.rows * (sd + Math.pow(mu - muC, 2))) / (P.rows + newP.rows)
    val newVariance = Math.pow(sigma, 2)

    /**
      * determine the new value for $\sigmaI$ from the prior work I've done in my research assignments
      * the new $I$ is not the identity matrix, but it taken from the diagonal of the $V_\beta = (X'X)^{-1}$ parameter.
      * The diagonal is used in the OLS as the $v_j$ parameter for the variance of the $\beta$ parameter.
      **/
    val newI = DenseMatrix.tabulate[Double](pseudoInverse.rows, pseudoInverse.cols) {
      case (i, j) => i == j match {
        case true => {
          // uniform variance
          // sigma
          // based on previous beta variance
          pseudoInverse(j, j)
        }
        case _ => 0.0
      }
    }
    val mu0 = Beta
    val lambda0 = pseudoInverse
    val cov = DenseMatrix.tabulate(newI.rows, newI.cols) {
      case (i,j) => newVariance * newI(i,j)
    }
    val lambdaNew = lambda0 + cov

    /**
      * from R
      *
      * ones of mu2
      *
      * MeanVec <- as.matrix(sample(x = c(1), size = nrow(Mu0), replace = TRUE) * mu2)
      * *
      *
      *
      * Mu2 <- ginv(ginv(Lambda0) + n2*ginv(Lambda2)) %*%(ginv(Lambda0)%*%Mu0 + n2*ginv(Cov)%*%MeanVec)
      * Mu2 <- data.frame(Mu2)
      * rownames(Mu2) <- rownames(Mu0)
      * result2$V <- Lambda2
      * result2$B <- as.matrix(Mu2)
      */

    /**
      * calculate the new Beta
      *
      * Mu2 <- ginv( ginv(Lambda0) + n2*ginv(Lambda2) ) %*%(ginv(Lambda0)%*%Mu0 + n2*ginv(Cov)%*%MeanVec)
      * Mu2 <- data.frame(Mu2)
      * result2$B <- as.matrix(Mu2)
      */
    // inverse of the inverse
    val meanVec = DenseMatrix.ones[Double](mu0.t.rows, 1) * mu
    val invLambda1 = PseudoInverse(lambda0)
    val invLambda2 = PseudoInverse(lambdaNew)
    val invCov = PseudoInverse(cov)
    val mat1 = invLambda1 + (invLambda2 * newP.rows.toDouble)
    val a = mu0 * invLambda1
    val b = meanVec.t * (invCov * newP.rows.toDouble)

    val mat2 = DenseMatrix.tabulate[Double](b.rows, b.cols) {
      case (i, j) => b(i, j) + a(0,j)
    }

    /**
      *  Mu2 <- ginv( ginv(Lambda0) + n2*ginv(Lambda2) ) %*%(ginv(Lambda0)%*%Mu0 + n2*ginv(Cov)%*%MeanVec)
      * becomes
      * ginv(mat1) * (a + b)
      */

    val newBeta = mat2 * PseudoInverse(mat1)
    // update the parameters
    pseudoInverse = lambdaNew
    Beta = newBeta
    variance = newVariance

    // estimate $\hat{Y}$
    val yHat = Beta * newP.t
    residuals = (newY1 - yHat.t).toDenseVector
    mse = squaredError(yHat.t, newY1)

    // update the z-values and p-values
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

    val criticalValue = CriticalValue.upperCriticalValue(0.0, 4 * 1.0, stdNorm)
    criticalBetaZValue = criticalValue.value(alpha)
    criticalPValue = stdNorm.pdf(criticalBetaZValue)

    (yHat, mse)
  }

}

object BayesLinearRegression {
  def apply(inX: DenseMatrix[Double], inY: DenseVector[Double], m: Int = 1) =
    new BayesLinearRegression(inX, inY, m)
}