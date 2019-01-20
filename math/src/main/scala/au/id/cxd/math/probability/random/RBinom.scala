package au.id.cxd.math.probability.random

import scala.annotation.tailrec

/**
  * Draw random variates from the binomial distribution.
  * This implementation is derived from the GSL implementation in randist/binomial_tpe.c
  * The method is based on gsl_ran_binomial.
  */
class RBinom(val n: Double, val p: Double) extends RandomDeviate {

  val SMALL_MEAN: Double = 14.0
  val BINV_CUTOFF: Double = 110.0
  val FAR_FROM_MEAN: Double = 20.0

  val uniform = RUniform()

  // probability of failure
  val q: Double = 1.0 - p
  // ratio of probabilities odds.
  val s: Double = p / q
  // mean
  val np: Double = n * p

  /**
    * approximation
    *
    * From GSL binomial_tpe.c:
    *
    * The "#define Stirling" above corresponds to the first five
    * terms in asymptoic formula for
    * log Gamma (y) - (y-0.5)log(y) + y - 0.5 log(2*pi);
    * See Abramowitz and Stegun, eq 6.1.40
    *
    * @param y
    * @return
    */
  @inline
  def stirling(y: Double) = {
    val y2 = y * y
    val s = (13860.0 -
      (462.0 - (132.0 - (99.0 - 140.0 / y2) / y2) / y2) / y2) / y / 166320.0
    s
  }

  /**
    * need to draw a random uniform value and iterate until maximum cutoff
    * the result computes the number of steps x from the binomial.
    *
    */
  def smallMeanEstimate(q: Double): Double = {
    val f0 = Math.pow(q, n)

    @tailrec
    def innerDraw(ix: Double, u: Double, f: Double): Double = {

      if (u < f) ix
      else {
        val u1 = u - f
        val f1 = f * s * (n - ix) / (ix + 1.0)
        if (ix >= BINV_CUTOFF) ix
        else innerDraw(ix + 1.0, u1, f1)
      }
    }

    val ix = innerDraw(0.0, uniform.draw(), f0)
    ix
  }

  @tailrec
  final def triangleCases(params: RBinomParameters, outputs: RBinomOutputs): RBinomOutputs = {
    val u = uniform.draw() * params.p4
    val v = uniform.draw()
    /* Triangular region */
    val (xi1, v1, u1): (Double, Double, Double) = if (u <= params.p1)
      (Math.round(params.xm - params.p1 * v + u), u, v)
    else if (u <= params.p2) {
      /* Parallelogram region */
      val x = Math.round(params.xl + (u - params.p1) / params.c)
      val v1 = v * params.c + 1.0 - Math.abs(x - params.xm) / params.p1
      if (v1 > 1.0 || v <= 0.0) (x, u, v)
      else (outputs.ix, u, v)
    } else if (u < params.p3) {
      /* Left tail */
      val x = Math.round(params.xl + Math.log(v) / params.lambda_l)
      (x, u, v)
    } else {
      /* Right tail */
      val x = Math.round(params.xr - Math.log(v) / params.lambda_r)
      val v1 = ((u - params.p3) * params.lambda_l)
      (x,u,v)
    }
    if (v1 <= 1.0 && v1 > 0.0) RBinomOutputs(xi1, outputs.vari, outputs.accept, u1, v1)
    else if (xi1 > n) RBinomOutputs(xi1, outputs.vari, outputs.accept, u1, v1 * (u1 - params.p2) * params.lambda_l)
    else triangleCases(params, RBinomOutputs(xi1, outputs.vari, outputs.accept, u1, v1))
  }

  /**
    * compute the squeeze on the outputs where the distance from mean is large or where the variance is too high.
    *
    * @param params
    * @param outputs
    * @return
    */
  @tailrec
  final def squeeze(params: RBinomParameters, outputs: RBinomOutputs): RBinomOutputs = {
    val k = Math.abs(outputs.ix - params.fm)
    val (vari, accept) = if (k <= FAR_FROM_MEAN) {
      val g = (n + 1.0) * params.s
      val f1:Double = 1.0
      if (params.fm < outputs.ix) {
        @tailrec
        def computeF(f: Double, i: Double, maxI: Double): Double = {
          if (i > maxI) f
          else computeF(f * (g / i - params.s), i + 1.0, maxI)
        }

        val f:Double = computeF(f1, params.fm + 1.0, outputs.ix)
        (outputs.v, f)
      } else if (params.fm > outputs.ix) {
        @tailrec
        def computeF(f: Double, i: Double, maxI: Double): Double = {
          if (i > maxI) f
          else computeF(f / (g / i - params.s), i + 1.0, maxI)
        }

        val f:Double = computeF(f1, outputs.ix + 1.0, params.fm)
        (outputs.v, f)
      } else (outputs.v, outputs.accept)
    } else {
      val vari1 = Math.log(outputs.v)
      /* Note below: two Stirling's are added, and two are
           * subtracted.  In both K+S, and in the ranlib
           * implementation, all four are added.  I (jt) believe that
           * is a mistake -- this has been confirmed by personal
           * correspondence w/ Dr. Kachitvichyanukul.  Note, however,
           * the corrections are so small, that I couldn't find an
           * example where it made a difference that could be
           * observed, let alone tested.  In fact, define'ing Stirling
           * to be zero gave identical results!!  In practice, alv is
           * O(1), ranging 0 to -10 or so, while the Stirling
           * correction is typically O(10^{-5}) ...setting the
           * correction to zero gives about a 2% performance boost;
           * might as well keep it just to be pendantic.  */
      val x1 = outputs.ix + 1.0
      val w1 = n - outputs.ix + 1.0
      val f1 = params.fm + 1.0
      val z1 = n + 1.0 - params.fm
      val accept1 = params.xm * Math.log(f1 / x1) + (n - params.fm + 0.5) * Math.log(z1 / w1) +
        (outputs.ix - params.fm) * Math.log(w1 * params.p / (x1 * params.q)) +
        stirling(f1) + stirling(z1) - stirling(x1) - stirling((w1))
      (vari1, accept1)
    }
    val repeatflag = if (k <= FAR_FROM_MEAN) false
    else if (k < params.npq / 2.0 - 1.0) {
      /* "Squeeze" using upper and lower bounds on
              * log(f(x)) The squeeze condition was derived
              * under the condition k < npq/2-1 */
      val amaxp = k / params.npq * ((k * (k / 3.8 + 0.625) + (1.0 / 6.0)) / params.npq + 0.5)
      val ynorm = -(k * k / (2.0 * params.npq))
      if (vari > ynorm - amaxp) false
      else true
    } else false

    if (vari <= accept) RBinomOutputs(outputs.ix, vari, accept, outputs.u, outputs.v)
    else if (!repeatflag) RBinomOutputs(outputs.ix, vari, accept, outputs.u, outputs.v)
    else squeeze(params, triangleCases(params, RBinomOutputs(outputs.ix, vari, accept, outputs.u, outputs.v)))
  }

  /**
    * compute the number of steps required for the event of probability p
    * @param p
    * @param q
    * @param np
    * @return
    */
  def triangularEstimate(p: Double, q: Double, np: Double): Double = {
    val params = RBinomParameters(p, q, np)
    // initialise the outputs
    val u = uniform.draw() * params.p4
    val v = uniform.draw()
    val outputs = RBinomOutputs(0.0, 0.0, 0.0, u, v)
    val nextOutputs = squeeze(params, triangleCases(params, outputs))
    nextOutputs.ix
  }

  /**
    * random draw from a distribution
    *
    * @return
    */
  override def draw(): Double = {
   @tailrec
   def innerDraw() : Double = {
     // the probability is swapped when p > 0.5
     val d = if (p > 0.5) {
       val ix = if (np < SMALL_MEAN) smallMeanEstimate(p)
       else triangularEstimate(1.0 - p, p, n * (1.0 - p))
       n - ix
     } else {
       val ix = if (np < SMALL_MEAN) smallMeanEstimate(1.0 - p)
       else triangularEstimate(p, q, n * p)
       ix
     }
     if (d >= 0) d
     else innerDraw()
   }
   innerDraw()
  }
}
object RBinom {
  def apply(n: Double, p: Double) = new RBinom(n,p)
}