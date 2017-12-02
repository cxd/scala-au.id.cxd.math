package au.id.cxd.math.function.hypergeometric

import au.id.cxd.math.function.Constants
import au.id.cxd.math.function.exponential.{ExpErr, ExpMult}
import au.id.cxd.math.function.gamma.{Digamma, LogGammaFn}

import scala.annotation.tailrec
import scala.util.{Success, Try}

/**
  * implementation of GSL hyperg_2F1.c method gsl_sf_hyperg_2F1e
  *
  * the gaussian hypergeometric series.
  */
class GaussHypergeometric {

  val logEPS = (1000.0 * Constants.DBL_EPSILON)

  /**
    * GSL Implementation hyperg_2F1_series from hyperg_2F1.c line 37
    *
    * Assumes c != negative integer.
    */
  private def hyperg_2F1_series(a: Double, b: Double, c: Double, x: Double) = {

    def result(k: Double, sum_pos: Double, sum_neg: Double, del_pos: Double, del_neg: Double) = {
      val y = sum_pos - sum_neg
      val err1 = del_pos + del_neg + 2.0 * Constants.DBL_EPSILON * (sum_pos + sum_neg)
      val err2 = err1 + 2.0 * Constants.DBL_EPSILON * (2.0 * Math.sqrt(k) + 1.0 * Math.abs(y))
      (y, err2)
    }

    @tailrec def seriesFn(k: Double, del: Double, sum_pos: Double, sum_neg: Double, del_pos: Double, del_neg: Double): (Double, Double) = {
      if (k + 1.0 > 30000) {
        val result1 = result(k, sum_pos, sum_neg, del_pos, del_neg)
        result1
      } else {
        val del1 = del * (a + k) * (b + k) * x / ((c + k) * (k + 1.0))
        val (terminate, sum_pos1, sum_neg1, del_pos1, del_neg1) = if (del1 > 0.0) {
          (false, sum_pos + del1, sum_neg, del, del_neg)
        } else if (del1 == 0.0) {
          (true, sum_pos, sum_neg, 0.0, 0.0)
        } else {
          (false, sum_pos, sum_neg - del, del_pos, del_neg - del)
        }
        if (terminate) {
          val result2 = result(k, sum_pos1, sum_neg1, del_pos1, del_neg1)
          (result2)
        } else if (Math.abs(del / (sum_pos1 - sum_neg1)) < Constants.DBL_EPSILON &&
          Math.abs(del1 / (sum_pos1 - sum_neg1)) < Constants.DBL_EPSILON) {
          val result3 = result(k, sum_pos1, sum_neg1, del_pos1, del_neg1)
          result3
        } else if (Math.abs((del_pos1 + del_neg1) / (sum_pos1 - sum_neg1)) <= Constants.DBL_EPSILON) {
          val result4 = result(k, sum_pos1, sum_neg1, del_pos1, del_neg1)
          result4
        } else {
          seriesFn(k + 1.0, del1, sum_pos1, sum_neg1, del_pos1, del_neg1)
        }
      }
    }

    val k = 0.0
    val del = 1.0
    val sum_pos = 1.0
    val sum_neg = 0.0
    val del_pos = 1.0
    val del_neg = 0.0
    seriesFn(k, del, sum_pos, sum_neg, del_pos, del_neg)
  }

  /**
    * Implementation of GSL Luke's rational approximation
    * from file hyperg_2F1.c line 171
    *
    * Note the comments from that file indicate the obscurity of this function:
    *
    * "Luke's rational approximation. The most accesible
    * discussion is in [Kolbig, CPC 23, 51 (1981)].
    * The convergence is supposedly guaranteed for x < 0.
    * You have to read Luke's books to see this and other
    * results. Unfortunately, the stability is not so
    * clear to me, although it seems very efficient when
    * it works."
    *
    * @param a
    * @param b
    * @param c
    * @param xin
    */
  def hyperg_2F1_luke(a: Double, b: Double, c: Double, xin: Double) = {
    val RECUR_BIG: Double = 1.0e+50
    val nmax: Double = 20000
    val n: Double = 3
    val x = -xin
    val x3 = x * x * x
    val t0 = a * b / c
    val t1 = (a + 1.0) * (b + 1.0) / (2.0 * c)
    val t2 = (a + 2.0) * (b + 2.0) / (2.0 * (c + 1.0))
    val F = 1.0
    val Bnm3 = 1.0
    val Bnm2 = 1.0 + t1 * x
    val Bnm1 = 1.0 + t2 * x * (1.0 + t1 / 3.0 * x)
    val Anm3 = 1.0
    val Anm2 = Bnm2 - t0 * x
    val Anm1 = Bnm1 - t0 * (1 + t2 * x) * x + t0 * (c / (c + 1)) * x * x


    @tailrec def approximate(n: Int, F: Double, Anm1: Double, Anm2: Double, Anm3: Double, Bnm1: Double, Bnm2: Double, Bnm3: Double): (Double, Double, Double) = {
      val npam1 = n + a - 1
      val npbm1 = n + b - 1
      val npcm1 = n + c - 1
      val npam2 = n + a - 2
      val npbm2 = n + b - 2
      val npcm2 = n + c - 2
      val tnm1 = 2 * n - 1
      val tnm3 = 2 * n - 3
      val tnm5 = 2 * n - 5
      val n2 = n * n
      val F1 = (3.0 * n2 + (a + b - 6) * n + 2 - a * b - 2 * (a + b)) / (2 * tnm3 * npcm1)
      val F2 = -(3.0 * n2 - (a + b + 6) * n + 2 - a * b) * npam1 * npbm1 / (4 * tnm1 * tnm3 * npcm2 * npcm1)
      val F3 = (npam2 * npam1 * npbm2 * npbm1 * (n - a - 2) * (n - b - 2)) / (8 * tnm3 * tnm3 * tnm5 * (n + c - 3) * npcm2 * npcm1)
      val E = -npam1 * npbm1 * (n - c - 1) / (2 * tnm3 * npcm2 * npcm1)

      val An = (1.0 + F1 * x) * Anm1 + (E + F2 * x) * x * Anm2 + F3 * x3 * Anm3
      val Bn = (1.0 + F1 * x) * Bnm1 + (E + F2 * x) * x * Bnm2 + F3 * x3 * Bnm3
      val r = An / Bn

      val prec = Math.abs((F - r) / F)
      val Fnew = r

      if (prec < Constants.DBL_EPSILON || n > nmax) {
        (Fnew, prec, n)
      } else if (Math.abs(An) > RECUR_BIG || Math.abs(Bn) > RECUR_BIG) {
        val An1 = An / RECUR_BIG
        val Bn1 = Bn / RECUR_BIG
        val Anm1new = Anm1 / RECUR_BIG
        val Bnm1new = Bnm1 / RECUR_BIG
        val Anm2new = Anm2 / RECUR_BIG
        val Bnm2new = Bnm2 / RECUR_BIG
        val Anm3new = Anm3 / RECUR_BIG
        val Bnm3new = Bnm3 / RECUR_BIG
        approximate(n + 1, Fnew, An1, Anm1new, Anm2new, Bn1, Bnm1new, Bnm2new)

      } else if (Math.abs(An) < 1.0 / RECUR_BIG || Math.abs(Bn) < 1.0 / RECUR_BIG) {
        val An1 = An * RECUR_BIG
        val Bn1 = Bn * RECUR_BIG
        val Anm1new = Anm1 * RECUR_BIG
        val Bnm1new = Bnm1 * RECUR_BIG
        val Anm2new = Anm2 * RECUR_BIG
        val Bnm2new = Bnm2 * RECUR_BIG
        val Anm3new = Anm3 * RECUR_BIG
        val Bnm3new = Bnm3 * RECUR_BIG

        approximate(n + 1, Fnew, An1, Anm1new, Anm2new, Bn1, Bnm1new, Bnm2new)

      } else {
        approximate(n + 1, Fnew, An, Anm1, Anm2, Bn, Bnm1, Bnm2)
      }

    }

    val result = approximate(n.toInt, F, Anm1, Anm2, Anm3, Bnm1, Bnm2, Bnm3)
    val y = result._1
    val err1 = 2.0 * Math.abs(result._2 * result._1)
    val err2 = err1 + 2.0 * Constants.DBL_EPSILON * (result._3 + 1.0) * Math.abs(result._1)
    val err3 = err2 * 8.0 * (Math.abs(a) + Math.abs(b) + 1.0)
    (y, err3)
  }

  /**
    * A copy of the implementation hyperg_2F1_reflect from the GSL
    * hyperg_2F1.c line 367.
    *
    *
    * Assumes a,b,c != neg integer.
    *
    * @param a
    * @param b
    * @param c
    * @param x
    */
  def hyperg_2F1_reflect(a: Double, b: Double, c: Double, x: Double) = {
    val d = c - a - b
    val intd = Math.floor(d + 0.5)
    val d_is_integer = (Math.abs(d - intd) < logEPS)
    d_is_integer match {
      case true => hyperg_2F1_reflect_dint(a, b, c, d, x)
      case _ => hyperg_2F1_reflect_d_nonint(a, b, c, d, x)
    }
  }

  /**
    * compute the hyperg_2F1_reflect method where d is an integer.
    *
    * @param a
    * @param b
    * @param c
    * @param d
    * @param x
    * @return
    */
  def hyperg_2F1_reflect_dint(a: Double, b: Double, c: Double, d: Double, x: Double) = {
    val intd = Math.floor(d + 0.5)
    val ln_omx = Math.log(1.0 - x)
    val ad = Math.abs(d)
    val (d1, d2) =
      if (d >= 0.0) (d, 0.0)
      else (0.0, d)
    val ln_ad2 = LogGammaFn(a + d2)
    val ln_bd2 = LogGammaFn(b + d2)
    val ln_c = LogGammaFn(c)
    val F1 = if (ad < Constants.DBL_EPSILON) {
      (0.0, 0.0)
    } else {

      val ln_ad = LogGammaFn(ad)
      val ln_ad1 = LogGammaFn(a + d1)
      val ln_bd1 = LogGammaFn(b + d1)
      // note in our implementation LogGammaFn will throw an exception if it cannot be evaluated.
      // hence there is no additional error condition

      val ln_pre1_val = ln_ad._1 + ln_c._1 + d2 * ln_omx - ln_ad1._1 - ln_bd1._1
      val ln_pre1_err = ln_ad._2 + ln_c._2 + ln_ad1._2 + ln_bd1._2 + Constants.DBL_EPSILON * Math.abs(ln_pre1_val)

      def consume(term: Double, sum: Double, i: Double) = {
        val j = i - 1
        val term1 = term * (a + d2 + j) * (b + d2 + j) / (1.0 + d2 + j) / i * (1.0 - x)
        val sum1 = sum + term
        (term1, sum1)
      }

      val (term1, sum1) = (for (i <- 1 until ad.toInt) yield i).toList.foldLeft(1.0, 1.0) {
        (accum, i) => consume(accum._1, accum._2, i.toDouble)
      }
      ExpMult(ln_pre1_val, ln_pre1_err, sum1, Constants.DBL_EPSILON * Math.abs(sum1))
    }

    val maxiter = 2000
    val psi_1 = Constants.EULER
    val psi_1pd = Digamma(1.0 + ad)
    val psi_apd1 = Digamma(a + d1)
    val psi_bpd1 = Digamma(b + d1)
    val psi_val = psi_1 + psi_1pd._1 + psi_apd1._1 - psi_bpd1._1 - ln_omx
    val psi_err = psi_1pd._2 + psi_apd1._2 + psi_bpd1._2 + Constants.DBL_EPSILON * Math.abs(psi_val)

    val fact = 1.0
    val sum2_val = psi_val
    val sum2_err = psi_err
    val ln_pre2_val = ln_c._1 + d1 * ln_omx - ln_ad2._1 - ln_bd2._1
    val ln_pre2_err = ln_c._2 + ln_ad2._2 + ln_bd2._2 + Constants.DBL_EPSILON * Math.abs(ln_pre2_val)

    @tailrec def aggregate(j: Int, psi_val1: Double, psi_err1: Double, sum2_val1: Double, sum2_err1: Double, fact1: Double): (Double, Double) = {
      j == maxiter match {
        case true => (sum2_val1, sum2_err1)
        case _ => {

          val term1 = 1.0 / j.toDouble + 1.0 / (ad + j.toDouble)
          val term2 = 1.0 / (a + d1 + j.toDouble - 1.0) + 1.0 / (b + d1 + j.toDouble - 1.0)
          val psi_val2 = psi_val1 + term1 - term2
          val psi_err2 = psi_err1 + Constants.DBL_EPSILON * (Math.abs(term1) + Math.abs(term2))

          val fact2 = (a + d1 + j.toDouble - 1.0) * (b + d1 + j.toDouble - 1.0) / (((ad + j.toDouble) * j.toDouble) * (1.0 - x))
          val delta = fact2 * psi_val2
          val sum2_val2 = sum2_val1 + delta
          val sum2_err2 = sum2_err1 + Math.abs(fact * psi_err2) + Constants.DBL_EPSILON * Math.abs(delta)
          if (Math.abs(delta) < Constants.DBL_EPSILON * Math.abs(sum2_val2)) (sum2_val2, sum2_err2)
          else aggregate(j + 1, psi_val2, psi_err2, sum2_val2, sum2_err2, fact2)
        }
      }
    }

    val (sum2_val1, sum2_err1) = aggregate(1, psi_val, psi_err, sum2_val, sum2_err, fact)
    val F2 = if (sum2_val1 == 0.0) {
      (0.0, 0.0)
    } else {
      ExpMult(ln_pre2_val, ln_pre2_err, sum2_val1, sum2_err1)
    }
    val sgn2 = if (intd % 2.0 == 0.0) 1.0
    else -1.0
    val result_val = F1._1 + sgn2 * F2._1
    val result_err1 = F1._2 + F2._2
    val result_err2 = result_err1 + 2.0 * Constants.DBL_EPSILON * (Math.abs(F1._1) + Math.abs(F2._1))
    val result_err3 = result_err2 + 2.0 * Constants.DBL_EPSILON * Math.abs(result_val)
    (result_val, result_err3)
  }

  /**
    *
    * compute the hyperg_2F1_reflect method where d is a non-integer.
    *
    * @param a
    * @param b
    * @param c
    * @param d
    * @param x
    */
  def hyperg_2F1_reflect_d_nonint(a: Double, b: Double, c: Double, d: Double, x: Double) = {

    val (sgn_g1ca, ln_g1ca, err_g1ca) = Try {
      LogGammaFn.withSign(c - a)
    } getOrElse {
      (0.0, 0.0, 0.0)
    }
    val (sgn_g1cb, ln_g1cb, err_g1cb) = Try {
      LogGammaFn.withSign(c - b)
    } getOrElse {
      (0.0, 0.0, 0.0)
    }
    val (sgn_g2a, ln_g2a, err_g2a) = Try {
      LogGammaFn.withSign(a)
    } getOrElse {
      (0.0, 0.0, 0.0)
    }
    val (sgn_g2b, ln_g2b, err_g2b) = Try {
      LogGammaFn.withSign(b)
    } getOrElse {
      (0.0, 0.0, 0.0)
    }

    val ok1 = sgn_g1ca != 0.0 && sgn_g1cb != 0.0
    val ok2 = sgn_g2a != 0.0 && sgn_g2b != 0.0

    val (sgn_gc, ln_gc, err_gc) = LogGammaFn.withSign(c)
    val (sgn_gd, ln_gd, err_gd) = LogGammaFn.withSign(d)
    val (sgn_gmd, ln_gmd, err_gmd) = LogGammaFn.withSign(-d)

    val sgn1 = sgn_gc * sgn_gd * sgn_g1ca * sgn_g1cb
    val sgn2 = sgn_gc * sgn_gmd * sgn_g2a * sgn_g2b

    val (flag, pre1_val, pre2_val, pre1_err, pre2_err) =
      if (ok1 && ok2) {
        val ln_pre1_val = ln_gc + ln_gd - ln_g1ca - ln_g1cb
        val ln_pre2_val = ln_gc + ln_gmd - ln_g2a - ln_g2b + d * Math.log(1.0 - x)
        val ln_pre1_err = err_gc + err_gd + err_g1ca + err_g1cb
        val ln_pre2_err = err_gc + err_gd + err_g2a + err_g2b
        val (p1, err1) = ExpErr(ln_pre1_val, ln_pre1_err)
        val (p2, err2) = ExpErr(ln_pre2_val, ln_pre2_err)
        (true, p1 * sgn1, p2 * sgn2, err1, err2)
      } else if (ok1 && !ok2) {
        val ln_pre1_val = ln_gc + ln_gd - ln_g1ca - ln_g1cb
        val ln_pre1_err = err_gc + err_gd + err_g1ca + err_g1cb
        val (p1, err1) = ExpErr(ln_pre1_val, ln_pre1_err)
        (true, p1 * sgn1, 0.0, err1, 0.0)
      } else if (!ok1 && ok2) {
        val ln_pre2_val = ln_gc + ln_gmd - ln_g2a - ln_g2b + d * Math.log(1.0 - x)
        val ln_pre2_err = err_gc + err_gd + err_g2a + err_g2b
        val (p2, err2) = ExpErr(ln_pre2_val, ln_pre2_err)
        (true, 0.0, p2 * sgn2, 0.0, err2)
      } else {
        (false, 0.0, 0.0, 0.0, 0.0)
      }
    if (!flag) {
      throw new IllegalArgumentException("Underflow error")
    }
    val F1 = hyperg_2F1_series(a, b, 1.0 - d, 1.0 - x)
    val F2 = hyperg_2F1_series(c - a, c - b, 1.0 + d, 1.0 - x)

    val result_val = pre1_val * F1._1 + pre2_val * F2._1
    val err1 = Math.abs(pre1_val * F1._2) + Math.abs(pre2_val * F2._2)
    val err2 = err1 + Math.abs(pre1_err * F1._1) + Math.abs(pre2_err * F2._1)
    val err3 = err2 + 2.0 * Constants.DBL_EPSILON * (Math.abs(pre1_val * F1._1) + Math.abs(pre2_val * F2._1))
    val err4 = err3 + 2.0 * Constants.DBL_EPSILON * Math.abs(result_val)

    (result_val, err4)
  }


  /**
    * copy of pow_omx from gsl_sf_hyperg_2F1_e line: 625
    *
    * @param x
    * @param p
    * @return
    */
  private def powOMX(x: Double, p: Double): (Double, Double) = {
    val omx = if (Math.abs(x) > Constants.ROOT5_DBL_EPSILON) {
      -x * (1.0 + x * (1.0 / 2.0 + x * (1.0 / 3.0 + x / 4.0 + x * x / 5.0)))
    } else Math.log(1.0 - x)
    val ln_result = p * omx
    val y = Math.exp(ln_result)
    val err = 2.0 * Constants.DBL_EPSILON * Math.abs(y)
    (y, err)
  }

  /**
    * a copy of the implementation from GSL
    * file hyperg_2F1.c
    * gsl_sf_hyperg_2F1_e line 643
    *
    * @param a
    * @param b
    * @param c
    * @param x
    */
  def hyperGauss2F(a: Double, b: Double, c: Double, x: Double) = {
    val d = c - a - b
    val rinta = Math.floor(a + 0.5)
    val rintb = Math.floor(b + 0.5)
    val rintc = Math.floor(c + 0.5)
    val isaneg = (a < 0.0 && Math.abs(a - rinta) < logEPS)
    val isbneg = (b < 0.0 && Math.abs(b - rintb) < logEPS)
    val iscneg = (c < 0.0 && Math.abs(c - rintc) < logEPS)

    if (Math.abs(x - 1.0) < logEPS && d > 0 && c != 0 && !iscneg) {
      val (sign1, y1, err1) = LogGammaFn.withSign(c)
      val (y2, err2) = LogGammaFn(d)
      val (sign3, y3, err3) = LogGammaFn.withSign(c - a)
      val (sign4, y4, err4) = LogGammaFn.withSign(c - b)
      val a1 = y1 + y2 - y3 - y4
      val result1 = Math.exp(a1)
      // error includes gsl_sf_exp_err_e calculation.
      val b1 = err1 + err2 + err3 + err4 + 2.0 * Constants.DBL_EPSILON * Math.abs(result1)

      (result1 * sign1 / (sign3 * sign4), b1)
    } else {
      /* If c is a negative integer, then either a or b must be a
            negative integer of smaller magnitude than c to ensure
            cancellation of the series. */
      if (iscneg) {
        if (!(isaneg && a > c + 0.1) && !(isbneg && b > c + 0.1)) {
          throw new IllegalArgumentException(
            """
              |If c is a negative integer, then either a or b must be a
              |            negative integer of smaller magnitude than c to ensure
              |            cancellation of the series.
            """.stripMargin)
        }
      }

      if (Math.abs(c - b) < logEPS || Math.abs(c - a) < logEPS) {
        powOMX(x, d)
      } else if (a >= 0.0 && b >= 0.0 && c >= 0.0 && x >= 0.0 && x < 0.995) {
        /**
          * hyper_2F1.c line 702
          * Series has all positive definite
          * terms and x is not close to 1.
          * hyperg_2F1_series(a, b, c, x, result)
          */
        hyperg_2F1_series(a, b, c, x)
      } else if (Math.abs(a) < 10.0 && Math.abs(b) < 10.0) {
        if (isaneg) {
          hyperg_2F1_series(rinta, b, c, x)
        } else if (isbneg) {
          hyperg_2F1_series(a, rintb, c, x)
        } else if (x < -0.25) {
          hyperg_2F1_luke(a, b, c, x)
        } else if (x < 0.5) {
          hyperg_2F1_series(a, b, c, x)
        } else if (Math.abs(c) > 10.0) {
          hyperg_2F1_series(a, b, c, x)
        } else {
          hyperg_2F1_reflect(a, b, c, x)
        }
      } else {
        /** Either a or b or both large.
          * Introduce some new variables ap,bp so that bp is
          * the larger in magnitude.
          */
        val (ap, bp) = if (Math.abs(a) > Math.abs(b)) {
          (b, a)
        } else {
          (a, b)
        }
        if (x < 0.0) {
          hyperg_2F1_luke(a, b, c, x)
        } else if (List(Math.abs(ap), 1.0).max * Math.abs(bp) * Math.abs(x) < 2.0 * Math.abs(c)) {
          hyperg_2F1_series(a, b, c, x)
        } else if (Math.abs(bp * bp * x * x) < 0.001 * Math.abs(bp) && Math.abs(ap) < 10.0) {
          // large b asymptote
          hyperg_2F1_reflect(a, b, c, x)
        } else {
          // error condition
          (0.0, 0.0)
        }
      }

    }
  }

}


object GaussHypergeometric {
  def apply(a: Double, b: Double, c: Double, x: Double) =
    new GaussHypergeometric().hyperGauss2F(a,b,c,x)
}
