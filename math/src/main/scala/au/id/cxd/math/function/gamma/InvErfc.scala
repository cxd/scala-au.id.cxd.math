package au.id.cxd.math.function.gamma

/**
  * #import MathJax
  *
  * This class provides an implementation of the inverse erfc function.
  *
  * There were two sources of information where I could find details on the inverse error function.
  *
  * The first from the Numerical Recipes, which prohibits reuse of their algorithms,
  * hence the implementation will not be leverage this approach in the long term.
  * It does serve as the starting reference point with a variety of sources being cited from that resource on the implementation.
  *
  * There are a number of other references describing the inverse erf function.
  * An implementation that reads in a fairly straight forward manner is available from the compute-io library:
  * https://github.com/compute-io/erfcinv/blob/master/lib/number.js
  *
  * This appears to be an interpretation of the boost implementation, which is more involved:
  * https://github.com/boostorg/math/blob/develop/include/boost/math/special_functions/erf.hpp
  * http://www.boost.org/doc/libs/1_66_0/libs/math/doc/html/math_toolkit/sf_erf/error_inv.html
  *
  * The topic of the inverse erf is discussed broadly in a number of resources.
  * http://octave.1599824.n4.nabble.com/New-implementations-of-qfunc-qfuncinv-erfcinv-td4649546.html
  * https://lists.gnu.org/archive/html/help-octave/2013-02/msg00173.html
  *
  * There is a spectacular thesis that provides quite indepth discussions of a variety of different numerical methods
  * by an author Thomas Luu called "Fast and accurate parallel computation of quantile functions for random number generation".
  * It has a broad survey of a number of methods and is quite a good source of additional information.
  *
  * http://discovery.ucl.ac.uk/1482128/1/Luu_thesis.pdf
  *
  * Similarly the paper "Approximating the erfinv function" by Mike Giles is also cited in the resources that I've found so far.
  * https://people.maths.ox.ac.uk/gilesm/files/gems_erfinv.pdf
  *
  * In relation to the Giles article the tensor flow library references this directly.
  * https://github.com/tensorflow/tensorflow/blob/d27f758308d475ebd76cdfec85dbca12a32cfc3f/tensorflow/compiler/xla/service/elemental_ir_emitter.cc#L318
  *
  * The implementation is based upon this approximation.
  *
  * Both of the latter will be of use in determining the methodology available for the implementation.
  */
class InvErfc {

  /**
    * an example of the method as described in numeric recipes, this method is unused but provided for reference.
    *
    * @param p
    * @return
    */
  def numeric_recipes_inverfc(p: Double): Double = {
    if (p >= 2.0) -100.0
    else if (p <= 0.0) 100.0
    else {
      val pp = if (p < 1.0) p
      else 2.0 - p
      val t = Math.sqrt(-2.0 * Math.log(pp / 2.0))
      val x = -0.70711 * ((2.30753 + t * 0.27601) / (1.0 + t * (0.99229 + t * 0.04481)) - 1)
      val result = (for (j <- 0 to 2) yield j).foldLeft(x) {
        (x1, j) => {
          val err = Erfc(x) - pp
          x1 + err / (1.12837916709551257 * Math.exp(-Math.sqrt(x)) - x * err)
        }
      }
      if (p < 1.0) result
      else -result
    }
  }



  def inverfc(p: Double): Double = InvErf(1.0 - p)

}

object InvErfc {
  def apply(p: Double): Double = new InvErfc().inverfc(p)
}