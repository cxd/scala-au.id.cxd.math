package au.id.cxd.math.count

/**
 * Permutation operation (select without replacement) $P_r^n$
 *
 * $\frac{n!}{(n-k)!}$
 *
 * Created by cd on 6/09/2014.
 */
class Permutation {
    def op(n:Double)(r:Double) = {
      if (r <= n) {
        Factorial(n) / Factorial(n-r)
      } else 0.0
    }
}

object Permutation {
  def apply(n:Double)(r:Double) = new Permutation().op(n)(r)
}
