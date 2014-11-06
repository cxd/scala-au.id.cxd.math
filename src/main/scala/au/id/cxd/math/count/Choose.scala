package au.id.cxd.math.count

/**
 * A choose operation $ C_k^n $
 * calculated as $\frac{n!}/{k!(n-k)!}$
 *
 * Created by cd on 6/09/2014.
 */
class Choose {
    def op(n:Double)(c:Double):Double = {
      if (c <= n) {
        Factorial(n) / (Factorial(c)*Factorial(n-c))
      } else 0.0
    }
}
object Choose {
  def apply(n:Double)(c:Double) = new Choose().op(Math.round(n))(Math.round(c))
}
