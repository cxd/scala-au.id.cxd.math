package au.id.cxd.math.probability.analysis

import au.id.cxd.math.probability.Distribution

/**
  * ##import MathJax
  *
  * Created by cd on 26/03/2016.
  */
class Odds(val dist:Distribution) {

  /**
    * compute the odds for $P(Y)$ over $1 - P(Y)$
    *
    * $$
    *  \frac{P(Y)}{1 - P(Y)}
    * $$
    *
    * @param y
    * @return
    */
  def odds (y:Double) = dist.pdf(y) / (1.0 - dist.pdf(y))

  /**
    * compute the log odds
    *
    * $$
    *   \log \left[ \frac{P(Y)}{1 - P(Y)} \right]
    * $$
    *
    * @param y
    * @return
    */
  def logOdds (y:Double) = Math.log ( odds(y) )

}
