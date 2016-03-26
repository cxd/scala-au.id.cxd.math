package au.id.cxd.math.probability.discrete

import breeze.linalg.{DenseMatrix, Matrix}

/**
  * A table distribution represents
  * a discrete collection of values which correspond to a discrete distribution.
  *
  * The table must have at least two columns.
  *
  * The domainColumn contains values of Y that map to the row associated with f(y).
  *
  * The rangeColumn is the column contains the P(Y) values, all values of this column must sum to 1.
  *
  *
  * Created by cd on 26/03/2016.
  */
class TableDistribution (val table:DenseMatrix[Double], val domainColumn:Int = 0, val rangeColumn:Int = 1)
  extends DiscreteDistribution {

  /**
    * lookup the row for the value y
    * the assumption is that Y exists in the table.
    * @param y
    * @return
    */
  def rowFor(y:Double):Int = {
    val result = table(::,domainColumn).activeIterator.find {
      (set:(Int,Double)) => set._2 == y
    }
    result.map {
      res:(Int,Double) => res._1
    }.getOrElse(-1)
  }

  /**
    * lookup the p-value corresponding to the value of Y.
    * @param y
    * @return
    */
  def pdf(y: Double): Double = {
    val row = rowFor(y)
    table(row, rangeColumn)
  }

  /**
    * approximate empircal mean of the values in Y
    * @return
    */
  def mean(): Double =
    1.0/table.rows * table(::,domainColumn).toArray.sum

  /**
    * the approximate standard deviation
    * @return
    */
  def variance(): Double = {
    val mu = mean()
    val totalSS = table(::,domainColumn).toArray.map { v => Math.pow(v - mu, 2.0) }.sum
    1.0 / (table.rows - 1) * totalSS
  }

}

object TableDistribution {
  def apply(table:DenseMatrix[Double], domainColumn:Int = 0, rangeColumn:Int = 1) = new TableDistribution(table, domainColumn, rangeColumn)
}
