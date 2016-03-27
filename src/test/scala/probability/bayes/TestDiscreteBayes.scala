package probability.bayes

import au.id.cxd.math.probability.bayes.DiscreteBayes
import au.id.cxd.math.probability.discrete.TableDistribution
import breeze.linalg.{DenseMatrix, DenseVector}
import org.scalatest._


trait TestData {
  /**
    * test the discrete bayes distribution
    *
    **/

  val data = DenseMatrix(
    (0d, 1d / 6d, 0d),
    (1d, 1d / 6d, 4d / 26d),
    (2d, 1d / 6d, 6d / 26d),
    (3d, 1d / 6d, 6d / 26d),
    (4d, 1d / 6d, 4d / 26d),
    (5d, 1d / 6d, 0d)
  )


  /**
    * the joint likelihood table
    * Its columns are
    *
    * [ Y, P(Y|X_0), P(Y|X_1), P(Y|X_2), P(Y|X_3), P(Y|X_4), P(Y|X_5) ]
    */
  val jointYData = DenseMatrix(
    (0d, 20d / 120d, 12d / 120d, 6d / 120d, 2d / 120d, 0d, 0d),
    (1d, 0d, 4d / 120d, 6d / 120d, 6d / 120d, 4d / 120d, 0d),
    (2d, 0d, 4d / 120d, 6d / 120d, 6d / 120d, 4d / 120d, 0d),
    (3d, 0d, 0d, 2d / 120d, 6d / 120d, 12d / 120d, 20d / 120d)
  )

  val prior = TableDistribution(
    table = data
  )

  val likelihoods = List(TableDistribution(
    table = jointYData,
    domainColumn = 0,
    rangeColumn = 1
  ),
    TableDistribution(
      table = jointYData,
      domainColumn = 0,
      rangeColumn = 2
    ),
    TableDistribution(
      table = jointYData,
      domainColumn = 0,
      rangeColumn = 3
    ),
    TableDistribution(
      table = jointYData,
      domainColumn = 0,
      rangeColumn = 4
    ),
    TableDistribution(
      table = jointYData,
      domainColumn = 0,
      rangeColumn = 5
    ),
    TableDistribution(
      table = jointYData,
      domainColumn = 0,
      rangeColumn = 6
    )
  )


  // calculate posterior P(X|Y)
  // for each value of X and Y
  val x = data(::, 0).toArray
  val y = jointYData(::, 1).toArray.distinct

  def computePosterior() = {

    val xRange = x.zip(likelihoods)

    // compute the posterior distribution
    // for P(X|Y)
    val xy = x.map {
      x1 => y.zip(likelihoods).map { y1 => (x1, y1._1, y1._2) }
    }.foldLeft(Seq[(Double, Double, TableDistribution)]()) {
      (accum, series) => accum ++ series.toSeq
    }
    xy.map {
      pair => {
        val dist = new DiscreteBayes(
          likelihood = pair._3,
          prior = prior
        )
        ((pair._1, pair._2), dist.posterior(pair._2, pair._1, xRange))
      }
    }

  }

  def tabulate(data: Seq[((Double, Double), Double)]) = {
    val matData = DenseMatrix.zeros[Double](data.length, 3)
    data.foldLeft((0, matData)) {
      (accum, row) => {
        val (x, y) = row._1
        val i = accum._1
        accum._2(i, 0 to 2) := DenseVector(x, y, row._2).t
        (accum._1 + 1, accum._2)
      }
    }
  }


}

/**
  * Created by cd on 27/03/2016.
  */
class TestDiscreteBayes extends FlatSpec with ShouldMatchers {

  "Discrete Bayes" should "find posterior" in new TestData {
    val posterior = computePosterior
    val table = tabulate(posterior)
    println(table)
  }

}
