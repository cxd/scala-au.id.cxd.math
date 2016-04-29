import au.id.cxd.math.probability.bayes.DiscreteBayes
import au.id.cxd.math.probability.discrete.TableDistribution
import breeze.linalg.DenseMatrix

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
  * [ X, Y, P(Y|X) ]
  */
val jointYData = DenseMatrix(
  (0d, 0d, 20d/120d),
  (0d, 1d, 0d),
  (0d, 2d, 0d),
  (0d, 3d, 0d),
  (1d, 0d, 12d/120d),
  (1d, 1d, 4d/120d),
  (1d, 2d, 4d/120d),
  (1d, 3d, 0d),
  (2d, 0d, 6d/120d),
  (2d, 1d, 6d/120d),
  (2d, 2d, 6d/120d),
  (2d, 3d, 2d/120d),
  (3d, 0d, 2d/120d),
  (3d, 1d, 6d/120d),
  (3d, 2d, 6d/120d),
  (3d, 3d, 6d/120d),
  (4d, 0d, 0d),
  (4d, 1d, 4d/120d),
  (4d, 2d, 4d/120d),
  (4d, 3d, 12d/120d),
  (5d, 0d, 0d),
  (5d, 1d, 0d),
  (5d, 2d, 0d),
  (5d, 3d, 20d/120d)
)

val prior = TableDistribution(
  table = data
)

val likelihood = TableDistribution(
  table = jointYData,
  domainColumn = 1,
  rangeColumn = 2
)

val dist = new DiscreteBayes(
  likelihood = likelihood,
  prior = prior
)
// calculate posterior P(X|Y)
// for each value of X and Y
val x = data(::,0).toArray
val y = jointYData(::,1).toArray.distinct

val xRange = x.map { x1 => (x1, likelihood) }

// compute the posterior distribution
// for P(X|Y)
val xy = x.map {
  x1 => y.map { y1 => (x1, y1) }
}.foldLeft(Seq[(Double,Double)]()) {
  (accum, series) => accum ++ series.toSeq
}
xy.map {
  pair => (pair, dist.posterior(pair._2, pair._1, xRange))
}

