package au.id.cxd.math.model.cluster

import au.id.cxd.math.data.{Writable, Readable}
import au.id.cxd.math.data.filter.Which
import au.id.cxd.math.function.Constants
import au.id.cxd.math.function.column.ColMeans
import au.id.cxd.math.function.distance.EuclideanDistance
import au.id.cxd.math.probability.random.RUniform
import au.id.cxd.text.model.LatentSemanticIndex
import breeze.linalg.{DenseMatrix, DenseVector, argmax, argmin, max}

import scala.annotation.tailrec
import scala.collection.mutable


/**
  * clustering result
  * @param indices
  * The indices of each cluster, for each row of the original data.
  * @param centroids
  * Centroid vectors for each cluster.
  * @param groups
  * Mapping of data row index and subset of data grouped by each cluster.
  */
case class KMeansResult(indices:Seq[Int], centroids:Seq[DenseVector[Double]], groups:Seq[(Seq[Int], DenseMatrix[Double])]) {}
/**
  * k means clustering.
 *
  * @param k
  * number of clusters
  * @param centroids
  * potential starting centroids can be supplied.
  */
class KMeans(k:Int=2, initCentroids:Seq[DenseVector[Double]] = Seq(), distThreshold:Double= Constants.ROOT4_DBL_EPSILON, maxIter:Option[Double] = None) {


  /**
    * select centroids at random for initialisation.
    * @param k
    * @param data
    * @return
    */
  def randomCentroids(k:Int, data:DenseMatrix[Double]): DenseMatrix[Double] = {
    val sampler = RUniform(0, data.rows)

    def sample() = {
      val series = mutable.Buffer[Int]()
      do {
        val n = sampler.draw().toInt
        if (!series.contains(n)) {
          series.append(n)
        }
      } while(series.length < k)
      series
    }
    val idx = sample()
    data(idx.toSeq,::).toDenseMatrix
  }

  /**
    * calculate the rows x k distance matrix
    * @param data
    * @param centroids
    */
  def rowDistance(data:DenseMatrix[Double], centroids:DenseMatrix[Double]): DenseMatrix[Double] = {
    val distMat = DenseMatrix.zeros[Double](data.rows, centroids.rows)
    distMat.mapPairs((i,j) => {
      EuclideanDistance(data(i._1,::).inner, centroids(i._2,::).inner)
    })
  }

  /**
    * assign each row in the matrix to the minimum centroid.
    * @param data
    * @param centroids
    * @return
    */
  def assignClusters(data:DenseMatrix[Double], centroids:DenseMatrix[Double]):(Seq[Int], DenseMatrix[Double]) = {
    val dist = rowDistance(data, centroids)
    val minIdx = (0 until data.rows).map {
      i =>
        val row = dist(i,::)
        argmin(row)
    }
    (minIdx, dist)
  }

  def groupByCluster(data:DenseMatrix[Double], minIdx:Seq[Int]): Seq[(Seq[Int], DenseMatrix[Double])] = {
    val keys = minIdx.distinct.sorted
    val groups = keys.map {
      c =>
        // the which function requires some optimisation.
        val idx = Which(minIdx, (i:Int) => i == c)
        val slice = data(idx,::)
        (idx, slice.toDenseMatrix)
    }
    groups
  }

  /**
    * calculate the means of each row group assigned to their respective clusters.
    * @param data
    * @param minIdx
    * @return
    */
  def calculateCentroids(data:DenseMatrix[Double], minIdx:Seq[Int]): Seq[DenseVector[Double]] = {
    val groups = groupByCluster(data, minIdx).map(_._2)
    val centroids = groups.map {
      g => ColMeans(g).toDenseVector
    }
    centroids
  }

  /**
    * convert centroids matrix to a vector list
    * @param centroids
    * @return
    */
  def centroidsToVector(centroids:DenseMatrix[Double]): Seq[DenseVector[Double]] = {
    (0 until centroids.rows).map {
      i =>
        val row = centroids(i,::).inner
        row
    }
  }

  /**
    * given the sequence of centroids convert to a matrix for use in calculations.
    * @param centroids
    * @return
    */
  def centroidsToMatrix(centroids:Seq[DenseVector[Double]]): DenseMatrix[Double] = {
    @tailrec
    def concat(a:DenseMatrix[Double], ss:Seq[DenseVector[Double]]):DenseMatrix[Double] = {
      if (ss.isEmpty) {
        a
      } else {
        val mat = DenseMatrix.vertcat(a, ss.head.toDenseMatrix)
        concat(mat, ss.tail)
      }
    }

    val a = centroids.head.toDenseMatrix
    val b = centroids.tail
    concat(a, b)
  }

  /**
    * run the kmeans clustering algorithm on the supplied data.
    * @param data
    * @return
    * a kmeans cluster result.
    * clusterIndices - list of cluster assignments for the closest matching centroid.
    * centroids - list of vectors representing the centroid of each cluster.
    * groups - list of partitions of the supplied data assigning each partition to the associated cluster.
    */
  def cluster(data:DenseMatrix[Double]): KMeansResult = {
    var flag:Boolean = false
    // initial bootstrap.
    var centroids = if (initCentroids.size == k) {
      centroidsToMatrix(initCentroids)
    } else {
      randomCentroids(k, data)
    }
    var clusterIndices:Seq[Int] = Seq()
    var i = 0
    do {
      val (idx, minDist) = assignClusters(data, centroids)
      val newCentroids = calculateCentroids(data, idx)
      val updateCentroids = centroidsToMatrix(newCentroids)
      val (newIdx, newDist) = assignClusters(data, updateCentroids)
      // we can optionally test the maximum distance of the distance matrix and see if it falls below a threshold.
      val delta = idx.zip(newIdx).map { pair => math.abs(pair._1 - pair._2) }.sum
      // if there are any differences the new assignments will be > 0
      val maxDist = max(newDist)
      centroids = updateCentroids
      clusterIndices = newIdx
      // exit condition if the maximum distance from the
      flag = (maxDist <= distThreshold || delta == 0)
      val maxIterFlag = maxIter.map { m => i >= m }
      if (maxIterFlag.getOrElse(false) == true) {
        flag = true
      }
      i += 1
    } while(!flag)
    val groups = groupByCluster(data, clusterIndices)
    val centroidList = centroidsToVector(centroids)
    KMeansResult(clusterIndices, centroidList, groups)
  }

}

object KMeans {

  /**
    * initialise the algorithm.
    * @param k
    * @param initCentroids
    * @param distThreshold
    * @return
    */
  def apply(k:Int=2, initCentroids:Seq[DenseVector[Double]] = Seq(), distThreshold:Double= Constants.DBL_EPSILON): KMeans = {
    new KMeans(k = k, initCentroids = initCentroids, distThreshold = distThreshold)
  }

  /**
    * binary serialise the KMeans result.
    * @param file
    * @param result
    */
  def writeBinary(path:String, result:KMeansResult): Option[Boolean] = {
    val writer = new Writable[KMeansResult] {}
    writer.write(path)(result)
  }

  /**
    * read file from binary.
    * @param filePath
    * @return
    */
  def readResults(filePath:String): Option[KMeansResult] = {
    val reader = new Readable[KMeansResult] {}
    reader.read(filePath)
  }
}