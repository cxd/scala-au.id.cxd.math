package au.id.cxd.math.data

import breeze.linalg.DenseMatrix

import scala.collection.immutable

/**
  * group partitioning trait takes
  * a set of group labels for each observation and
  * then generates group partitions for a supplied input matrix
  * where the ith row of the input matrix corresponds with the ith group label.
  *
  * It returns an ordered set of group partitions which contains the subset of observations for each group
  *
  * type GroupName = String
  *
  * List [ (GroupName, DenseMatrix[Double] )]
  *
  * Where the set of tuples are ordered by the group name in the first item of the tuple
  */
trait GroupPartition {

  /**
    * the group name for each observation in the dataset that is to be partitioned.
    */
  val groupLabels: List[String]

  val data:DenseMatrix[Double]

  lazy val groups = groupIndexes(groupLabels)

  lazy val partitions : List[(String, DenseMatrix[Double])] = partitionGroups(data)(groups)

  /**
    * identify the group indexes.
    *
    * @param groupNames
    */
  def groupIndexes(groupNames: List[String]): Map[String, Vector[Int]] =
    groupNames.foldLeft((0, Map[String, Vector[Int]]())) {
      (accum, groupName) => {
        val i = accum._1
        val groups = accum._2
        (i + 1, groups.contains(groupName.toLowerCase) match {
          case true => {
            val indexes = groups(groupName.toLowerCase) :+ i
            (groups - (groupName.toLowerCase)) + (groupName.toLowerCase -> indexes)
          }
          case _ => (groups + (groupName.toLowerCase -> Vector(i)))
        })
      }
    }._2


  /**
    * given the group names for each row
    * determine the size of each group and the approximate proportion of each group.
    *
    * @param groupNames
    * @return
    * (groupname x n_i x \pi_i)
    *
    * where n_i is the size of the group
    * and \pi_i is the proportion of the training data for the group.
    */
  def groupSizes(groupNames: List[String]): immutable.Iterable[(String, Int, Double)] = {
    val totalSize = groupNames.length

    groups.map {
      pair => {
        val groupName = pair._1
        val ni = pair._2.length
        (groupName, ni, ni.toDouble / totalSize.toDouble)
      }
    }
  }

  /**
    * extract the partitions for each of the groups.
    *
    * @param m
    * @param groups
    * @return
    */
  def partitionGroups(m: DenseMatrix[Double])(groups: Map[String, Vector[Int]]): List[(String, DenseMatrix[Double])] = {
    val keys = groups.keys.toIndexedSeq.sorted
    keys.foldLeft(List[(String, DenseMatrix[Double])]()) {
      (accum, key) => {
        val idx = groups(key)
        val submat = m(idx, ::).toDenseMatrix
        accum :+ (key, submat)
      }
    }
  }
}
