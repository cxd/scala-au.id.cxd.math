package au.id.cxd.math.data


import breeze.linalg.DenseMatrix

/**
  * Created by cd on 10/05/2016.
  */
trait PredictorTargetData {

  /**
    * select the range of column indices that correspond to the dependent variables
    *
    * @return
    */
  def rangeOfDependentVariables(dataSet:DataSet, dependentColumnName:String):Array[Int] = {
    val keyMap = dataSet.discreteMapping
    val startIdx = dataSet.continuousCols
    val (totalCols, depCols) = keyMap.foldLeft (startIdx, Array[Int]()) {
      (add, key) => {
        val total = add._1 + key._2.toList.length
        val cols = key._1.equalsIgnoreCase(dependentColumnName) match {
          case true => (for (i <- add._1+1 to total) yield (i)).toArray
          case _ => add._2
        }
        (total, cols)
      }
    }
    depCols
  }

  /**
    * get the list of classes for the dependent column name.
    * @param dataSet
    * @param dependentColumnName
    * @return
    */
  def classesForDependentVariable(dataSet:DataSet, dependentColumnName:String) = {
    val keyMap = dataSet.discreteMapping
    keyMap.foldLeft (Array[String]()) {
      (accum, key) => {
        key._1.equalsIgnoreCase(dependentColumnName) match {
          case true => accum ++ key._2.toList
          case _ => accum
        }
      }
    }
  }


  /**
    * extract the dependent variables from the data set given the set of column numbers corresponding to
    * each of the dependent variables. The number of dependent variables depends on the number
    * of unique values in the target column.
    *
    * @param data
    * @return
    */
  def extractDependentVariables(data:DenseMatrix[Double], dependentVariables:Array[Int]) = {
    // select the dependent variables
    val dependents = data(::, for (i <- 0 until dependentVariables.length) yield dependentVariables(i))
    // select the predictors
    val range = for (i <- 0 until data.cols if !dependentVariables.contains(i)) yield i
    val predictors = data(::, range)
    (predictors.toDenseMatrix, dependents.toDenseMatrix)
  }

}