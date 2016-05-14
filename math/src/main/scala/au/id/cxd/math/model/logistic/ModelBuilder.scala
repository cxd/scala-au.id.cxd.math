package au.id.cxd.math.model.logistic

import au.id.cxd.math.data.{DataSet, PredictorTargetData}
import au.id.cxd.math.probability.regression.LogisticLeastSquares


/**
  * Generate the model using the linear regression algorithm for the supplied dependent variable columns
  *
  *
  *
  * Created by cd on 3/05/2016.
  */
class ModelBuilder(val dataSet:DataSet, dependentColumnName:String) extends PredictorTargetData {

  /**
    * the column indices of the dependent variables.
    */
  val dependentVariables:Array[Int] = rangeOfDependentVariables(dataSet, dependentColumnName)


  /**
    * training the model will result in the same number of models as there are dependent variables.
    * Each model predicts a single variable, $P(C_k|X)$ the collection of models is capable of
    * ranking the classification in order of probability of the target variable.
    *
    * @param degree - specify the degree of the polynomial
    *
    */
  def trainModels(degree:Int)(blockFn:(Int, LogisticLeastSquares) => Unit) = {
    val (predictors, dependents) = extractDependentVariables(dataSet.trainData, dependentVariables)
    val range = for (i <- 0 until dependents.cols) yield i
    range.foreach {
      col => {
        val target = dependents(::, col)
        val logitClassifier = LogisticLeastSquares(predictors, target, degree)
        val (est1, error1) = logitClassifier.train()
        blockFn (col, logitClassifier)
      }
    }
  }
}

object ModelBuilder {
  def apply(dataSet:DataSet, dependentColumnName:String) = new ModelBuilder(dataSet, dependentColumnName)
}
