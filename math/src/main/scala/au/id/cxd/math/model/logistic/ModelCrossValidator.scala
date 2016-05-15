package au.id.cxd.math.model.logistic

import java.io.File

import au.id.cxd.math.data.{DataSet, PredictorTargetData}
import au.id.cxd.math.model.ModelSerializer
import au.id.cxd.math.model.classification.ModelAssessment
import au.id.cxd.math.probability.regression.LogisticLeastSquares
import breeze.linalg.DenseMatrix

import scala.reflect.io.Directory

/**
  * The model cross validator will load a list of logistic regression models that
  * have been named with the same prefix and will calculate metrics for the set of models.
  *
  * It also requires the data file that contains the cross validation data.
  *
  * Created by cd on 10/05/2016.
  */
class ModelCrossValidator(val dataFile: File, val dependentColumnName: String, val basePath: String, val modelPrefix: String)
  extends PredictorTargetData {

  /**
    * load the data set.
    */
  val dataSet: DataSet = ModelSerializer.deserialize(dataFile)

  /**
    * the column indices of the dependent variables.
    */
  val dependentVariables: Array[Int] = rangeOfDependentVariables(dataSet, dependentColumnName)

  /**
    * the name of the classes associated with the dependent columns
    */
  val classNames:Array[String] = classesForDependentVariable (dataSet, dependentColumnName)

  def crossValidationData() = {
    val cvData = dataSet.crossValidateData
    extractDependentVariables(cvData, dependentVariables)
  }

  /**
    * process the model directory path and generate the set of files
    *
    * @param path
    * @param visitFn
    * @return
    */
  def process(path: String)(visitFn: (Int , scala.reflect.io.File) => ModelAssessment):List[ModelAssessment] = {
    val file = Directory(path)
    val modelFiles = file.files filter {
      f => f.name.startsWith(modelPrefix)
    }
    modelFiles.foldLeft ((0, List[ModelAssessment]())) {
      (pair, file) => (pair._1+1, pair._2 :+ visitFn (pair._1, file))
    }._2
  }

  /**
    * calculate the deviance for the positive class G = 1
    * L(G, \hat{p}(X)) = -2 \log {\hat{p}(X) } = -2 * log-likelihood
    * @param likelihoodY
    * @return
    */
  def computeLogLikelihoodDeviance(likelihoodY:DenseMatrix[Double]):Double = {
    // the likelihood of class == class 1
    val g1 = likelihoodY(::, 1).toArray
    val total = g1.map { g => Math.log (g) }.reduce { (a, b) => a + b }
    -2.0 * total
  }

  /**
    * count the number of true positives
    * @param target
    * @param y
    * @return
    */
  def countTruePositives(target:DenseMatrix[Double], y:DenseMatrix[Double]) = {
    val y1 = y.toDenseVector
    target.toDenseVector.toArray.foldLeft ((0, 0)) {
      (pair, t) => {
        val i = pair._1
        val flag = y1(i)
        flag == 1 && flag == t match {
          case true => (i+1, pair._2 + 1)
          case _ => (i+1, pair._2)
        }
      }
    }._2
  }

  /**
    * count the number of false positives
    * @param target
    * @param y
    * @return
    */
  def countFalsePositives(target:DenseMatrix[Double], y:DenseMatrix[Double]) = {
    val y1 = y.toDenseVector
    target.toDenseVector.toArray.foldLeft ((0, 0)) {
      (pair, t) => {
        val i = pair._1
        val flag = y1(i)
        flag == 1 && flag != t match {
          case true => (i+1, pair._2 + 1)
          case _ => (i+1, pair._2)
        }
      }
    }._2
  }

  /**
    * count the number of true negatives
    * @param target
    * @param y
    * @return
    */
  def countTrueNegatives(target:DenseMatrix[Double], y:DenseMatrix[Double]) = {
    val y1 = y.toDenseVector
    target.toDenseVector.toArray.foldLeft ((0, 0)) {
      (pair, t) => {
        val i = pair._1
        val flag = y1(i)
        flag == 0 && flag == t match {
          case true => (i+1, pair._2 + 1)
          case _ => (i+1, pair._2)
        }
      }
    }._2
  }

  /**
    * count the false negatives
    * @param target
    * @param y
    * @return
    */
  def countFalseNegatives(target:DenseMatrix[Double], y:DenseMatrix[Double]) = {
    val y1 = y.toDenseVector
    target.toDenseVector.toArray.foldLeft ((0, 0)) {
      (pair, t) => {
        val i = pair._1
        val flag = y1(i)
        flag == 0 && flag != t match {
          case true => (i+1, pair._2 + 1)
          case _ => (i+1, pair._2)
        }
      }
    }._2
  }

  /**
    * count the total positives
    * @param target
    * @return
    */
  def countPopulationPositives(target:DenseMatrix[Double]) = {
    val total = target.toDenseVector.toArray.sum
    total
  }

  /**
    * count total negatives
    * @param target
    * @return
    */
  def countPopulationNegatives(target:DenseMatrix[Double]) = {
    val total = countPopulationPositives(target)
    target.toDenseVector.length - total
  }

  def crossValidate(className:String, model: LogisticLeastSquares, predictors: DenseMatrix[Double], target: DenseMatrix[Double]) = {
    // the output of the estimate gives
    // y = logistic output for y
    val (y, likelihoodY) = model.estimate(predictors)

    val classes = likelihoodY(::,2).toDenseMatrix
    // assess the model using the class column
    // calculate the MSE.
    val mse = model.squaredError(target, classes)
    val deviance = computeLogLikelihoodDeviance (likelihoodY)
    //
    val sigmaSqr = model.variance

    // tp, fp, tn, fn
    val tp = countTruePositives(target, classes)
    val tn = countTrueNegatives(target, classes)
    val fp = countFalsePositives(target, classes)
    val fn = countFalseNegatives(target, classes)

    val pos = countPopulationPositives(target)
    val neg = countPopulationNegatives(target)
    // false positive rate
    val fpr = fp * 1.0 / neg*1.0
    // false negative rate
    val fnr = fn*1.0 / pos*1.0
    // true positive rate
    val tpr = tp*1.0 / pos*1.0
    // true negative rate
    val tnr = tn*1.0 / neg*1.0
    // positive likelihood ratio
    val lrPlus = tpr / fpr
    // negative likelihood ratio
    val lrNeg = fnr / tnr
    // diagnostic odds ratio
    val dor = lrPlus / lrNeg

    // accuracy
    val accuracy = (tp + tn)*1.0 / (target.toDenseVector.length*1.0)

    ModelAssessment(className, mse, deviance, model.Beta.t, accuracy, dor, tp, fp, tn, fn, pos.toInt, neg.toInt)
  }

  /**
    * validate the model and return the set of model assessments and the beta matrix.
    * @return
    */
  def validateModel() = {
    val (predictors, target) = crossValidationData()
    val results = process (basePath) {
      (col, file) => {
        val logitModel:LogisticLeastSquares = ModelSerializer.deserialize(file.jfile)
        val targetCol = target(::,col)
        val className = classNames(col)
        // get the target name of the column
        crossValidate (className, logitModel, predictors, targetCol.toDenseMatrix)
      }
    }
    val cols = results.length
    val rows = predictors.cols
    // the first column of the beta matrix is the bias, hence we need to add 1 to the rows
    // the beta parameter is a vertical for each column (one class per column)
    // note this also assumes there has been no transformation of the data
    // within the logistic regression model itself.
    val betaMatrix = results.foldLeft ( 0, DenseMatrix.zeros[Double](rows+1, cols) ) {
      (pair, result) => {
        val col = pair._1
        val M = pair._2
        // the first column of the beta matrix will be the bias
        M(::, col) := result.beta(::,0)
        (pair._1 + 1, M)
      }
    }
    (results, betaMatrix._2)
  }

}

object ModelCrossValidator {
  def apply(dataFile: File, dependentColumnName: String, basePath: String, modelPrefix: String) = {
    new ModelCrossValidator(dataFile, dependentColumnName, basePath, modelPrefix)
  }
}
