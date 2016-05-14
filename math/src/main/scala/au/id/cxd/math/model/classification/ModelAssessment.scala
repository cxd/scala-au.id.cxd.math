package au.id.cxd.math.model.classification

import breeze.linalg.DenseMatrix

/**
  * A data object to contain the outputs of Model assessment for
  * classification
  * Created by cd on 10/05/2016.
  */
class ModelAssessment(val className:String,
                      val mse: Double,
                      val loglikelihood: Double,
                      val beta:DenseMatrix[Double],
                      val accuracy:Double,
                      val oddsRatio:Double,
                      val truePositive:Int,
                      val falsePositive:Int,
                      val trueNegative:Int,
                      val falseNegative:Int,
                      val dataPositive:Int,
                      val dataNegative:Int) extends Serializable {

}

object ModelAssessment {
  def apply(className:String,
            mse: Double,
            loglikelihoodDeviance: Double,
            beta:DenseMatrix[Double],
            accuracy:Double,
            oddsRatio:Double,
            tp:Int,
            fp:Int,
            tn:Int,
            fn:Int,
            dp:Int,
            dn:Int) =
    new ModelAssessment(className, mse, loglikelihoodDeviance, beta, accuracy, oddsRatio, tp, fp, tn, fn, dp, dn)
}
