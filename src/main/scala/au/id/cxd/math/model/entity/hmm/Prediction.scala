package au.id.cxd.math.model.entity.hmm

/**
 * a class used to store results from predictions.
 * Created by cd on 20/01/15.
 */
class Prediction(val prob:Double, val state:String, val evidence:String, val t:Int, val success:Boolean) {

  /**
   * print the prediction
   * @return
   */
  override def toString() = {
    s"Prob: $prob State: $state Evidence: $evidence T: $t Success: $success"
  }

}
object Prediction {
  def apply(prob:Double, state:String, evidence:String, t:Int, success:Boolean) = {
    new Prediction(prob, state, evidence, t, success)
  }
}
