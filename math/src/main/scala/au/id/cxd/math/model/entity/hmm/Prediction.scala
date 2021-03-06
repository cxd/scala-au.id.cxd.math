package au.id.cxd.math.model.entity.hmm

import au.id.cxd.math.data.{Writable, Readable}

/**
 * a class used to store results from predictions.
 * Created by cd on 20/01/15.
 */
class Prediction(val prob:Double, val state:String, val evidence:String, val t:Int, val success:Boolean)
  extends Serializable {

  /**
   * print the prediction
   * @return
   */
  override def toString() = {
    s"{\n\tprob: $prob,\n\tstate: $state\n\tevidence: $evidence\n\tT: $t\n\tsuccess: $success\n}"
  }

}
object Prediction extends Readable[Prediction] with Writable[Prediction] {
  def apply(prob:Double, state:String, evidence:String, t:Int, success:Boolean) = {
    new Prediction(prob, state, evidence, t, success)
  }
}
