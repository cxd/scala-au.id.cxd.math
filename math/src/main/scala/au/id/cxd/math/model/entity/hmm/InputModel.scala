package au.id.cxd.math.model.entity.hmm

import au.id.cxd.math.data.{Writable, Readable}
import breeze.linalg.DenseMatrix

/**
 * an input model used to train a HMM
 * Created by cd on 20/01/15.
 */
class InputModel(val pi:List[Double], val A:DenseMatrix[Double], val Bk:List[DenseMatrix[Double]], val states:List[String], val evidence:List[String])
  extends Serializable {

}

object InputModel extends Readable[InputModel] with Writable[InputModel] {
  def apply(pi:List[Double], A:DenseMatrix[Double], Bk:List[DenseMatrix[Double]], states:List[String], evidence:List[String]) = {
    new InputModel(pi, A, Bk, states, evidence)
  }
}
