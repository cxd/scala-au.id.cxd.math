package au.id.cxd.math.model.entity.hmm

import au.id.cxd.math.data.Readable
import au.id.cxd.math.data.Writable
import breeze.linalg.DenseMatrix

/**
 * The model that stores data from a trained HMM
 * Created by cd on 20/01/15.
 */
@scala.SerialVersionUID(1L)
class Model(val pi:List[Double], val A:DenseMatrix[Double], val B:DenseMatrix[Double], val states:List[String], val evidence:List[String], val epoch:Int, val error:Double)
  extends Serializable {

}

object Model extends Readable[Model] with Writable[Model] {

  def apply(pi:List[Double], A:DenseMatrix[Double], B:DenseMatrix[Double], states:List[String], evidence:List[String], epoch:Int, error:Double) = {
    new Model(pi, A, B, states, evidence, epoch, error)
  }
}
