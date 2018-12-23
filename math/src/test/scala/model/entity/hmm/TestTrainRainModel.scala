package model.entity.hmm

import au.id.cxd.math.data.{SequenceEstimation, SequenceReader}
import au.id.cxd.math.model.entity.hmm.InputModel
import au.id.cxd.math.model.sequence.HiddenMarkovModel
import org.scalatest._

/**
 * Created by cd on 31/01/15.
 */
class TestTrainRainModel  extends FlatSpec with Matchers {

  val data = TestRainExample.data

  "HMM Model" should "train for rain" in {
    val reader = SequenceReader()
    val states = reader.readStates (data)
    val evidenceVars = reader.readEvidenceVars (data)
    val estimator = SequenceEstimation()
    val test = estimator.countStateFreq(data)(states)
    val pi = estimator.statePriors(data)(states)
    val T = estimator.countTransitions(data)(states)
    val A = estimator.stateTransitions(data)(states)
    val Bk = estimator.avgPriorEvidences(data)(evidenceVars)(states)

    val input = InputModel(pi, A, List(Bk), states, evidenceVars)
    val model = HiddenMarkovModel.train(input)(data)(0.00001)(50)

  }
}
