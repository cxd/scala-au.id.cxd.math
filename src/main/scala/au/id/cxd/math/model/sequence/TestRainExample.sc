import au.id.cxd.math.data.{SequenceReader, SequenceEstimation}
import au.id.cxd.math.model.entity.hmm.InputModel
import au.id.cxd.math.model.sequence.HiddenMarkovModel

val data =
  List(
    List("noumbrella", "dry"),
    List("noumbrella", "noumbrella", "dry"),
    List("noumbrella", "noumbrella", "noumbrella", "dry"),
    List("noumbrella", "umbrella", "noumbrella", "dry"),
    List("noumbrella", "umbrella", "noumbrella", "noumbrella", "dry"),
    List("noumbrella", "noumbrella", "noumbrella", "dry"),

    List("noumbrella", "umbrella", "rain"),
    List("noumbrella", "umbrella", "noumbrella", "dry"),
    List("noumbrella", "umbrella", "noumbrella", "noumbrella", "dry"),
    List("noumbrella", "umbrella", "noumbrella", "noumbrella", "umbrella", "rain"),
    List("noumbrella", "umbrella", "noumbrella", "umbrella", "rain"),

    List("umbrella", "rain"),
    List("umbrella", "umbrella", "rain"),
    List("umbrella", "umbrella", "umbrella", "rain"),


    List("noumbrella", "absent", "storm"),
    List("umbrella", "absent", "storm"),
    List("absent", "absent", "absent", "storm"),

    List("noumbrella", "absent", "umbrella", "rain"),
    List("noumbrella", "absent", "umbrella", "absent", "storm"),
    List("noumbrella", "absent", "absent", "absent", "umbrella", "rain"),
    List("absent", "absent", "absent", "absent", "storm")

  )
val reader = SequenceReader()
val states =  reader.readStates (data)
val evidenceVars = reader.readEvidenceVars (data)
val estimator = SequenceEstimation()
val test = estimator.countStateFreq(data)(states)
val pi = estimator.statePriors(data)(states)
val T = estimator.countTransitions(data)(states)
val A = estimator.stateTransitions(data)(states)
val Bk = estimator.avgPriorEvidences(data)(evidenceVars)(states)
val input = InputModel(pi, A, List(Bk), states, evidenceVars)
val model = HiddenMarkovModel.train(input)(data)(0.00001)(50)
val example = List("noumbrella", "absent", "absent", "absent", "umbrella")
val V = estimator.indices (evidenceVars) (example)
val alpha = HiddenMarkovModel.alpha (states.size - 1)(pi)(A)(Bk)(states.size)(evidenceVars.size)(V)
model.A
model.B
val testpredict1 = HiddenMarkovModel.viterbiPredict(model) (List("noumbrella", "umbrella", "umbrella", "noumbrella", "umbrella", "umbrella"))
val testpredict2 = HiddenMarkovModel.viterbiPredict(model) (List("umbrella", "absent", "absent"))
val testpredict3 = HiddenMarkovModel.viterbiPredict(model) (List("noumbrella", "noumbrella", "absent"))
val testpredict4 = HiddenMarkovModel.viterbiPredict(model) (List("noumbrella", "umbrella", "noumbrella", "umbrella", "umbrella", "umbrella", "umbrella", "absent"))

