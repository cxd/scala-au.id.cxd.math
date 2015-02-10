import java.io.File

import au.id.cxd.math.data.{SequenceEstimation, SequenceReader}
import au.id.cxd.math.model.entity.hmm.{Model, InputModel}
import au.id.cxd.math.model.sequence.HiddenMarkovModel
val fileName = "/Users/cd/Projects/scala/au.id.cxd.math/src/test/data/example_train_data.csv"
val file = new File(fileName)
println("File Path: " + file.getAbsolutePath)
val reader = SequenceReader()
val data = reader.readSequences(file)
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
val test5 = List("Ringing(inbound)",
  "UserEvent(Start)",
  "UserEvent(Stop)",
  "OffHook",
  "Established",
  "Held",
  "Dialing(Consult)");
val predict1 = HiddenMarkovModel.viterbiPredict (model) (test5)
val test6 = List("Ringing(inbound)",
  "UserEvent(Start)",
  "UserEvent(Stop)",
  "OffHook",
  "Established",
  "Held")
val predict2 = HiddenMarkovModel.viterbiPredict (model) (test6)
val test7 = List("Ringing(inbound)",
  "UserEvent(Start)",
  "UserEvent(Stop)",
  "OffHook",
  "Established",
  "Held",
  "Released")
val predict3 = HiddenMarkovModel.viterbiPredict (model) (test7)

// test saving the model
val result = for {
  flag <- Model.write("testcti.model")(model)
  model2 <- Model.read("testcti.model")
  // try prediction on the deserialized model
  predict = HiddenMarkovModel.viterbiPredict (model2)(test7)
} yield predict
