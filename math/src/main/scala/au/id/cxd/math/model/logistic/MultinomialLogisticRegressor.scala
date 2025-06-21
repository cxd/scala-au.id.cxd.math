package au.id.cxd.math.model.logistic

import au.id.cxd.math.count.CrossTabulate
import au.id.cxd.math.data.{DataSet, PredictorTargetData}
import au.id.cxd.math.function.moments.Mean
import au.id.cxd.math.model.network.activation.{Identity, Relu, Sigmoid, Softmax}
import au.id.cxd.math.model.network.builder.Sequence
import au.id.cxd.math.model.network.initialisation.RandomWeightInitialisation
import au.id.cxd.math.model.network.layers.{DenseLayer, InputLayer}
import au.id.cxd.math.model.network.loss.{DiscreteCrossEntropy, LogisticNegativeLogLikelihood}
import au.id.cxd.math.model.network.train.SGDTrainer
import breeze.linalg.DenseMatrix

/**
 * Perform multinomial logistic regression using a simple 1 layer neural network
 * and train with negative log likelihood
 *
 * Created by cd on 21/06/2025.
 */
case class MultinomialLogisticRegressor(val dataSet: DataSet,
                                        val numFeatures: Int,
                                        val numClasses: Int,
                                        val featureColRange: (Int, Int),
                                        val targetColRange: (Int, Int),
                                        val classLabels: List[String],
                                        val logits: Boolean = false,
                                        val temperature: Double = 1.0,
                                        val trainEpochs: Int = 100,
                                        val learnRate: Double = 10e-3,
                                        val momentum: Double = 10e-5) extends PredictorTargetData {

  var train_loss: Double = 0.0
  var validation_loss: Double = 0.0


  /**
   * The logist model will be represented as a single layer network
   * with a linear activation
   *
   * @param numFeatures
   * @param numClasses
   * @return
   */
  def buildModel(numFeatures: Int, numClasses: Int): Sequence = {
    val initialisation = RandomWeightInitialisation()
    Sequence(Seq(
      InputLayer(units = numFeatures),
      DenseLayer(activation = Identity(), units = numFeatures),
      DenseLayer(activation = Softmax(), units = numClasses)
    ), initialisation).compile()
      .asInstanceOf[Sequence]
  }

  /**
   * train the model
   *
   * @param epochs
   * @param model
   * @param trainX
   * @param trainY
   * @param validX
   * @param validY
   * @return
   */
  def trainModel(epochs: Int, model: Sequence, trainX: DenseMatrix[Double], trainY: DenseMatrix[Double], validX: DenseMatrix[Double], validY: DenseMatrix[Double]): Sequence = {

    val trainer = SGDTrainer(trainX, trainY, validX, validY,
      learnRate = learnRate,
      momentum = momentum,
      lossFn = LogisticNegativeLogLikelihood(logits, temperature))

    val (loss, valloss, trained_model) = trainer.train(epochs)(model)
    train_loss = Mean(loss)
    validation_loss = Mean(valloss)
    trained_model
  }

  def test(model: Sequence, classLabels: List[String], dataSet: DataSet): (DenseMatrix[Double], (Double, Double, Double, Double, Double, Double)) = {
    val testX = dataSet.testData(::, featureColRange._1 to featureColRange._2)
    val testY = dataSet.testData(::, targetColRange._1 to targetColRange._2)

    val labels = model.classify(testX, classLabels)

    val actualLabels = dataSet.convertToClassLabels("origin", testY)
    val xtab = CrossTabulate(actualLabels, labels)
    CrossTabulate.printMetrics(xtab)
    val metrics = CrossTabulate.metrics(xtab)
    (xtab, metrics)
  }

  def train(): Sequence = {

    val trainX = dataSet.trainData(::, featureColRange._1 to featureColRange._2)
    val trainY = dataSet.trainData(::, targetColRange._1 to targetColRange._2)
    val validX = dataSet.crossValidateData(::, featureColRange._1 to featureColRange._2)
    val validY = dataSet.crossValidateData(::, targetColRange._1 to targetColRange._2)


    val model = buildModel(numFeatures, numClasses)

    val trainedModel = trainModel(trainEpochs,
      model,
      trainX,
      trainY,
      validX,
      validY
    )


    trainedModel
  }


}
