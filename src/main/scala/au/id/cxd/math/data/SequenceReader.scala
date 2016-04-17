package au.id.cxd.math.data

import java.io.File

/**
  * The sequence reader class reads staggered sequences
  * where the last value on the line is seen as a state label
  * and all preceding items are regarded as features or observations.
  *
  * All tokens are separated by a comma.
  *
  * For example from the CTI telephony world where evidence variables represent events and states represent call states:
  *
  * {{{
  * # The last column in each line represents the state label.
  * # evidence variables are those items appearing in each column before the last column.
  * Ringing(inbound),OnCall
  * Ringing(inbound),AttachedDataChanged,OffHook,OnCall
  * Ringing(inbound),AttachedDataChanged,OffHook,Established,OnCall
  * Ringing(inbound),UserEvent(Start),UserEvent(Stop),AttachedDataChanged,OffHook,Established,OnCall
  * Ringing(inbound),UserEvent(Start),UserEvent(Stop),AttachedDataChanged,AttachedDataChanged,OffHook,Established,OnCall
  * }}}
  *
  * Example usage:
  *
  * {{{
  * val fileName = "data/example_train_data.csv"
  * val file = new File(fileName)
  * val reader = SequenceReader()
  * // the data set (jagged array)
  * val data = reader.readSequences(file)
  * // unique set of states
  * val states =  reader.readStates (data)
  * // the unique labels for evidence variables
  * val evidenceVars = reader.readEvidenceVars (data)
  *
  * }}}
  *
  * This is used in conjunction with a [[SequenceEstimation]] in order to prepare a model for use
  * with the [[au.id.cxd.math.model.sequence.HiddenMarkovModel]].
  *
  *
  * Created by cd on 10/01/15.
  */
class SequenceReader extends TextReader {

  /**
    * read all sequences from a file
    * split on commas in each line
    *
    * @param file
    * @return
    */
  def readSequences(file: File): List[List[String]] = {
    val lines = readLines(file)
    removeComments(lines) map {
      line => {
        line.split(",").toList
      }
    }
  }

  /**
    * read all states from the data
    * the last item in each line is considered to be a state
    *
    * @param data
    * @return
    */
  def readStates(data: List[List[String]]): List[String] = {
    val states =
      data map {
        dataLine => {
          dataLine.reverse.head
        }
      }
    states
      .sorted
      .distinct
  }

  /**
    * read the evidence variables from the test data
    * evidence variables are considered to be each item prior to the state label.
    *
    * @param data
    * @return
    */
  def readEvidenceVars(data: List[List[String]]): List[String] = {
    val evidence = data.foldLeft(List[String]()) {
      (accum, item) => {
        val seq = item.reverse.tail
        seq.foldLeft(accum) {
          (accum1, item) => item :: accum1
        }
      }
    }
    evidence
      .sorted
      .distinct
  }

}

object SequenceReader {
  def apply() = new SequenceReader()
}