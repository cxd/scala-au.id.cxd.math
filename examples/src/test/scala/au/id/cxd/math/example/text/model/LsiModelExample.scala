package au.id.cxd.math.example.text.model

import java.io.File

import au.id.cxd.math.data.CsvReader
import au.id.cxd.text.count.TfIdfCount
import au.id.cxd.text.helpers.EmbeddedStopwordsLoader
import au.id.cxd.text.model.LatentSemanticIndex

/**
  * This class is provided to experiment with the LSI model
  * on larger corpus. In order to execute the class it should be invoked
  * with suitable memory parameters.
  *
  * The process to build the SVD may take up to over 3gb of RAM once completed it will then free memory back around 1gb.
  * This is due to the moderate data size. Obviously depending on the volume of data, RAM and CPU will affect performance.
  * The algorithm is not a distributed algorithm and computes in process.
  * For this example make sure to add java arguments for -Xmx4g -Xms1g
  *
  * This program takes an optional query as the first argument or instead uses the default query:
  *
  * "http redirect error".
  *
  * The document corpus data used in the example is taken from the apache defects list for a variety of projects.
  *
  * The example computes the entire SVD model it then computes the search space and performs a search against it.
  *
  * In a real world application, the LatentSemanticIndex which contains the SVD, document row map and term index map would be computed separately and saved to an archive.
  *
  * In a long running system the model would be loaded at startup, and the search space computed at startup.
  * It would not be necessary to recompute this on subsequent requests, only the query, the query projection and the cosine distances would need to be computed per request.
  *
  * In this example, the resulting text is retrieved from CSV, however, given the document ID map is available in memory, it would be possible to use an external system
  * such as a data base or other system, to act as the storage to retrieve text examples from.
  *
  * Note also, the col term map and the document map could also be externalised, however in this implementation it is retained in memory.
  * If externalising the members of the LSI model, loading those and assigning them back to the instance at startup could be done as a separate procedure without
  * any need of using the default methods for archiving built into the model.
  *
  *
  * An example of the output from this program is:
  *
Entropy: 9.894113652385852E-4
Contributions: DenseVector(0.9994790785406367, 1.835121819616125E-6, 1.7322777058828093E-6, 1.6448122003800841E-6, 1.6114471046601103E-6, 1.5503555534535887E-6, ... , )
Dimensions: U (5118 x 5079) S: 5079 Vt: (5079 x 5079)

Reduced Dimensions: U (5118 x 1001) S: 1001 Vt: (1001 x 5079)
Search Space U dimension: 5118 x 1001
Search Space V dimension: 5079 x 1001

First 10 Search Results
ArrayBuffer((522,0.00857476395210225,List(Apache httpd-2, 55452)), (779,0.008411627826394047,List(Apache httpd-2, 48499)), (3029,0.00769331654015826,List(Apache httpd-2, 56028)), (2198,0.00766045509643702,List(Ant, 50379)), (1624,0.007532632780709755,List(Apache httpd-2, 52465)), (1698,0.007404865348059863,List(Apache httpd-2, 41000)), (1092,0.007404865348059654,List(Apache httpd-2, 46762)), (452,0.007404865348059145,List(Apache httpd-2, 47814)), (4292,0.007273857873182794,List(Fop - Now in Jira, 49842)), (3063,0.007225493338190402,List(Apache httpd-2, 51814)))
First 10 Search Results
List((55452,"Redirect"), (48499,"mod_rewrite can't redirect to files with ? in them"), (56028,"Add http/1.0), (50379,"get task does not last part of url when redirected"), (52465,"mod_dir is allowed to redirect proxy requests"), (41000,"mod_authn_dbd have an error), (46762,"Error), (47814,"openssl_pkcs7_sign error"), (49842,"1.0: not all events redirected"), (51814,"mod_proxy in Apache HTTP 2.2 FIN_WAIT2 in server side))

  *
  * Created by cd on 13/1/17.
  */
object LsiModelExample {

  val textData = "example_text_corpus_data.csv"

  val defaultQuery = Array("http", "redirect", "error")


  def buildModel(query:Array[String]) = {
    val url = getClass.getClassLoader().getResource(textData)
    val inputCsv = url.getFile
    val idCols = Seq(0,1)
    val (entropy, contributions, lsi) = LatentSemanticIndex.buildFromCsv(inputCsv, idCols)

    println(s"Entropy: $entropy")
    println(s"Contributions: $contributions")
    // TODO: plot contributions for first k

    println(s"Dimensions: U (${lsi.svD.U.rows} x ${lsi.svD.U.cols}) S: ${lsi.svD.S.length} Vt: (${lsi.svD.Vt.rows} x ${lsi.svD.Vt.cols})")


    // Now given that the entropy is very close to 0 this means the variation is explained largely in the first dimension.
    // however the first component explains the 99% or so of the variation.
    // we will reduce the LSI to roughly 1000 dimensions which is less than the 5000 or so dimensions from the original tfidf.
    val lsi2 = LatentSemanticIndex.reduceToDimensons(lsi, 1000)
    println(s"Reduced Dimensions: U (${lsi2.svD.U.rows} x ${lsi2.svD.U.cols}) S: ${lsi2.svD.S.length} Vt: (${lsi2.svD.Vt.rows} x ${lsi2.svD.Vt.cols})")

    // Note: prior to performing the search we can make use of the entropy and contributions to select k dimensions from the search space.
    // at this stage we have not done that.

    val (ssU, ssS, ssVt) = LatentSemanticIndex.makeSearchSpace(lsi2)

    // Note from inspection, we can see that the entropy is close to 0, and that the model has

    println(s"Search Space U dimension: ${ssU.rows} x ${ssU.cols}")
    println(s"Search Space V dimension: ${ssVt.rows} x ${ssVt.cols}")

    val loader = EmbeddedStopwordsLoader()
    val stopwords = loader.load()


    // TODO: debug the search process and the search projection
    val searchResults = LatentSemanticIndex.performSearch((ssU, ssS, ssVt), TfIdfCount(), query, stopwords, lsi2)


    println(s"First 10 Search Results")
    println(searchResults.take(10))

    val data = new CsvReader().readCsv(new File(inputCsv))
    // find the data that matches the search results.
    val results = searchResults.take(10).foldLeft(Seq[Tuple2[String, String]]()) {
      (accum, result) => {
        val ids = result._3
        val id = ids(1)
        val matchRecord = data.find { row => row(1).equalsIgnoreCase(id) }
        matchRecord match {
          case Some(row: _root_.scala.collection.mutable.Buffer[_root_.scala.Predef.String]) => accum :+ ((row(1), row(2)))
          case _ => accum
        }
      }
    }
    println(s"First 10 Search Results")
    println(results)
  }



  def main(args: Array[String]) = {

    val query = args.length > 0 match {
      case true => args(0).split(" ")
      case _ => defaultQuery
    }


    buildModel(query)

  }

}
