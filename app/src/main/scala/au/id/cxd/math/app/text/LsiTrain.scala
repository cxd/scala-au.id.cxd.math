package au.id.cxd.math.app.text

import java.io.File
import javax.imageio.ImageIO

import au.id.cxd.math.app.model.Resource
import au.id.cxd.math.app.workflow.{ProjectWorkflow, ProjectWorkflowStep, Workflow}
import au.id.cxd.math.model.components.SingularValueDecomposition
import au.id.cxd.text.model.LatentSemanticIndex
import org.jfree.chart.ChartFactory
import org.jfree.chart.plot.PlotOrientation
import org.jfree.data.category.DefaultCategoryDataset

import scala.util.Success

/**
  * Created by cd on 22/1/17.
  */
/**
  * Start the latent semantic indexer training process.
  *
  * @param name
  * Provide a group name for the model
  * @param inputCsv
  * Define the source CSV that contains the training data.
  * @param idCols
  * Define which id columns to remove from the training data.
  * @param kDimensions
  * Define the number of dimensions to use after reduction via Singular value decomposition.
  * @param baseDir
  * Define the working base directory where the resources for the model should be created.
  */
case class LsiTrain(val name: String,
                    val inputCsv: String,
                    val idCols: Seq[Int],
                    val kDimensions: Int,
                    val baseDir: String,
                    val writeArchive: Boolean = false,
                    val stemTerms:Boolean = true)
  extends ProjectWorkflowStep[LsiProject] {

  /**
    * chart the first 100 contributions associated with the components
    */
  def chartContributions(lsi: LatentSemanticIndex, resources: Seq[Resource]): Seq[Resource] = {
    val entropy = SingularValueDecomposition.entropy(lsi.svD)
    val contributions = SingularValueDecomposition.contributions(lsi.svD)

    // select the first 100 components
    val first100 = contributions(0 until 100)
    // assign entropy contribution to category for each component
    val dataSet = new DefaultCategoryDataset()
    first100.toArray.zipWithIndex.foreach {
      pair => {
        dataSet.setValue(pair._1, "", pair._2.toString)
      }
    }
    val chart = ChartFactory.createBarChart(s"Variation for first 100 components total Entropy ${entropy}",
      "Component",
      "Variation",
      dataSet,
      PlotOrientation.VERTICAL,
      false, true, false)
    val img = chart.createBufferedImage(800, 600)
    val targetFile = s"""${baseDir.stripSuffix("/")}/contributions-100.png"""
    ImageIO.write(img, "png", new File(targetFile))
    resources :+ Resource("contributions-100", Map("contributions-100.png" -> targetFile))
  }

  /**
    * write the model to a binary file.
    *
    * @param lsi
    * @param resources
    * @return
    */
  def writeModel(lsi: LatentSemanticIndex, resources: Seq[Resource]): Seq[Resource] = {
    val targetSer = s"""${baseDir.stripSuffix("/")}/$name.ser"""
    LatentSemanticIndex.writeBinary(lsi)(targetSer) match {
      case Some(true) => resources :+ Resource("model", Map(s"$name.ser" -> targetSer))
      case _ => resources
    }
  }

  /**
    * write a zip archive of CSV files that can be loaded into the R application.
    *
    * @param lsi
    * @return
    */
  def writeZipArchive(flag: Boolean, lsi: LatentSemanticIndex, resources: Seq[Resource]): Seq[Resource] =
    flag match {
      case false => resources
      case true => {
        val targetPath = s"""${baseDir.stripSuffix("/")}/$name.zip"""
        // the contents of the zip file contains csv data for use in visualisation tools such as R.
        LatentSemanticIndex.writeZipTemp(lsi)(targetPath) match {
          case Success(true) => {
            resources :+ Resource("archive", Map(s"$name.zip" -> targetPath))
          }
          case _ => resources
        }
      }
    }

  /**
    * build resources for the project
    *
    * @param writeCsvFlag
    * @param lsi
    * @return
    */
  def buildResources(writeCsvFlag: Boolean, lsi: LatentSemanticIndex): Seq[Resource] = {
    writeZipArchive(writeCsvFlag, lsi, Seq[Resource]()) ++
      writeModel(lsi, Seq[Resource]()) ++
      chartContributions(lsi, Seq[Resource]())
  }

  /**
    * write the project file.
    *
    * @param lsiP
    * @return
    */
  def writeProjectFile(lsiP: LsiProject): Boolean = {
    val targetFile = s"""${baseDir.stripSuffix("/")}/${lsiP.name}-project.ser"""
    lsiP.write(targetFile)(lsiP) match {
      case Some(true) => true
      case _ => false
    }
  }


  /**
    * create the initial model, and flag whether the generate a CSV archive
    * to allow exploration of the resulting model.
    *
    * @return
    */
  def createModel() = {
    val (entropy, contributions, lsi) = LatentSemanticIndex.buildFromCsv(inputCsv, idCols, stemTerms = stemTerms)
    val lsi2 = LatentSemanticIndex.reduceToDimensons(lsi, kDimensions)
    val proj = LsiProject(name, Seq[Resource](), entropy, kDimensions, stemTerms, lsi2)
    writeProjectFile(proj)
    proj
  }

  /**
    * update the resources in the project
    *
    * @param archiveFlag
    * @param proj
    * @return
    */
  def updateResources(archiveFlag: Boolean)(proj: LsiProject) = {
    val lsi = proj.workingModel
    val resources = buildResources(archiveFlag, lsi)
    LsiProject(proj.name, resources, proj.trainEntropy, proj.kDimensions, stemTerms = proj.stemTerms, workingModel = proj.workingModel)
  }


  /**
    * train the lsi model
    * and return it as a project work flow.
    *
    * @return
    */
  def runWorkflow(): Workflow[LsiProject] = {
    val workflow = ProjectWorkflow(createModel()) map {
      updateResources(writeArchive)(_)
    } map {
      proj => {
        writeProjectFile(proj)
        proj
      }
    }
    workflow
  }

}
