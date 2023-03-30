package au.id.cxd.math.example.text.model

import java.awt.event.{ActionEvent, ActionListener}
import java.awt.{BorderLayout, Graphics, GridLayout}
import java.io.File
import java.util

import javax.swing._
import javax.swing.table.{AbstractTableModel, TableModel}
import org.jfree.data._
import org.jfree.data.category._
import org.jfree.chart._
import au.id.cxd.math.data.CsvReader
import au.id.cxd.math.example.charting.ChartHelper
import au.id.cxd.math.example.text.model.LsiModelWriteExample.defaultConfig
import au.id.cxd.math.example.text.model.LsiReadModelExample.getClass
import au.id.cxd.math.example.text.model.config.ModelConfig
import au.id.cxd.math.model.components.SingularValueDecomposition
import au.id.cxd.text.model.LsiComponentCluster.{LsiAttributeCluster, LsiDocumentCluster}
import au.id.cxd.text.model.{LatentSemanticIndex, LsiComponentCluster}
import com.kennycason.kumo.bg.RectangleBackground
import com.kennycason.kumo.font.scale.SqrtFontScalar
import com.kennycason.kumo.palette.ColorPalette
import com.kennycason.kumo.{CollisionMode, WordCloud, WordFrequency}
import org.jfree.chart.plot.PlotOrientation
import org.jfree.data.xy.{DefaultXYDataset, XYSeries}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.collection.JavaConversions._
import scala.swing.{Color, Dimension}

/**
  * This example investigates the use of clustering
  * via the LSI model.
  * Created by cd on 13/1/17.
  *
  * Can be invoked from sbt using
  *
  * runMain au.id.cxd.math.example.text.model.LsiModelClusterExample
  */
object LsiModelClusterExample {

  val defaultConfig = "lsi-model.conf"


  def loadDocuments(config:ModelConfig): ListBuffer[mutable.Buffer[String]] = {
    val url = config.getInputFile()
    val inputCsv = url.getFile
    val data = new CsvReader().readCsv(new File(inputCsv))
    data
  }

  def readModel(config:ModelConfig) = {
    val readResult = LatentSemanticIndex.readBinary(config.targetSer)
    readResult match {
      case Some(lsi2) => {
        println(s"Dimensions: U (${lsi2.svD.U.rows} x ${lsi2.svD.U.cols}) S: ${lsi2.svD.S.length} Vt: (${lsi2.svD.Vt.rows} x ${lsi2.svD.Vt.cols})")
        Some(lsi2)
      }
      case _ => {
        println(s"Failed to read ${config.targetSer}")
        None
      }
    }
  }

  /**
    * Using threshold and minClusters we choose how many components should be used.
    * This is somewhat arbitrary, however the choice of clusters is dependent on the amount of variation accounted for in each component.
    * Use both the measure of entropy and the amount of variation accounted for in each component to make a choice.
    * This can be automated using a threshold, however it may also be necessary to provide a minimum number of clusters desired.
    * For example in the case where all variation is explained by the first component, it is indicative
    * that there is not alot of partitioning necessarily available in the data set.
    * However a minimum of desired clusters may be required in this case.
    */
  def plotEntropy(lsi: LatentSemanticIndex, threshold: Double, minClusters: Int) = {
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
    val frame = new JFrame(s"Entropy ${entropy}")
    frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    frame.setLayout(new GridLayout(1, 1))
    val chart = ChartFactory.createBarChart(s"Variation for first 100 components total Entropy ${entropy}",
      "Component",
      "Variation",
      dataSet,
      PlotOrientation.VERTICAL,
      false, true, false)

    frame.add(ChartHelper.makeChartPanel(chart, 800, 600))
    frame.pack()
    frame.setVisible(true)

    // choose the default number of clusters and the amount of variation selected.
    contributions.toArray.foldLeft(0, 0.0) {
      (accum, v) => {
        v + accum._2 > threshold match {
          case true => accum._1 >= minClusters match {
            case true => accum
            case _ => (accum._1 + 1, accum._2 + v)
          }
          case _ => (accum._1 + 1, accum._2 + v)
        }
      }
    }

  }

  /**
    * create document clusters
    *
    * @param lsi
    */
  def makeDocumentClusters(lsi: LatentSemanticIndex, cluster: LsiComponentCluster, k: Int) = {
    val clusters = cluster.clusterDocuments(lsi, k)
    clusters
  }

  /**
    * create the term clusters
    *
    * @param lsi
    * @param cluster
    * @param k
    * @return
    */
  def makeTermClusters(lsi: LatentSemanticIndex, cluster: LsiComponentCluster, k: Int) = {
    val clusters = cluster.clusterAttributes(lsi, k)
    clusters
  }

  def plotClusterSizes[T, S](clusterA: LsiDocumentCluster, clusterB: LsiAttributeCluster) = {
    val populationA = clusterA.map {
      pair => (pair._1, pair._2.length)
    }.toArray.sortBy { pair => pair._1 }

    val populationB = clusterB.map {
      pair => (pair._1, pair._2.length)
    }.toArray.sortBy { pair => pair._1 }

    val dataSetA = new DefaultCategoryDataset()
    populationA.foreach {
      pair => dataSetA.setValue(pair._2, "", pair._1.toString)
    }
    val dataSetB = new DefaultCategoryDataset()
    populationB.foreach {
      pair => dataSetB.setValue(pair._2, "", pair._1.toString)
    }

    val frame = new JFrame(s"Population of Clusters")
    frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    frame.setLayout(new GridLayout(2, 1))

    val chartA = ChartFactory.createBarChart(s"Population of Document Clusters",
      "Cluster",
      "Population",
      dataSetA,
      PlotOrientation.VERTICAL,
      false, true, false)

    val chartB = ChartFactory.createBarChart(s"Population of Term Clusters",
      "Cluster",
      "Population",
      dataSetB,
      PlotOrientation.VERTICAL,
      false, true, false)


    frame.add(ChartHelper.makeChartPanel(chartA, 800, 300))
    frame.add(ChartHelper.makeChartPanel(chartB, 800, 300))


    frame.pack()
    frame.setVisible(true)

  }


  /**
    * mapping documents to cluster takes a data source, and the set of grouped clusters produced by the LsiComponentCluster
    * Note that the data source for the documents in this example is derived from CSV.
    * As stated earlier, the data source for the documents does not need to be CSV, it is possible to use other data storage mechanisms.
    * The default storage mechanism is CSV, however mapping between external storage and the document source type of ListBuffer[mutable.Buffer[String]]
    * is certainly feasible.
    * The document keys in this example are already known they are indexes 0,1 of each row of the document ListBuffer.
    * The clusters also contain the document keys from the document map.
    * These alternately could be used to query an external data source to retrieve summary text for each cluster member.
    *
    * @param data
    * @param clusters
    */
  def mapDocumentsToCluster(data: ListBuffer[mutable.Buffer[String]], clusters: LsiDocumentCluster): Map[Int, Array[(String, String, String)]] = {
    clusters.map {
      docPair => {
        val cluster = docPair._1
        val cnt = docPair._2.length
        // sort the examples from maximum component value to least
        val results = docPair._2.sortBy(-_._4).map {
          doc => {
            // index x docIds x Component x ComponentWeight
            // (Int , Seq[String], Int, Double )
            if (doc._2.length > 0) {
              val id = doc._2(1)
              val matchRecord = data.find { row => row(1).equalsIgnoreCase(id) }
              matchRecord match {
                case Some(row) => Some((row(0), row(1), row(2)))
                case _ => None
              }
            } else None
          }
        }.filter(!_.isEmpty)
          .map(_.get)
        (cluster, results)
      }
    }
  }

  /**
    * take the clustered terms and sort them by weighting for the cluster component.
    * Return a list of terms for the cluster.
    *
    * @param lsi
    * @param clusters
    * @return
    */
  def mapTermsToCluster(lsi: LatentSemanticIndex, clusters: LsiAttributeCluster): Map[Int, Array[(Int, String, Double)]] =
    clusters.map {
      termPair => {
        val cluster = termPair._1
        val cnt = termPair._2.length
        val terms = termPair._2.sortBy(-_._4)
        //(Cluster x TermColumnIndex x Term x Weight)
        // map it to
        // (ColumnIndex, TermString, TermCount)
        //
        // in cluster.
        val mappedTerms = terms.map { term =>
          //(term._2, term._3._1, term._4)
          (term._2, term._3._1, math.abs(term._3._4))
        }

        (cluster, mappedTerms)
      }
    }


  /**
    * draw a word cloud for the supplied cluster.
    *
    * @param cluster
    * @param terms
    */
  def drawTermCloudPanel(k: Int, terms: Map[Int, Array[(Int, String, Double)]]) = {
    terms.get(k).map {
      cluster => {
        val weights = cluster.map(_._3)
        val minWeight = weights.min
        // scaling factor to convert to whole numbers.
        val factor = 1 //100* (1.0 / minWeight)
        val subset = cluster.length > 100 match {
          case true => cluster.sortBy(-_._3).take(100)
          case _ => cluster.sortBy(-_._3)
        }
        // map to a word frequency type for the word cloud renderer.
        val wordFreq = subset.map {
          term => new WordFrequency(term._2, Math.round(term._3 * factor).toInt)
        }.toList

        // explicitly convert to java collection
        val wordFreqList = new util.ArrayList[WordFrequency]()
        wordFreqList.addAll(wordFreq)
        // create the word cloud
        val dim = new Dimension(800, 600)
        val cloud = new WordCloud(dim, CollisionMode.PIXEL_PERFECT)
        cloud.setPadding(0)
        cloud.setBackground(new RectangleBackground(dim))
        cloud.setColorPalette(new ColorPalette(new Color(0x4055F1), new Color(0x408DF1), new Color(0x40AAF1), new Color(0x40C5F1), new Color(0x40D3F1), new Color(0xFFFFFF)))
        cloud.setFontScalar(new SqrtFontScalar(10, 40))
        cloud.build(wordFreqList)
        val bufIm = cloud.getBufferedImage
        val panel = new JPanel() {
          override def paintComponent(g: Graphics): Unit = {
            super.paintComponent(g)
            g.drawImage(bufIm, 0, 0, 800, 600, null)
          }
        }
        panel.setSize(800, 600)
        panel
      }
    }
  }

  /**
    * redraw the term cloud.
    *
    * @param panel
    * @param k
    * @param terms
    * @return
    */
  def redrawTermCloud(panel: JPanel, k: Int, terms: Map[Int, Array[(Int, String, Double)]]) = {
    drawTermCloudPanel(k, terms) map {
      child => {
        panel.removeAll()
        panel.add(child)
        panel.revalidate()
        panel.repaint()
      }
    }
  }

  /**
    * ceate the frame containing the term cloud.
    *
    * @param terms
    */
  def createTermFrame(terms: Map[Int, Array[(Int, String, Double)]]) = {
    val termFrame = new JFrame(s"Cluster TermClouds")
    termFrame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    termFrame.setPreferredSize(new Dimension(800, 800))
    termFrame.setLayout(new BorderLayout())

    val clusters = terms.keys.toArray.sorted.map { i => new Integer(i) }
    val selectPanel = new JPanel()
    val dim1 = new Dimension(800, 100)
    selectPanel.setSize(dim1)
    selectPanel.setPreferredSize(dim1)
    selectPanel.setLayout(new GridLayout(1, 2))

    val imagePanel = new JPanel()
    val dim2 = new Dimension(800, 600)
    imagePanel.setSize(dim2)
    imagePanel.setPreferredSize(dim2)
    imagePanel.setLayout(new GridLayout(1, 1))

    selectPanel.add(new JLabel("Select Cluster"))
    val combo = new JComboBox(clusters)
    combo.setSelectedIndex(0)
    combo.addActionListener(new ActionListener {
      def actionPerformed(e: ActionEvent): Unit = {
        val item = combo.getSelectedItem.asInstanceOf[Int]

        redrawTermCloud(imagePanel, item, terms)
      }
    })
    selectPanel.add(combo)
    termFrame.add(selectPanel, BorderLayout.NORTH)
    termFrame.add(imagePanel, BorderLayout.CENTER)


    redrawTermCloud(imagePanel, 0, terms)

    termFrame.pack()
    termFrame.setVisible(true)
  }

  /**
    * generate a scatter plot for the dimensions
    * of the term matrix specified in dim1 and dim2.
    *
    * @param lsi
    * @param dimX
    * @param dimY
    * @param clusters
    */
  def createTermScatterPlot(lsi: LatentSemanticIndex, dimX: Int, dimY: Int, clusters: Map[Int, Array[(Int, String, Double)]]) = {
    // k x n attribute relations
    val Vt = SingularValueDecomposition.scaleAttributes(lsi.svD)// lsi.svD.Vt
    val dataA = Vt(dimX, ::).inner.toArray
    val dataB = Vt(dimY, ::).inner.toArray
    val xyData = dataA.zip(dataB)
    // we construct a data set for the scatter plot
    val dataSet = new DefaultXYDataset()
    // there are k possible clusters
    // data from two components contains n columns for each term
    // we now need to create k XY series each containing values from X and Y dataA,dataB for the cluster.
    clusters.keys.toArray.sorted.foreach {
      key => {
        val cluster = (key, clusters.get(key).get)
        val clusterName = cluster._1.toString
        val series = new XYSeries(clusterName)
        val (xdata, ydata) = cluster._2.foldLeft(List[(Double, Double)]()) {
          (accum, pair) => {
            val data = xyData(pair._1)
            accum :+ data
          }
        }.unzip
        val term = cluster._2.sortBy(-_._3).head
        // TODO: use the term as the shape for the cluster.
        dataSet.addSeries(clusterName, Array(xdata.toArray, ydata.toArray))
      }
    }
    val chart = ChartFactory.createScatterPlot(s"Attribute components $dimX $dimY",
      s"Component $dimX",
      s"Component $dimY",
      dataSet,
      PlotOrientation.HORIZONTAL,
      true, true, false)

    val frame = new JFrame(s"Attribute Components for Clusters")
    frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    frame.setLayout(new GridLayout(1, 1))

    frame.add(ChartHelper.makeChartPanel(chart, 800, 600))

    frame.pack()
    frame.setVisible(true)

  }

  def createDocumentScatterPlot(lsi:LatentSemanticIndex, dimX:Int, dimY:Int, docs: LsiDocumentCluster) = {
    val U = SingularValueDecomposition.scaleObjects(lsi.svD)//lsi.svD.U
    val dataX = U(::,dimX).toArray
    val dataY = U(::, dimY).toArray
    val xyData = dataX.zip(dataY)
    // we construct a data set for the scatter plot
    val dataSet = new DefaultXYDataset()

    docs.keys.toArray.sorted.foreach {
      key => {
        val cluster = (key, docs.get(key).get)
        val (xdata, ydata) = cluster._2.foldLeft(List[(Double,Double)]()) {
          (accum, row) => {
            val data = xyData(row._1)
            accum :+ data
          }
        }.unzip
        dataSet.addSeries(key.toString, Array(xdata.toArray, ydata.toArray))
      }
    }

    val chart = ChartFactory.createScatterPlot(s"Document components $dimX $dimY",
      s"Component $dimX",
      s"Component $dimY",
      dataSet,
      PlotOrientation.HORIZONTAL,
      true, true, false)

    val frame = new JFrame(s"Document Components for Clusters")
    frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    frame.setLayout(new GridLayout(1, 1))

    frame.add(ChartHelper.makeChartPanel(chart, 800, 600))

    frame.pack()
    frame.setVisible(true)

  }


  /**
    * create the table model for the supplied cluster
    *
    * @param k
    * @param docs
    */
  def createTableModel(k: Int, docs: Map[Int, Array[(String, String, String)]]) = {
    docs.get(k).map {
      doc => {

        new AbstractTableModel() {
          val documents = doc
          val columnNames = Array("docType", "docId", "text")
          val columnClasses = Array(classOf[String], classOf[String], classOf[String])

          def getRowCount: Int = documents.length

          override def getColumnClass(columnIndex: Int): Class[_] = {
            columnClasses(columnIndex)
          }

          def getColumnCount: Int = columnNames.length

          override def getColumnName(col: Int) = columnNames(col)

          def getValueAt(rowIndex: Int, columnIndex: Int): AnyRef = {
            val record = documents(rowIndex)
            val arr = Array(record._1, record._2, record._3)
            arr(columnIndex)
          }
        }
      }
    }
  }


  def createTable(model: TableModel) = {
    val table = new JTable(model)
    val scrollPane = new JScrollPane(table)
    (scrollPane, table)
  }

  /**
    * use a similar method to the term frame,
    * select a cluster at a time to update the document table contents
    *
    * @param docs
    */
  def createDocumentFrame(docs: Map[Int, Array[(String, String, String)]]) = {
    val docFrame = new JFrame("Document Clusters")
    docFrame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    docFrame.setPreferredSize(new Dimension(800, 800))
    docFrame.setLayout(new BorderLayout())

    val clusters = docs.keys.toArray.sorted.map { i => new Integer(i) }
    val selectPanel = new JPanel()
    val dim1 = new Dimension(800, 100)
    selectPanel.setSize(dim1)
    selectPanel.setPreferredSize(dim1)
    selectPanel.setLayout(new GridLayout(1, 2))
    selectPanel.add(new JLabel("Select Cluster"))
    val combo = new JComboBox(clusters)
    combo.setSelectedIndex(0)

    val model = createTableModel(0, docs).get
    val (scrollPane, table) = createTable(model)


    combo.addActionListener(new ActionListener {
      def actionPerformed(e: ActionEvent): Unit = {
        val item = combo.getSelectedItem.asInstanceOf[Int]
        createTableModel(item, docs).foreach {
          model1 => {
            table.setModel(model1)
            model1.fireTableDataChanged()
            table.repaint()
          }
        }
      }
    })
    selectPanel.add(combo)
    docFrame.add(selectPanel, BorderLayout.NORTH)

    // create
    val tablePanel = new JPanel()
    val dim2 = new Dimension(800, 600)
    tablePanel.setSize(dim2)
    tablePanel.setPreferredSize(dim2)
    tablePanel.setLayout(new GridLayout(1, 1))
    tablePanel.add(scrollPane)


    docFrame.add(tablePanel, BorderLayout.CENTER)


    docFrame.pack()
    docFrame.setVisible(true)
  }


  def main(args: Array[String]) = {
    val config = ModelConfig(args, defaultConfig)
    val data = loadDocuments(config)
    readModel(config) foreach {
      model => {
        println("Plotting Entropy")
        val (k, variation) = plotEntropy(model, 0.9997, 50)
        println(s"selected $k clusters")

        val cluster = new LsiComponentCluster {}
        val docClusters = makeDocumentClusters(model, cluster, k)

        val termClusters = makeTermClusters(model, cluster, k)

        println("Plotting cluster populations")
        plotClusterSizes(docClusters, termClusters)

        val mappedTerms = mapTermsToCluster(model, termClusters)
        println("Plotting Term Clouds")
        createTermFrame(mappedTerms)

        val mappedDocs = mapDocumentsToCluster(data, docClusters)
        println("Plotting Document Table")
        createDocumentFrame(mappedDocs)

        println("Plotting Attribute Components")
        createTermScatterPlot(model, 0, 1, mappedTerms)

        println("Plotting Document Components")
        createDocumentScatterPlot(model, 0, 1, docClusters)

      }
    }
  }

}
