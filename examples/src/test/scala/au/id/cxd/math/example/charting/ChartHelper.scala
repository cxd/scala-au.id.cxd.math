package au.id.cxd.math.example.charting

import org.jfree.chart.{ChartPanel, JFreeChart}

/**
  * Created by cd on 30/04/2016.
  */
object ChartHelper {

  def makeChartPanel(chart:JFreeChart, width:Int, height:Int) = {
    //ChartPanel(JFreeChart chart, int width, int height, int minimumDrawWidth, int minimumDrawHeight, int maximumDrawWidth, int maximumDrawHeight, boolean useBuffer, boolean properties, boolean save, boolean print, boolean zoom, boolean tooltips

    new ChartPanel(chart, width, height, width, height, width, height, false, true, true, true,
      true, true)

  }

}
