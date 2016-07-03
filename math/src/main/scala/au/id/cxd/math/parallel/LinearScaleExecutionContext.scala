package au.id.cxd.math.parallel

import java.util.concurrent.Executors

import scala.concurrent.ExecutionContext

/**
  * Create an execution context that scales the threadpool using
  * the number of process multiplied by a constant factor
  * Created by cd on 3/07/2016.
  */
class LinearScaleExecutionContext(val scale:Int) extends ExecutionContext {
  // we'll limit the number of threads to 2 * the physical number of processors
  val numProcessors = Runtime.getRuntime.availableProcessors * scale

  val threadPool = Executors.newFixedThreadPool(numProcessors);

  def execute(runnable: Runnable) {
    threadPool.submit(runnable)
  }

  def reportFailure(t: Throwable) {}
}

object LinearScaleExecutionContext {
  def apply(scale:Int) = new LinearScaleExecutionContext(scale)
}