package au.id.cxd.math.app.workflow

/**
  * Created by cd on 22/1/17.
  */
class Workflow[K](val data:K) {

  def map[S](fn:K => S):Workflow[S] = {
    val result = fn (data)
    new Workflow(result)
  }

  def flatMap[S](fn:K => Workflow[S]):Workflow[S] = fn(data)

}
object Workflow {

  def id[T](data:T) = new Workflow(data)

  def apply[T](data:T) = id(data)
}


trait WorkflowStep[T] {
  def runWorkflow():Workflow[T]

}

trait ParameterisedWorkflowStep[S,T] {
  def runWorkflow(input:S):Workflow[T]
}

