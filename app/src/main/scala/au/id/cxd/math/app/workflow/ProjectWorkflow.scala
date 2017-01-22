package au.id.cxd.math.app.workflow

import au.id.cxd.math.app.model.Project

/**
  * Created by cd on 22/1/17.
  */
class ProjectWorkflow[T <: Project](override val data:T) extends Workflow[T](data) {

}

object ProjectWorkflow {

  def id[T<: Project](data:T) = new ProjectWorkflow(data)

  def apply[T<: Project](data:T) = id(data)

}

trait ProjectWorkflowStep[T <: Project] extends WorkflowStep[T] {

}