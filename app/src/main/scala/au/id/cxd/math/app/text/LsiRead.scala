package au.id.cxd.math.app.text

import au.id.cxd.math.app.workflow.{ProjectWorkflow, ProjectWorkflowStep, Workflow, WorkflowStep}

/**
  * Read the project from the base dir
  * Created by cd on 22/1/17.
  */
case class LsiRead(val name:String, val baseDir:String)
  extends WorkflowStep[Option[LsiProject]] {

  /**
    * read the project
    * @return
    */
  def readProject() = {
    val targetFile = s"""${baseDir.stripSuffix("/")}/${name}-project.ser"""
    LsiProject.read(targetFile)
  }

  /**
    * run the operation as a workflow
    * @return
    */
  def runWorkflow() = {
    val proj = readProject()
    Workflow(proj)
  }

}
