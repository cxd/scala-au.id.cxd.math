package au.id.cxd.math.app.text

import au.id.cxd.math.app.model.{Project, Resource}
import au.id.cxd.math.data.Writable
import au.id.cxd.text.model.LatentSemanticIndex

/**
  * Created by cd on 22/1/17.
  */
class LsiProject(override val name: String,
                 override val resources: Seq[Resource],
                 val trainEntropy: Double,
                 val kDimensions: Int,
                 val stemTerms:Boolean,
                 val workingModel: LatentSemanticIndex) extends Project(name, resources)
  with Writable[LsiProject] {}


object LsiProject extends au.id.cxd.math.data.Readable[LsiProject] {
  def apply(name: String,
            resources: Seq[Resource],
            trainEntropy: Double,
            kDimensions: Int,
            stemTerms:Boolean,
            workingModel: LatentSemanticIndex) = new LsiProject(name, resources, trainEntropy, kDimensions, stemTerms, workingModel)
}
