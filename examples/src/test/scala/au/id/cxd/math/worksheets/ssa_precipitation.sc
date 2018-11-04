import java.io.File

import au.id.cxd.math.data.MatrixReader
import au.id.cxd.math.model.sequence.SingularSpectrumAnalysis

val fileName:String = "/Users/cd/Projects/scala/au.id.cxd.math/data/precipitation/example_cape_moreton.csv"

new File(fileName).getAbsolutePath

val mat = MatrixReader.readFileAt(fileName, cols=Seq(1))
val data = mat(::, 0).toArray
// estimating SSA with 365 days per stride
val (eVals,eVec,varExp,proj) = SingularSpectrumAnalysis(data,365)

varExp(0 to 10)