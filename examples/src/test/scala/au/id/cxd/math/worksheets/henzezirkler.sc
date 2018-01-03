import java.io.File

import au.id.cxd.math.data.MatrixReader
import au.id.cxd.math.function.transform.StandardisedNormalisation
import au.id.cxd.math.probability.analysis.{HenzeZirklerTest, MardiaTest}
import au.id.cxd.math.probability.continuous.Normal

import scala.math.{exp, pow, sqrt}

// demonstrating the use of mardia test.


val fileName:String = "/Users/cd/Projects/scala/au.id.cxd.math/data/iris_virginica.csv"

new File(fileName).getAbsolutePath

val mat = MatrixReader.readFileAt(fileName)
val data = mat(::, 0 to 3).toDenseMatrix
val X = StandardisedNormalisation().transform(data)
val test = HenzeZirklerTest(0.05, X)

test
