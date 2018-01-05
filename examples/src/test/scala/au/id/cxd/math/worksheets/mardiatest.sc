import java.io.File

import au.id.cxd.math.data.MatrixReader
import au.id.cxd.math.function.transform.StandardisedNormalisation
import au.id.cxd.math.probability.analysis.MardiaTest
import au.id.cxd.math.probability.continuous.Normal

import scala.math.{exp, pow, sqrt}

// demonstrating the use of mardia test.


val fileName:String = "/Users/cd/Projects/scala/au.id.cxd.math/data/iris_virginica.csv"

new File(fileName).getAbsolutePath

val mat = MatrixReader.readFileAt(fileName)
val data = mat(::, 0 to 3).toDenseMatrix
val X = StandardisedNormalisation().transform(data)
val test = MardiaTest(0.05, X)

println(s"$test")

val file2:String = "/Users/cd/Projects/scala/au.id.cxd.math/data/test_sparrows.csv"
val mat2 = MatrixReader.readFileAt(file2)

val data2 = mat2(::, 1 to 5).toDenseMatrix

val X2 = StandardisedNormalisation().transform(data2)
val test2 = MardiaTest(0.05, X2)

println(test2.toString)
