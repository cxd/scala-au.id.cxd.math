import java.io.File

import au.id.cxd.math.data.SequenceReader

val fileName = "/Users/cd/Projects/scala/au.id.cxd.math/src/test/data/example_train_data.csv"
val file = new File(fileName)
println("File Path: " + file.getAbsolutePath)
val reader = SequenceReader()
val data = reader.readSequences(file)