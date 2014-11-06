import au.id.cxd.math.probability
import au.id.cxd.math.probability.TchebysheffInequality
import au.id.cxd.math.probability.continuous.{Gamma, Normal}

val mu = 20.0
val std = 2.0
val inequality = TchebysheffInequality(mu)(std)
val p = inequality.pdf(16, 24)
//
"Probability " + p
inequality.pdfLower(16.0)
inequality.pdfUpper(24.0)
inequality.pdfUpper(28.0)
inequality.pdfLower(5.0)
inequality.pdfUpper(5.0)


