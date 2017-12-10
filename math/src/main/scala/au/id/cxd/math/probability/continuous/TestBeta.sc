import au.id.cxd.math.probability.continuous.Beta

val betaDist = Beta(1.0,1.0)

/**
> qbeta(0.5,1,1)
[1] 0.5

  **/
betaDist.invcdf(0.5)



