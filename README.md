scala-au.id.cxd.math
====================

Libraries containing math related functions in scala. 

Provides probability distributions (similar to Breeze), and other utilities for analysis.

Further details are available at the "github pages" branch for the project located at:

[au.id.cxd.math](https://cxd.github.io/scala-au.id.cxd.math/)


### A library experimenting with statistics.

This library is a hobby project, and is only occasionally worked on. It serves several purposes for me.

 - To gradually document a variety of ideas I'm learning in relation to probability and statistics.
 
 - To provide a sandpit to experiment with those ideas and to get to grips with the mathematics behind those ideas.
 
 - To experiment with implementing those ideas in a software library form.

Part of this is experimenting with library design, or to simply attempt an implementation of an algorithm in order to get to grips with the mechanics behind it. Alot of it is reading, reading other material, attempting to unravel and capture some form of it. 

 The library will change over time, and progress will be limited by other activities. This library is not intended as a stable production library, and is experimental in nature.
 
If you're cloning it and modifying it, it'd be great to receive constructive feedback, so let me know (there seems to be alot of regular clones of the repo). 
 
It's not necessarily going to be as robust as other tools with large numbers of maintainers, so as its only a single maintainer experimenting with these methods, you may need to experimentally compare outcomes with other more mature projects. The accuracy of this project is likely to have a high margin of error in comparison to tools such as GSL, R, SciPy, Scalalab or Matlab.

## Building

The build process can be achieved via:

```
sbt compile package publishLocal
```

This gets around an issue with finding dependencies between projects during
the cross compile phase. Then to produce the cross compiled assemblies.

```
sbt +assembly
```

Note the build process is currently dependent on jdk8. 
Using jenv is a good way to manage multiple java versions on a single environment.

```
jenv versions
jenv local 1.8
```

### Documentation.

The documentation consists both of [API documentation](https://cxd.github.io/scala-au.id.cxd.math/latest/math/api/index.html) and some of this information is extracted into separate notes and examples.

### Notes on Regression.

Notes on [Linear Regression](https://cxd.github.io/scala-au.id.cxd.math/notes/linearregression.html)

Notes on [Logistic Regression](https://cxd.github.io/scala-au.id.cxd.math/notes/logisticregression.html)

Notes on [Bayesian Linear Regression](https://cxd.github.io/scala-au.id.cxd.math/notes/bayesianlinearregression.html)

Notes on [Bayesian Regression applied to estimation of Project durations](https://cxd.github.io/scala-au.id.cxd.math/notes/bayesianestimation.html)

### Notes Inference.

Notes on [ANOVA](https://cxd.github.io/scala-au.id.cxd.math/notes/anova.html)

Notes on [MANOVA](https://cxd.github.io/scala-au.id.cxd.math/notes/manova.html)

[Testing assumptions of Multivariate Normality](https://cxd.github.io/scala-au.id.cxd.math/notes/mvn_testing.html)

## Notes on Multivariate Methods

[Canonical Discriminant Analysis](https://cxd.github.io/scala-au.id.cxd.math/notes/canonicaldiscriminantfunc.html)

[Quadratic Discriminant Functions](https://cxd.github.io/scala-au.id.cxd.math/notes/quadraticdiscriminantfunction.html)

### Notes Sequential Learning.

Notes on [Hidden Markov Model](https://cxd.github.io/scala-au.id.cxd.math/notes/hmm.html)

### Text Processing.

Notes on [Singular Value Decomposition and Latent Semantic Indexing](https://cxd.github.io/scala-au.id.cxd.math/notes/latentsemanticindex.html)

