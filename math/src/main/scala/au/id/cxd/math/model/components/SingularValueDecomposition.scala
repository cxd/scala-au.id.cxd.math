package au.id.cxd.math.model.components

/**
  * ##import MathJax
  *
  * The singular value decomposition provides a decomposition of a matrix such that
  *
  * $$
  *   A = USV'
  * $$
  *
  * The decomposition is described in Skillicorn 2007, as one which indicates the amount of variation in the
  * latent features of the data set.
  *
  * Several interpretations are explained in the book giving the following views
  *
  * - Factor interpretation
  *   The rows of $V'$ are interpreted as underlying factors. The transformed basis provides a set of new factors
  *   a correlation between the original attributes of A and the rows of V can be constructed in order to determine
  *   the relationship between the original attributes and the factors in V. This relationship may include positive and
  *   negative relationships for multiple attributes with a single factor in $V'$.
  *
  *
  * - Geometric interpretation
  * The interpretation of the components is similar to the interpretation of PCA in this approach.
  * When reviewing the relationship of of objects to attributes the $V'$ matrix can be seen as an orthonormal basis for the matrix $U$
  * Alternately, $U$ also represents an orthonormal basis for the matrix $V'$.
  *
  * This is especially useful for visualisation. For example, coordinates from $U$ can be plotted,
  * these are separated by the directions of the corresponding components in $V'$.
  *
  *
  * - Graphical interpretation
  *
  * The resulting matrices describe the following
  * The matrix $U$ describes the relationship between objects or rows of A, the transpose matrix $V'$ describes the
  * relationship between attributes or columns in $A$ with the rows of $V'$ or columns of $V$ its transpose.
  * The diagonal (or vector) $S$ describes the amount of variation in the respective components which are the columns of $U$
  * and the columns of $V'$.
  * A useful difference between PCA and SVD is that whilst PCA is capable of measuring either the relationship between objects $(A'A)$
  * or the relationship between attributes $AA'$ it can only do so distinctly, and not both at the same time in the single operation.
  * Whilst the SVD can provide the measurements of components for objects and attributes in one operation.
  *
  * Data is considered to be standardised and spherically distributed prior to executing the procedure, due to this
  * an appropriate method of normalisation or standardisation should be performed on the input data set prior to
  * performing the procedure.
  *
  * There are some exceptions to this rule as in the case of matrices of dummy variables for example.
  *
  * The $S$ matrix in the components interpretation provides a measure of the amount of variation each component c
  *
  *
  * References:
  *
  * 1. Skillicorn, D. Understanding Complex Datasets: Data Mining with Matrix Decompositions. Chapman and Hall/CRC 2007
  *
  *
  * Created by cd on 10/07/2016.
  */
class SingularValueDecomposition {

}
