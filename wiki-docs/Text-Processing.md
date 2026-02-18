# Text Processing

Comprehensive guide to Natural Language Processing (NLP) and text analysis with Latent Semantic Indexing.

## Overview

The library provides tools for processing and analyzing text documents, including:

- **Document-Term Matrix** - Convert text to numerical representations
- **TF-IDF Weighting** - Term frequency-inverse document frequency
- **Latent Semantic Indexing (LSI)** - Dimensionality reduction for text using SVD
- **Text Preprocessing** - Stopword removal, stemming, tokenization
- **Document Similarity** - Cosine similarity in semantic space
- **Model Persistence** - Save and load LSI models

**Package**: `au.id.cxd.text`

## Quick Start

### Basic LSI Workflow

```scala
import au.id.cxd.text.model.LatentSemanticIndex
import au.id.cxd.text.count.DocumentTermVectoriser

// 1. Prepare documents
val documents = Seq(
  "The quick brown fox jumps over the lazy dog",
  "The dog was sleeping under the tree",
  "The brown cat climbed the tall tree"
)

// 2. Create document-term matrix
val vectoriser = DocumentTermVectoriser()
val (matrix, dictionary) = vectoriser.makeDocTermMatrix(documents)

// 3. Build LSI model (k = number of latent dimensions)
val lsi = LatentSemanticIndex(matrix, k = 2)

// 4. Transform documents to semantic space
val transformed = lsi.transform(matrix)

println(s"Original dimensions: ${matrix.cols}")
println(s"Reduced dimensions: ${transformed.cols}")
```

## Document-Term Matrix

Convert text documents into numerical matrix representation.

### Creating Document-Term Matrix

**Package**: `au.id.cxd.text.count.DocumentTermVectoriser`

```scala
import au.id.cxd.text.count.DocumentTermVectoriser
import breeze.linalg._

val documents = Seq(
  "machine learning is amazing",
  "deep learning uses neural networks",
  "machine learning includes neural networks"
)

// Create vectoriser
val vectoriser = DocumentTermVectoriser()

// Generate document-term matrix
val (termIndexMap, docTermMatrix) = vectoriser.count(
  documents.map(_.split("\\s+"))
)

println(s"Matrix shape: ${docTermMatrix.rows} documents x ${docTermMatrix.cols} terms")
println(s"Document-term matrix:\n$docTermMatrix")

// Term index map shows which column each term occupies
termIndexMap.foreach { case (colIndex, (term, hashCode, docCount, termCount)) =>
  println(f"Column $colIndex: '$term' (appears in $docCount docs, $termCount total)")
}
```

### Understanding the Matrix

```scala
// Each row is a document
// Each column is a term
// Values are term frequencies or TF-IDF scores

val (termMap, matrix) = vectoriser.count(docs.map(_.split("\\s+")))

// Access specific document
val doc0 = matrix(0, ::)
println(s"Document 0 vector: $doc0")

// Access specific term across all documents
val term0 = matrix(::, 0)
println(s"Term 0 across documents: $term0")

// Find non-zero terms in a document
val nonZeroIndices = doc0.findAll(_ > 0)
println(s"Non-zero terms in doc 0: $nonZeroIndices")
```

## TF-IDF Weighting

Term Frequency-Inverse Document Frequency gives more weight to important terms.

### TF-IDF Basics

**Package**: `au.id.cxd.text.count.TfIdfCount`

The library uses augmented frequency to prevent bias towards longer documents:

$$
tf(t, d) = 0.5 + 0.5 \times \frac{f_{t,d}}{\max(f_{t',d} : t' \in d)}
$$

Inverse document frequency:

$$
idf(t, D) = \log\left[\frac{N}{|d \in D : t \in d|}\right]
$$

```scala
import au.id.cxd.text.count.TfIdfCount
import breeze.linalg._

val documents = Seq(
  "data science is awesome",
  "machine learning is part of data science",
  "deep learning is part of machine learning"
)

// Create TF-IDF vectoriser
val tfidf = TfIdfCount()

// Generate TF-IDF matrix
val (termMap, tfidfMatrix) = tfidf.count(
  documents.map(_.split("\\s+"))
)

println("TF-IDF Matrix:")
println(tfidfMatrix)

// Terms appearing in all documents get lower weights
// Terms unique to few documents get higher weights
```

### TF-IDF Properties

```scala
val tfidf = TfIdfCount()
val (termMap, matrix) = tfidf.count(docs.map(_.split("\\s+")))

// Examine term importance
termMap.foreach { case (col, (term, hash, docCount, termCount)) =>
  val avgTfidf = sum(matrix(::, col)) / matrix.rows
  println(f"'$term': avg TF-IDF = $avgTfidf%.4f (in $docCount docs)")
}

// Common words across documents have low TF-IDF
// Rare but important words have high TF-IDF
```

## Latent Semantic Indexing (LSI)

LSI uses Singular Value Decomposition (SVD) to discover latent semantic structure in documents.

### Building LSI Model

**Package**: `au.id.cxd.text.model.LatentSemanticIndex`

```scala
import au.id.cxd.text.model.LatentSemanticIndex
import au.id.cxd.text.count.TfIdfCount
import breeze.linalg._

// Prepare documents
val documents = Seq(
  "cat dog pet animal",
  "kitten puppy pet",
  "car truck vehicle",
  "automobile bus vehicle",
  "feline canine animal"
)

// Create TF-IDF matrix
val tfidf = TfIdfCount()
val (termMap, docTermMatrix) = tfidf.count(
  documents.map(_.split("\\s+"))
)

// Build LSI model with k latent dimensions
val k = 2  // Number of topics/dimensions
val lsi = LatentSemanticIndex(docTermMatrix, k)

println(s"Original dimensions: ${docTermMatrix.cols} terms")
println(s"Reduced dimensions: $k latent topics")
```

### Understanding LSI Components

```scala
val lsi = LatentSemanticIndex(docTermMatrix, k = 3)

// SVD decomposition: A = U * Σ * V^T
val U = lsi.svD.U          // Document-topic matrix
val S = lsi.svD.S          // Singular values (topic importance)
val V = lsi.svD.Vt.t       // Term-topic matrix

println(s"U shape: ${U.rows} docs x ${U.cols} topics")
println(s"Singular values: $S")
println(s"V shape: ${V.rows} terms x ${V.cols} topics")

// Singular values indicate topic importance
val totalVar = sum(S.map(x => x * x))
val varExplained = S.map(s => (s * s) / totalVar)
println(s"Variance explained by each topic: $varExplained")
```

### Transforming Documents

```scala
val lsi = LatentSemanticIndex(docTermMatrix, k = 2)

// Transform documents to semantic space
val docsInSemanticSpace = lsi.transform(docTermMatrix)

println("Documents in semantic space:")
println(docsInSemanticSpace)

// Each row is now a document in k-dimensional semantic space
// Documents with similar meaning are close together
```

### Querying with LSI

```scala
import au.id.cxd.text.count.TfIdfCount

val tfidf = TfIdfCount()
val (termMap, matrix) = tfidf.count(docs.map(_.split("\\s+")))
val lsi = LatentSemanticIndex(matrix, k = 3)

// Transform a new query
val query = "cat and dog".split("\\s+")
val queryVector = tfidf.countQuery(query, lsi)
val queryInSemanticSpace = lsi.transform(queryVector)

println(s"Query in semantic space: $queryInSemanticSpace")

// Compare query to documents using cosine similarity
import au.id.cxd.math.function.distance.Cosine

val docsTransformed = lsi.transform(matrix)
val similarities = (0 until docsTransformed.rows).map { i =>
  val docVec = docsTransformed(i, ::).t
  val similarity = 1.0 - Cosine().measure(queryInSemanticSpace, docVec)
  (i, similarity)
}

// Sort by similarity
val ranked = similarities.sortBy(-_._2)
println("Most similar documents:")
ranked.take(3).foreach { case (docIdx, sim) =>
  println(f"  Document $docIdx: similarity = $sim%.4f")
  println(f"    ${docs(docIdx)}")
}
```

## Document Similarity

Calculate similarity between documents in semantic space.

### Cosine Similarity

```scala
import au.id.cxd.math.function.distance.{Cosine, CosineDistance}
import breeze.linalg._

val lsi = LatentSemanticIndex(matrix, k = 5)
val transformed = lsi.transform(matrix)

// Compare two documents
val doc1 = transformed(0, ::).t
val doc2 = transformed(1, ::).t

val cosine = Cosine()
val distance = cosine.measure(doc1, doc2)
val similarity = 1.0 - distance

println(f"Cosine similarity: $similarity%.4f")
println(f"Cosine distance: $distance%.4f")

// Similarity close to 1 = very similar
// Similarity close to 0 = not similar
```

### Similarity Matrix

```scala
import breeze.linalg._

// Compute all pairwise similarities
val n = transformed.rows
val simMatrix = DenseMatrix.zeros[Double](n, n)

val cosine = Cosine()
for (i <- 0 until n; j <- i until n) {
  val sim = 1.0 - cosine.measure(transformed(i, ::).t, transformed(j, ::).t)
  simMatrix(i, j) = sim
  simMatrix(j, i) = sim
}

println("Document similarity matrix:")
println(simMatrix)

// Find most similar documents
for (i <- 0 until n) {
  val similarities = (0 until n).filter(_ != i).map(j => (j, simMatrix(i, j)))
  val mostSimilar = similarities.maxBy(_._2)
  println(f"Doc $i most similar to Doc ${mostSimilar._1} (sim = ${mostSimilar._2}%.3f)")
}
```

## Text Preprocessing

Clean and normalize text before analysis.

### Stopword Removal

**Package**: `au.id.cxd.text.preprocess.StopwordPatternFilter`

```scala
import au.id.cxd.text.preprocess.StopwordPatternFilter
import au.id.cxd.text.helpers.EmbeddedStopwordsLoader

// Load common English stopwords
val stopwords = EmbeddedStopwordsLoader.load()

println(s"Loaded ${stopwords.length} stopwords")
println(s"Sample stopwords: ${stopwords.take(10).mkString(", ")}")

// Create filter
val filter = StopwordPatternFilter(stopwords)

// Filter text
val text = "The quick brown fox jumps over the lazy dog"
val tokens = filter.tokenise(text)

println(s"Original: $text")
println(s"After stopword removal: ${tokens.mkString(" ")}")
// Output: "quick brown fox jumps lazy dog"
```

### Stemming

**Package**: `au.id.cxd.text.preprocess.PorterStemmer`

Reduce words to their root form using the Porter Stemming Algorithm.

```scala
import au.id.cxd.text.preprocess.PorterStemmer

val stemmer = PorterStemmer()

// Stem individual words
val words = Array("running", "runs", "ran", "runner")
val stemmed = words.map(stemmer.stem)

println("Original -> Stemmed:")
words.zip(stemmed).foreach { case (orig, stem) =>
  println(s"  $orig -> $stem")
}

// Typical output:
// running -> run
// runs -> run
// ran -> ran
// runner -> runner
```

### Combined Preprocessing

```scala
import au.id.cxd.text.preprocess.{StopwordPatternFilter, StemmingPatternFilter}
import au.id.cxd.text.helpers.EmbeddedStopwordsLoader

val stopwords = EmbeddedStopwordsLoader.load()

// Create preprocessing pipeline
val stopwordFilter = StopwordPatternFilter(stopwords)
val stemFilter = StemmingPatternFilter()

val text = "The running foxes are quickly jumping over the sleeping dogs"

// Step 1: Remove stopwords
val tokens = stopwordFilter.tokenise(text)

// Step 2: Apply stemming
val processed = stemFilter.tokeniseQuery(tokens)

println(s"Original: $text")
println(s"Processed: ${processed.mkString(" ")}")
// Output: "run fox quick jump sleep dog"
```

### Custom Filters

```scala
import au.id.cxd.text.preprocess.LinePatternFilter

// Custom tokenization pattern
val customFilter = new LinePatternFilter(pattern = """[\s,;:]+""")

val text = "word1,word2;word3:word4"
val tokens = customFilter.tokenise(text)

println(s"Tokens: ${tokens.mkString(", ")}")
// Output: "word1, word2, word3, word4"
```

## Model Persistence

Save and load LSI models for reuse.

### Saving LSI Model

**Package**: `au.id.cxd.text.model.LatentSemanticIndex`

```scala
import au.id.cxd.text.model.LatentSemanticIndex
import java.io.File

// Build LSI model
val lsi = LatentSemanticIndex(matrix, k = 10)

// Save to file
val outputFile = new File("lsi_model.zip")
LatentSemanticIndex.write(lsi, outputFile)

println(s"Model saved to ${outputFile.getName}")
```

### Loading LSI Model

```scala
import au.id.cxd.text.model.LatentSemanticIndex
import java.io.File

// Load from file
val modelFile = new File("lsi_model.zip")
val lsi = LatentSemanticIndex.read(modelFile)

println(s"Model loaded successfully")
println(s"Number of documents: ${lsi.docIdMap.size}")
println(s"Number of terms: ${lsi.colTermMap.size}")
println(s"Latent dimensions: ${lsi.svD.S.length}")

// Use loaded model
val transformed = lsi.transform(lsi.tfIdf)
```

## Complete Workflow

### End-to-End Document Analysis

```scala
import au.id.cxd.text.model.LatentSemanticIndex
import au.id.cxd.text.count.TfIdfCount
import au.id.cxd.text.preprocess.{StopwordPatternFilter, StemmingPatternFilter}
import au.id.cxd.text.helpers.EmbeddedStopwordsLoader
import au.id.cxd.math.function.distance.Cosine
import breeze.linalg._
import java.io.File

// 1. Load and preprocess documents
val rawDocuments = Seq(
  "Machine learning is a subset of artificial intelligence",
  "Deep learning uses neural networks with multiple layers",
  "Natural language processing deals with text data",
  "Computer vision focuses on image recognition",
  "Reinforcement learning involves agents and rewards"
)

val stopwords = EmbeddedStopwordsLoader.load()
val stopwordFilter = StopwordPatternFilter(stopwords)
val stemFilter = StemmingPatternFilter()

// Preprocess: lowercase, remove stopwords, stem
val preprocessed = rawDocuments.map { doc =>
  val tokens = stopwordFilter.tokenise(doc.toLowerCase)
  stemFilter.tokeniseQuery(tokens)
}

println(s"Preprocessed ${preprocessed.length} documents")

// 2. Create TF-IDF matrix
val tfidf = TfIdfCount()
val (termMap, tfidfMatrix) = tfidf.count(preprocessed)

println(s"TF-IDF matrix: ${tfidfMatrix.rows} docs x ${tfidfMatrix.cols} terms")

// 3. Build LSI model
val k = 3  // Number of topics
val lsi = LatentSemanticIndex(tfidfMatrix, k)

// Examine topics
println(s"\nSingular values (topic importance): ${lsi.svD.S}")

// 4. Transform to semantic space
val semanticSpace = lsi.transform(tfidfMatrix)

println(s"\nDocuments in semantic space:")
println(semanticSpace)

// 5. Query the model
val query = "neural network deep learning".split("\\s+")
val stemmedQuery = stemFilter.tokeniseQuery(query)
val queryVector = tfidf.countQuery(stemmedQuery, lsi)
val queryTransformed = lsi.transform(queryVector)

// 6. Find similar documents
val cosine = Cosine()
val similarities = (0 until semanticSpace.rows).map { i =>
  val docVec = semanticSpace(i, ::).t
  val sim = 1.0 - cosine.measure(queryTransformed, docVec)
  (i, sim)
}.sortBy(-_._2)

println(s"\nQuery: '${query.mkString(" ")}'")
println("Most similar documents:")
similarities.take(3).foreach { case (idx, sim) =>
  println(f"  [$idx] similarity=$sim%.4f: ${rawDocuments(idx)}")
}

// 7. Save model for later use
val modelFile = new File("document_analysis_model.zip")
LatentSemanticIndex.write(lsi, modelFile)
println(s"\nModel saved to ${modelFile.getName}")

// 8. Analyze term-topic relationships
val V = lsi.svD.Vt.t  // Term-topic matrix
println("\nTop terms for each topic:")
for (topic <- 0 until k) {
  val termScores = termMap.map { case (col, (term, _, _, _)) =>
    (term, math.abs(V(col, topic)))
  }.toSeq.sortBy(-_._2).take(5)
  
  println(s"\nTopic $topic:")
  termScores.foreach { case (term, score) =>
    println(f"  $term%.20s : $score%.4f")
  }
}
```

## Advanced Topics

### Multi-Document Clustering

```scala
import au.id.cxd.math.model.cluster.KMeans

val lsi = LatentSemanticIndex(tfidfMatrix, k = 10)
val semanticSpace = lsi.transform(tfidfMatrix)

// Cluster documents in semantic space
val numClusters = 3
val kmeans = KMeans(numClusters, semanticSpace)

println("Document clusters:")
(0 until numClusters).foreach { cluster =>
  val docIndices = kmeans.assignments.zipWithIndex
    .filter(_._1 == cluster)
    .map(_._2)
  
  println(s"\nCluster $cluster (${docIndices.length} documents):")
  docIndices.take(5).foreach { idx =>
    println(f"  [$idx] ${documents(idx).take(60)}...")
  }
}
```

### Batch Processing

```scala
import scala.io.Source

// Process large document collection
def processLargeCorpus(filePath: String): LatentSemanticIndex = {
  val documents = Source.fromFile(filePath)
    .getLines()
    .toSeq
  
  println(s"Processing ${documents.length} documents")
  
  // Preprocess in batches
  val batchSize = 1000
  val allProcessed = documents.grouped(batchSize).flatMap { batch =>
    batch.map { doc =>
      val tokens = stopwordFilter.tokenise(doc.toLowerCase)
      stemFilter.tokeniseQuery(tokens)
    }
  }.toSeq
  
  // Build model
  val tfidf = TfIdfCount()
  val (termMap, matrix) = tfidf.count(allProcessed)
  val lsi = LatentSemanticIndex(matrix, k = 50)
  
  lsi
}
```

### Incremental Updates

While the library doesn't support true incremental LSI, you can:

```scala
// 1. Load existing model
val existingLsi = LatentSemanticIndex.read(new File("model.zip"))

// 2. Combine with new documents
val newDocs = Seq("new document 1", "new document 2")
val allDocs = existingDocs ++ newDocs

// 3. Rebuild model
val tfidf = TfIdfCount()
val (termMap, matrix) = tfidf.count(allDocs.map(_.split("\\s+")))
val updatedLsi = LatentSemanticIndex(matrix, k = 50)

// 4. Save updated model
LatentSemanticIndex.write(updatedLsi, new File("model_updated.zip"))
```

## Best Practices

### Choosing K (Number of Topics)

```scala
// Try different k values
val kValues = Seq(5, 10, 20, 50, 100)

kValues.foreach { k =>
  val lsi = LatentSemanticIndex(matrix, k)
  val singularValues = lsi.svD.S
  
  // Examine singular value decay
  val totalEnergy = sum(singularValues.map(s => s * s))
  val cumEnergy = singularValues.map(s => s * s).toArray.scanLeft(0.0)(_ + _).tail
  val varExplained = cumEnergy.last / totalEnergy
  
  println(f"k=$k: variance explained = ${varExplained * 100}%.2f%%")
}

// Choose k where variance explained reaches 80-90%
```

### Preprocessing Pipeline

Recommended preprocessing steps:

1. **Lowercase** - Normalize case
2. **Remove stopwords** - Filter common words
3. **Stemming** - Reduce to root forms
4. **Remove rare terms** - Terms appearing in < 2 documents
5. **Remove common terms** - Terms appearing in > 90% of documents

```scala
def preprocessDocument(text: String, 
                      stopwords: Seq[String],
                      minDocFreq: Int = 2,
                      maxDocFreq: Double = 0.9): Array[String] = {
  // Lowercase and tokenize
  val tokens = text.toLowerCase.split("\\s+")
  
  // Remove stopwords
  val filtered = tokens.filterNot(token => stopwords.contains(token))
  
  // Stem
  val stemmer = PorterStemmer()
  val stemmed = filtered.map(stemmer.stem)
  
  // Remove very short words
  stemmed.filter(_.length > 2)
}
```

### Memory Management

For large corpora:

```scala
// Process in batches
def processInBatches(documents: Seq[String], batchSize: Int = 1000): LatentSemanticIndex = {
  val batches = documents.grouped(batchSize).toSeq
  
  // Process each batch
  val allTokens = batches.flatMap { batch =>
    batch.map(doc => preprocessDocument(doc, stopwords))
  }
  
  // Build model from all tokens
  val tfidf = TfIdfCount()
  val (termMap, matrix) = tfidf.count(allTokens)
  LatentSemanticIndex(matrix, k = 100)
}
```

## Common Use Cases

### Document Search Engine

```scala
class DocumentSearchEngine(val lsi: LatentSemanticIndex, 
                          val documents: Seq[String]) {
  
  def search(query: String, topK: Int = 5): Seq[(Int, Double, String)] = {
    // Preprocess query
    val queryTokens = preprocessQuery(query)
    val tfidf = TfIdfCount()
    val queryVector = tfidf.countQuery(queryTokens, lsi)
    val queryTransformed = lsi.transform(queryVector)
    
    // Transform all documents
    val docsTransformed = lsi.transform(lsi.tfIdf)
    
    // Calculate similarities
    val cosine = Cosine()
    val results = (0 until documents.length).map { i =>
      val docVec = docsTransformed(i, ::).t
      val similarity = 1.0 - cosine.measure(queryTransformed, docVec)
      (i, similarity, documents(i))
    }
    
    // Return top-k most similar
    results.sortBy(-_._2).take(topK)
  }
}
```

### Document Clustering

```scala
def clusterDocuments(lsi: LatentSemanticIndex, 
                    numClusters: Int): Map[Int, Seq[Int]] = {
  val semanticSpace = lsi.transform(lsi.tfIdf)
  val kmeans = KMeans(numClusters, semanticSpace)
  
  // Group document indices by cluster
  kmeans.assignments.zipWithIndex
    .groupBy(_._1)
    .map { case (cluster, docs) => (cluster, docs.map(_._2)) }
}
```

### Topic Modeling

```scala
def extractTopics(lsi: LatentSemanticIndex, 
                 termsPerTopic: Int = 10): Seq[Seq[String]] = {
  val V = lsi.svD.Vt.t
  val k = V.cols
  
  (0 until k).map { topic =>
    lsi.colTermMap.map { case (col, (term, _, _, _)) =>
      (term, math.abs(V(col, topic)))
    }.toSeq
     .sortBy(-_._2)
     .take(termsPerTopic)
     .map(_._1)
  }
}
```

## Common Pitfalls

- ❌ Not preprocessing text (stopwords, stemming)
- ❌ Using too few latent dimensions (underfitting)
- ❌ Using too many latent dimensions (overfitting)
- ❌ Not handling rare/common terms appropriately
- ❌ Forgetting to normalize query vectors
- ❌ Not saving models for reuse

## See Also

- [Multivariate Analysis](Multivariate-Analysis.md) - SVD and dimensionality reduction
- [Examples Catalog](Examples-Catalog.md) - LSI examples
- [API Quick Reference](API-Quick-Reference.md) - Quick syntax lookup
- [Data Processing](Data-Processing.md) - Data loading and preprocessing

---

[← Back to Home](Home.md)
