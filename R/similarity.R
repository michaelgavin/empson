#' Measure similarities over matrices
#' 
#' Measures similarities over term-document and word-context matrices.
#' 
#' @param mat A term-document or word-context matrix.
#'           
#' @param vec The vector or term you are evaluating. If you enter a single term (typically a 
#'           keyword or document name), that term must be entered with quotation marks. If 
#'           you have defined a new vector to analyze, enter the name of the vector without 
#'           quotes.
#' 
#' @param method A character string: 'cosine', 'euclidean', 'pearson' or 'covariance', which names
#'              the mathematical similarity test to be performed. Default is 'cosine'. The most
#'              common other method is 'euclidean'.
#' 
#' @param margin Numeric value: 1 or 2. If 1, calculations are performed over the rows. If 2, over
#'              the columns.
#'              
#' @param fullResults Logical value. Default is false.
#' 
#' @param threshold Numeric value: 0 to 100. Default is 0. 
#'                 
#' @section What it does:
#' Each one of these similarity measurements captures slightly different relationships
#' and will generate somewhat different output. In general, cosine similarity and Pearson 
#' correlations (which are very similar functions) are best for estimating synonyms. Covariance
#' and Euclidean distance tend to find more various kinds of relationships. Keep in mind that
#' \strong{the relationship between semantic similarity measures and qualitative assumptions
#' about word meaning remains underdetermined in the research.} Identifying if and how similarity
#' scores can map contours of meaning in a document collection should be considered a question
#' not yet answered. \code{empson} was designed to help humanists think this problem through. The
#' statistical tests included here were chosen for their simplicity.
#' 
#' 
#' When working with a term-document matrix, selecting \code{margin = 1} will find similarity of 
#' words, and \code{margin = 2} will find similar documents. When working with a word-context
#' matrix, \code{margin = 2} will read across the columns, and so will be limited only to those
#' words for which \code{empson} built concordances.
#' 
#' The \code{threshold} parameter filters out low-frequency words for human reading, limiting 
#' results to the frequency percentile of the threshold. For example, if threshold = 50, only 
#' words with above-average frequency will be included in the displayed results. If set to 0, there 
#' is no threshold and all results are returned.  Raising the threshold to 90 or 95 will limit 
#' results to only higher-frequency words. Often this is desirable if you're looking for human-
#' readable output, though it's worth keeping in mind that the conceptual relations at play
#' among high-frequency and low-frequency terms are underdetermined. Filtering out low-frequency
#' words often 'improves' the outputs of topic models and similarity measurements, in that it 
#' restricts the output to words people use often enough to feel comfortable interpreting across
#' contexts. Whether that comfort is trustworthy or misleading is not known.
#' 
#' @return If \code{fullResults} is true, all results are included in a full-length vector. If
#'         false, only the 12 most similar terms with frequency above the threshold will
#'         be displayed. (In general, include the full results when you plan to use the
#'         vector for further evaluation. Display partial results when you're just 
#'         glancing over the top hits.)
#' 
#' @examples
#' # For most similar words in a word-context matrix
#' data(eebo)
#' similarity(mat = eebo, vec = "rights") 
#' similarity(mat = eebo, vec = "rights", threshold = 95)
#' similarity(mat = eebo, vec = "rights", method = "euclidean")
#' 
#' # For most similar words of a composite vector
#' compvec = eebo["mind",] - eebo["soul",]
#' similarity(mat = eebo, vec = compvec)
#' 
#' # For most similar documents in a term-document matrix
#' data(shakespeare)
#' similarity(mat = shakespeare, vec = "TN", margin = 2)
#' 
#' # For full results
#' similarity(mat = eebo, vec = "rights", fullResults = TRUE)
#' 
#' @export
similarity = function(mat, vec, method = "cosine", margin = 1, fullResults = F, threshold = 0) {
  cos_sim = function(x,y) { x %*% y / (sqrt(x%*%x) * sqrt(y%*%y)) }
  euc_dist = function(x, y) { sqrt(sum((x - y) ^ 2)) }
  
  if (class(mat) == "docMatrix") {
    mat <- mat@mat
  }
  
  if (length(vec) %in% dim(mat) == F) {
    keyword = vec
    if (margin == 1) { 
      if (keyword %in% row.names(mat) == F) stop("Your keyword doesn't match any of your matrix's row names.")
      vec = mat[keyword,] 
    }
    if (margin == 2) { 
      if (keyword %in% colnames(mat) == F) stop("Your keyword doesn't match any of your matrix's column names.")
      vec = mat[,keyword] 
    }
  }
  
  if (margin == 1 && threshold != 0) {
    totals = apply(mat,margin,sum)
    pts = quantile(totals, probs = threshold/100)
    mat = mat[which(totals > pts),]
  }
  
  if (method %in% c("cosine", "euclidean", "covariance", "pearson") == F) {
    stop("Method must be specified as 'cosine', 'euclidean', 'covariance', or 'pearson'.")
  }
  if (method == "cosine") {
    results = apply(mat, margin, cos_sim, vec)
  }
  if (method == "euclidean") {
    results = apply(mat, margin, euc_dist, vec)
  }
  if (method == "covariance") {
    results = apply(mat, margin, cov, vec)
  }
  if (method == "pearson") {
    results = suppressWarnings(apply(mat, margin, cor, vec))
  }
  
  if (fullResults == F) {
    if (length(vec) == 1) { 
      results = results[-which(names(results) == vec)] 
    }
    if (method == "euclidean") {
      results = sort(results)[1:12]
    } else {
      results = sort(results, decreasing = T)[1:12]
    }
  }
  return(results)
}
