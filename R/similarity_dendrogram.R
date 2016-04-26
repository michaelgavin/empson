#' Create a graph of similar words
#' 
#' Build a scatterplot that represents the conceptual structure of a keyword.
#' 
#' @param mat A word-context matrix.
#' 
#' @param keyword A string.
#' 
#' @param method A character string: 'cosine', 'euclidean', 'pearson' or 'covariance', which names
#'              the mathematical similarity test to be performed. Default is 'cosine'.
#' 
#' @param margin Numeric value: 1 or 2. If 1, calculations are performed over the rows. If 2, over
#'              the columns.
#'              
#' @param threshold Numeric value: 0 to 100. Default is 50.
#' 
#' @param numResults Numeric value. The number of words to be displayed in the 
#'                   graph. Default is 40.
#'  
#' @param numGrps Numeric value. The number of groups in which you'd like to divide
#'                the display. Default is 5.
#'       
#' @return A scatterplot showing structure of 30 most-similar words.
#' 
#' @section What it does:
#' This function runs \code{\link{similarity}} over a word-context matrix and looks for the
#' thirty most similar terms, then clusters them.
#' @export
similarity_dendrogram = function(mat, 
                            keyword, 
                            method = "cosine", 
                            margin = 1, 
                            threshold = 50, 
                            numResults = 30, 
                            numGrps = 5) {
  if (class(mat) == "docMatrix") {
    mat = mat@mat
  }
  if (method %in% c("cosine", "euclidean", "pearson", "covariance") == F) {
    stop("The 'method' argument must be deployed: 'cosine', 'euclidean', 'pearson', 'covariance'.")
  }
  
  # Get words most similar to keyword
  results = similarity(mat = mat, vec = keyword, method = method, margin = margin, threshold = threshold, fullResults = T)
  if (method == "euclidean") {
    results = sort(results)[1:numResults]
  } else {
    results = sort(results, decreasing = T)[1:numResults]
  }
  words = names(results)
  
  corr_method = function(x,y) { x %*% y / (sqrt(x%*%x) * sqrt(y%*%y)) }
  
  # Now build a correlations matrix among the words
  mat = mat[words, ]
  correlations = matrix(0, length(words), length(words))
  for (i in 1:nrow(correlations)) {
    correlations[i, ] = apply(mat, 1, corr_method, mat[i, ])
  }
  rownames(correlations) = words

  hc = hclust(dist(correlations))
  plot(as.dendrogram(hc, hang = 0.02), horiz = T, main = paste(keyword, " (", method, ")"))
}