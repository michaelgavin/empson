#' Normalize a vector in a matrix
#' 
#' Return normalized vector in a word-context or term-document matrix
#' 
#' @param mat A word-context matrix
#' 
#' @param term The word you are evaluating
#' 
#' @param margin The direction of comparison. If "1", normalization will be computed
#'              for a row. If "2", normalization will be computed over a column.
#'              
#'               
#' @export
normalize <- function(x, margin = 1) {
  if (class(x) == "numeric") {
    results = x / sum(sqrt(x^2)) 
  }
  if (class(x) == "matrix") {
    results = apply(x, margin, function(i) i / sum(sqrt(i^2)))
    if (margin == 1) results = t(results)
    rownames(results) = rownames(x)
    colnames(results) = colnames(x)
  }
  return(results)
}
