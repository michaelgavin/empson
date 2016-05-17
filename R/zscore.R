#' Return z-score for a keyword
#' 
#' Return z-score for a keyword in a word-context matrix
#' 
#' @param mat A word-context matrix
#' 
#' @param term The word you are evaluating
#' 
#' @param margin The direction of comparison. If "1", zscore will be computed
#'              for a row. If "2", zscore will be computed over a column.
#'              
#' @export
zscore = function(mat, term, margin = 1) {
  if (margin == 1) {
    avg = apply(mat, 2, mean)
    dev = apply(mat, 2, sd)
    score = (mat[term,] - avg) / dev
    return(score)
  }
  if (margin == 2) {
    avg = apply(mat, 1, mean)
    dev = apply(mat, 1, sd)
    score = (mat[,term] - avg) / dev
    return(score)
  }
}
