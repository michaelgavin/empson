#' Return the signatures from a matrix
#' 
#' Prints the sorted values from a term-document or word-context matrix
#' 
#' @param mat A term-document or word-context matrix.
#'           
#' @param keyword The term you are evaluating, either a keyword or document 
#'                name.
#' 
#' @param margin Numeric value: 1 or 2. If 1, calculations are performed over the rows. If 2, over
#'              the columns.
#'              
#' @param fullResults Logical value. Default is false.
#' 
#'                 
#' @section What it does:
#' Sorts and prints the most frequent collocates for words and documents. In a term-
#' document matrix, it will return either the word counts for a document or the top
#' documents for a given word.
#' 
#' In a word-context matrix, the relationship is similar. Selecting \code{margin = 2}
#' will find the keyword's column, then return the word counts of the concordance for
#' that term (assuming that it was among the keywords used to build the matrix). 
#' Selecting \code{margin = 1} will return the keyword's row, showing in which
#' concordances it had the highest representation.
#' 
#' For comparison, see the example of 'rights' given below.
#' 
#' @return If \code{fullResults} is true, all results are included in a full-length vector. If
#'         false, only the 12 top words will display.
#' 
#' @examples
#' # For most frequent collocates in a word-context matrix
#' data(eebo)
#' sig(mat = eebo, keyword = "rights", margin = 1)
#' sig(eebo, "rights", margin = 2)
#' 
#' # For most frequent words in a document
#' data(shakespeare)
#' sig(mat = shakespeare, vec = "TN", margin = 2)
#' sig(shakespeare, "wife", margin = 1)
#' 
#' @export
sig = function(mat, keyword, margin = 1, fullResults = F) {
  if (class(mat) == "docMatrix") {
    mat <- mat@mat
  }
  
  if (margin == 1 && keyword %in% row.names(mat) == F) {
    stop("There isn't a row in your matrix for that keyword. No signature found.")
  }
  
  if (margin == 2 && keyword %in% colnames(mat) == F){
    stop("There isn't a column in your matrix for that keyword or document. No signature found.")
  }
  
  if (margin == 1) { 
    results = mat[keyword,]  
  }
  if (margin == 2) { 
    results = mat[,keyword] 
  }
  if (fullResults == F) {
    results = sort(results, decreasing = T)[1:12] 
  } 
  return(results)
}