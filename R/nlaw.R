#' Data - 17th Cent. Political Theory (word-context matrix)
#' 
#' A \code{docMatrix} object containing a word-context matrix for a sample dataset
#' of 33 17th-century documents relating loosely to political theory.
#' 
#' @format A matrix with 5,000 rows and 500 columns.
#' 
#' @examples
#' # Load the matrix, then analyze a term.
#' data(nlaw)
#' graph_context(mat = nlaw, keyword = "laws")
"nlaw"