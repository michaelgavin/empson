#' Data - EEBO term-document matrix
#' 
#' A word-context matrix drawn from the EEBO-TCP collection, built from all publicly
#' available files with publication years between 1640 and 1699.
#' 
#' @format A matrix with 2,002 rows and 18,311 columns.
#' 
#' @examples
#' # Load the matrix, then analyze a term.
#' data(deebo)
#' similarity(mat = eebo, keyword = "A48901")
"deebo"