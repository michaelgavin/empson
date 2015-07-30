#' Data - EEBO word-context matrix
#' 
#' A word-context matrix drawn from the EEBO-TCP collection, built from all publicly
#' available files with publication years between 1640 and 1699.
#' 
#' @format A matrix with 28,235 rows and 1,751 columns.
#' 
#' @examples
#' # Load the matrix, then analyze a term.
#' data(eebo)
#' find_similar(mat = eebo, keyword = "rights")
"eebo"