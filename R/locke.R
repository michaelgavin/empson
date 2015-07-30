#' Data - Locke's Treatise (word-context matrix)
#' 
#' A word-context matrix drawn from the EEBO-TCP edition of John Locke's
#' Two Treatises of Government (TCP# A48901.)
#' 
#' @format A matrix with 5,000 rows and 500 columns.
#' 
#' @examples
#' # Load the matrix, then analyze a term.
#' data(locke)
#' graph_context(mat = locke, keyword = "woman")
"locke"