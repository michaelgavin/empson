#' Data - Shakespeare (term-document matrix)
#' 
#' A term-document matrix drawn from the Folger Digital Texts
#' editions of William Shakespeare's works.
#' 
#' \link[=http://www.folgerdigitaltexts.org/]{Folger Digital Texts}
#' 
#' @format A matrix with 5,000 rows and 500 columns.
#' 
#' @examples
#' # Load the matrix, then analyze a term.
#' data(shakespeare)
#' similarity(mat = shakespeare, vec = "TN", margin = 2)
"shakespeare"