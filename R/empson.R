#' empson: Create vector-space models from \code{tei2r}
#' 
#' The \code{empson} package builds on \code{tei2r} to create matrix 
#' representations of document collections.
#' 
#' @docType package
#' @name empson
#' @author Author:
#' Michael Gavin
#' 
#' @section Tutorial:
#' To get started with \code{empson}, follow the tutorial at \code{\link{Tutorial: empson}}.
#' 
#' \strong{Basic operations:}
#' \itemize{
#'   \item \code{\link{buildMatrix}}: Converts documents to matrix format
#'   \item \code{\link{find_similar}}: Identifies semantic relationships among terms
#'   \item \code{\link{graph_context}}: Maps conceptual structures
#' }
#' 
#' \strong{Sample Data:}
#' \itemize{
#'   \item \code{\link{eebo}}: A large matrix built from the EEBO-TCP corpus, representing
#'                             all texts from 1640 to 1699.
#' }
#' 
#' 
#' @import tei2r
NULL

