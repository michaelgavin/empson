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
#' 
#' \strong{Basic operations:}
#' \itemize{
#'   \item \code{\link{buildMatrix}}: Converts documents to matrix format
#'   \item \code{\link{similarity}}: Identifies semantic relationships among terms
#'   \item \code{\link{similarity_map}}: Maps conceptual structures
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
#' @import ggplot2
NULL

