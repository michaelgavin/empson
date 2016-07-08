#' Compose Vectors
#' 
#' Performs vector composition and negation, then returns the 12 most similar terms.
#' 
#' @param mat A term-document or word-context matrix (format either matrix or \code{docMatrix}).
#' 
#' @param positive A character vector of terms naming the rows to be aggregated.
#' 
#' @param operation A character vector (either "+" or "*"), that controls whether the vectors
#'                 will be added as simple sums or as products. Default is "*".
#' 
#' @param fullResults A logical value. If \code{TRUE}, the composite vector will be returned, without
#'                    computing similarity scores. If \code{FALSE}, only terms most similar to the
#'                    composite vector will be returned.
#'                    
#' @param method The calculation to be performed ("cosine", "euclidean", "pearson", or
#'               "covariance").
#' 
#' @param margin A numeric value. If \code{margin = 1} then rows will be computed. \strong{Note
#'               that the current development version of this function only supported computing
#'               over rows, so don't change this parameter.}
#'               
#' @param threshold A numeric value, between 0 and 100. See \code{\link{similarity}} for more 
#'                  details.
#' 
#' @examples
#' data(eebo)
#' compose(mat = eebo, positive = c("wit", "sense"))
#' @export
compose <- function(mat, 
                    positive = "", 
                    negative = "", 
                    operation = "+",
                    method = "cosine", 
                    margin = 1, 
                    threshold = 0)
{
  if (length(positive) == 1 && positive == "") {
    stop("You must define at least one positive value.")
  }
  if (margin != 1) {
    stop("In its current form, vector_adaptation() only works over rows, not columns. Please leave margin == 1.")
  }
  if (class(mat) == "docMatrix") {
    mat <- mat@mat
  }
  if (length(positive) > 1) {
    pos = positive[which(positive %in% rownames(mat))]
    if (operation == "*") {
      pos = apply(mat[pos, ], 2, prod)
    }
    if (operation == "+") {
      pos = apply(mat[pos, ], 2, sum)
    }
  } 
  
  if (length(positive) == 1) {
    pos = mat[positive, ]
  }
  
  if (length(negative) > 1) {
    neg = negative[which(negative %in% rownames(mat))]
    if (operation == "*") {
      neg = apply(mat[neg, ], 2, prod)
    }
    if (operation == "+") {
      neg = apply(mat[neg, ], 2, sum)
    }
  }
  
  if (length(negative) == 1 && negative != "") {
    neg = mat[negative,]
  }
  
  if (length(negative) > 0 && negative != "") {
    if (operation == "*") {
      vec = pos / neg
      vec[which(is.finite(vec) == F)] = 0
    }
    if (operation == "+") {
      vec = pos - neg
    } 
  } else {
    vec = pos
  }
  results = vec
  return(results)
}