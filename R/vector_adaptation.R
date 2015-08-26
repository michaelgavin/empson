#' Vector adaptation
#' 
#' Combines word vectors using either addition or component-wise multiplication
#' then returns the 20 most similar terms.
#' 
#' @param mat A word-context matrix (format either matrix or \code{docMatrix}).
#' 
#' @param positive A character vector of terms naming the rows to be computed.
#' 
#' @param result A string. If "analogies" then it returns similar terms, just like find_analogies.
#'                         if "vector" will return the centroid vector.
#' 
#' @param operation A character vector (either "+" or "*"), that controls whether the vectors
#'                 will be added as simple sums or as products. Default is "*".
#' 
#' @param sorted A logical value.
#' 
#' @param exclude A logical value. If TRUE, the computed terms ('positive' and 'negative') will
#'               be excluded from the results.
#'
#' 
#' @examples
#' data(eebo)
#' vector_adaptation(mat = eebo, positive = c("wit", "sense"))
#' 
#' @export
vector_adaptation = function (mat, positive = NULL, result = "analogies", operation = "*", sorted = T, exclude = T) {
  if (class(positive) == "NULL") {
    stop("You must define at least one positive value.")
  }
  if (class(mat) == "docMatrix") {
    mat <- mat@mat
  }
  if (length(positive) > 1) {
    vec = positive[which(positive %in% rownames(mat))]
    if (operation == "*") {
      vec = apply(mat[vec, ], 2, prod)
    }
    if (operation == "+") {
      vec = apply(mat[vec, ], 2, sum)
      #vec = vec / length(vec) To switch back to 'centroid' function
    }
  }
  else {
    vec = mat[positive, ]
  }
  if (result == "vector") {
    return(vec)
  } else {
    results = apply(mat[names(vec), ], 2, cor, vec)
    if (exclude == T) {
      ex = which(names(results) %in% positive)
      if (length(ex) > 0) {
        results = results[-ex]
      }
    }
    if (sorted == T) {
      results = sort(results, decreasing = T)[1:20]
    }
    return(results)
  }
}

