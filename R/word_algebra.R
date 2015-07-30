#' Add and subtract word vectors
#' 
#' Performs basic word calculations to find analogous terms using the vector-offset method.
#' 
#' @slot mat A word-context matrix (format either matrix or \code{docMatrix}).
#' 
#' @slot positive A character vector of terms to add.
#' 
#' @slot negative A character vector of terms to subtract.
#' 
#' @slot operation A character vector (either "+" or "*"), that controls whether the vectors
#'                 will be added as simple sums or as products.
#'                 
#' @slot transpose A logical value. If TRUE, the matrix will be transposed and similar terms
#'                 will be found across columns, rather than rows.
#' 
#' @slot exclude A logical value. If TRUE, the computed terms ('positive' and 'negative') will
#'               be excluded from the results.
#'
#' @section What it does:
#' This function attempts to replicate the word-offset method described in Mikolov, 
#' et al. (2013). Generally speaking, it does not work very well, if by "work" we 
#' expect that it will be able to find analogies, such that:
#' \deqn{mat['king',] - mat['man',] ~= mat['queen',] - mat['woman',]}
#' 
#' If the vector off-set method was working properly, we should be able to use these
#' calculations to infer relational similarity between terms. According to Turney and
#' Pantel (2010), it isn't supposed to be possible to infer relational similarity from
#' attribute similarity.
#' 
#' @section Caveat:
#' This function will not produce the desired results unless the underlying matrix
#' is transformed in some way that I do not yet understand. Frankly, working with this
#' has made me less impressed with word2vec.
#' 
#' @examples
#' word_algebra(mat = eebo, positive = "body", negative = "soul")
#' word_algebra(mat = eebo, positive = c("heart", "soul"), negative = "mind")
#' word_algebra(mat = eebo, positive = c("king", "woman"), negative = "queen")
#' 
#' # Results tend to over-emphasize low-frequency terms. This
#' # can sometimes be counteracted by searching for similarity across the columns, rather
#' # than the rows.
#' word_algebra(mat = eebo, positive = c("king", "woman"), negative = "queen", transpose = T)
#' 
#' # If low-frequency terms are what you're most interested in, you can sometimes get cool
#' # but weird results using multiplication, rather than addition.
#' word_algebra(mat = eebo, positive = "mind", negative = "soul")
#' @export
word_algebra = function(mat, positive = NULL, negative = NULL, operation = "+", transpose = F, sorted = T, exclude = T) {
  if ( class(positive) == "NULL") {
    stop("You must define at least one positive value.")
  }
  if (operation %in% c("+", "*") == F) {
    stop("Operation must be set to '+' or '*'.")
  }
  if (class(mat) == "docMatrix") { mat <- mat@mat  }
  if (transpose == T) { mat = t(mat) }
  if (length(positive) > 1) {
    if (operation == "+") {
      pos = apply(mat[positive,], 2, sum)
    }
    if (operation == "*") {
      pos = apply(mat[positive,], 2, prod)
    }
  } else {
    pos = mat[positive,]
  }
  
  if (length(negative) > 1) {
    if (operation == "+") {
      neg = apply(mat[negative,], 2, sum)     
    }
    if (operation == "*") {
      neg = apply(mat[negative,], 2, prod)
    }
  } else {
    neg = mat[negative,]
  }
  if (length(neg) > 0) { 
    if (operation == "+") {
      vec = pos - neg
    }
    if (operation == "*") {
      vec = pos / neg
    } 
  } else {
    vec = pos
  }
  results = apply(mat, 1, cor, vec)
  if (exclude == T) { 
    ex = which(names(results) %in% c(positive, negative))
    if (length(ex) > 0) { results = results[-ex] }
  }
  if (sorted == T) { results = sort(results, decreasing = T)[1:8]}
  return(results)
}

