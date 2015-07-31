#' Multiply and divide word vectors
#' 
#' Performs basic word calculations to find analogous terms using a vector-offset method.
#' 
#' @slot mat A word-context matrix (format either matrix or \code{docMatrix}).
#' 
#' @slot positive A character vector of terms to multiply/add.
#' 
#' @slot negative A character vector of terms to divide/subtract.
#' 
#' @slot operation A character vector (either "+" or "*"), that controls whether the vectors
#'                 will be added as simple sums or as products. Default is "*".
#' 
#' @slot exclude A logical value. If TRUE, the computed terms ('positive' and 'negative') will
#'               be excluded from the results.
#'
#' @section What it does:
#' This function finds analogies by entering word vectors into a simple proportional equation. For
#' example, the analogy 'king':'queen'::'man':'woman' can be represented as
#' \deqn{V('king') / V('queen') ~= V('man') / V('woman') }
#' 
#' Of course, with vector-space models these equations are never exact, so what you're looking
#' for is not a precise equivalence, but a vector of candidates that might best fill the
#' analogy. If we re-state the above formula as \eqn{A/B = C/D}, then given three word vectors,
#' A, B, and C, we should be able to guess the best candidates for D. The equation \eqn{A/B = C/D}
#' can be converted to \eqn{C * B / A = D}.
#' 
#' When using the \code{find_analogies} function to seek out analogies in this way, enter your
#' search term in this (reverse) order: 
#' 
#' \code{find_analogies(mat = eebo, positive = c("man","queen"), negative = "king")}
#' 
#' \strong{Notice that you have to enter your 'A' term as the 'negative' parameter.}
#'  
#' This returns "woman" as the first result. If you reverse the terms, such that you're looking
#' for the analogy \eqn{man:woman::king:?}, enter it this way:
#' 
#' \code{find_analogies(mat = eebo, positive = c("king","woman"), negative = "man")}
#' 
#' This returns some queens' names, and "queen" is the fourth result.
#' 
#' Results are similar when using addition and subtraction, but I have found results to be
#' sharper using multiplication, and the representation of analogies as fractions makes
#' more intuitive sense as an analogy for analogy, at least to me.
#' 
#' However, it's really not super precise. It struggles with analytical thinking, like
#' part-whole relationships.
#' 
#' @examples
#' data(eebo)
#' # 'arteries':'veins' for words that exist together in a common category
#' find_analogies(mat = eebo, positive = c("liberties", "veins"), negative = "arteries")
#' 
#' # 'red':'yellow' captures something really interesting (not sure what)
#' # about 'prejudice' and its relation to the color 'red'.
#' find_analogies(mat = eebo, positive = c("rights", "yellow"), negative = "red")
#' find_analogies(mat = eebo, positive = c("liberties", "yellow"), negative = "red")
#' find_analogies(mat = eebo, positive = c("privileges", "yellow"), negative = "red")
#' 
#' # contrast to 'yellow':'red'
#' find_analogies(mat = eebo, positive = c("rights", "red"), negative = "yellow")
#'  
#' # It has trouble specifying relations among commonly co-occuring words
#' find_analogies(mat = eebo, positive = c("tree", "city"), negative = "street")
#' find_analogies(mat = eebo, positive = c("tree", "street"), negative = "city")
#'  
#' # 'husband':'wife' for antonyms
#' find_analogies(mat = eebo, positive = c("heaven", "wife"), negative = "husband")
#' find_analogies(mat = eebo, positive = c("good", "wife"), negative = "husband")
#' 
#' # 'husband':'marriage' for (not sure how to characterize it, really)
#' find_analogies(mat = eebo, positive = c("coronation", "marriage"), negative = "husband")
#' find_analogies(mat = eebo, positive = c("ordination", "marriage"), negative = "husband")
#' 
#' # Were sherriffs elected in the seventeenth century?
#' find_analogies(mat = eebo, positive = c("election", "king"), negative = "succession")
#' find_analogies(mat = eebo, positive = c("election", "king"), negative = "coronation")
#' 
#' # 'members':'parliament' captures frequent co-occurence, though not necessarily
#' # "A of B" locutions:
#' find_analogies(mat = eebo, positive = c("church", "parliament"), negative = "members")
#' find_analogies(mat = eebo, positive = c("king", "parliament"), negative = "members")
#' 
#' @export
find_analogies = function(mat, positive = NULL, negative = NULL, operation = "*", sorted = T, exclude = T) {
  if ( class(positive) == "NULL") {
    stop("You must define at least one positive value.")
  }
  if (operation %in% c("+", "*") == F) {
    stop("Operation must be set to '+' or '*'.")
  }
  if (class(mat) == "docMatrix") { mat <- mat@mat  }
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
  vec[is.na(vec)] = 0
  vec[which(vec == Inf)] = 0
  #results = apply(mat, 1, cor, vec)
  avg = apply(mat, 2, mean)
  dev = apply(mat, 2, sd)
  vec = (vec - avg) / dev # convert to z-score
  results = apply(mat[names(vec),], 2, cor, vec)
  if (exclude == T) { 
    ex = which(names(results) %in% c(positive, negative))
    if (length(ex) > 0) { results = results[-ex] }
  }
  if (sorted == T) { results = sort(results, decreasing = T)[1:8]}
  return(results)
}

