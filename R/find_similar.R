#' Find similar terms
#' 
#' Finds similar terms according to one of three metrics. All three metrics
#' together describe the structure of the word in the vector space.
#' 
#' @slot mat A word-context matrix (format either matrix or \code{docMatrix}).
#'           
#' @slot keyword The character string (word) you are interested in evaluating.
#' 
#' @slot method A character string: 'para', 'syn' or 'sig'.
#' 
#' @slot sorted Logical value. If TRUE, will return only the top ten most similar words.
#' 
#' @slot exclude Logical value. If TRUE, results will not return the keyword.
#' 
#' @section Methods:
#' \describe{
#'    \item{Signature}{Find context words that co-occur with a keyword. The 'signature' is
#'                     the word-count over a concordance.} 
#'    \item{Paradigmatic}{Find terms that share a similar signature. "Paradigmatic similarity"
#'                        means that words are deployed in similar contexts.}
#'    \item{Syntagmatic}{Find terms that appear near similar keywords. "Syntagmatic similarity"
#'                       means that words group together as contexts for other keywords.} 
#' }
#' 
#' Each one of these kinds of similarity names different kinds of relationships. For different
#' kinds of words, the most \emph{synonymous} words may co-occur either syntagmatically or
#' paradigmatically. My general sense is that lower frequency terms are more likely to find
#' synonyms syntagmatically and that higher frequency terms are more likely to find synonyms
#' paradigmatically. However, \emph{frequency} here is really a placeholder for a
#' semantic structure that needs better clarification.
#' 
#' The ability to disambiguate different 'senses' of a term by clustering their context words
#' seems like a crucial first step toward better specifying the conceptual relations that
#' underpin the structure. For now, though, it could be that the use of computational analysis
#' of concepts is limited to creating objects for human interpretation. In which case,
#' the similarity relationships identified in this function are best observed using the
#' \code{\link{graph_context}} function.
#' 
#' @examples
#' # For 10 most paradigmatically similar words
#' find_similar(mat = eebo, keyword = "rights") 
#' 
#' #For 10 most syntagmatically similar words
#' find_similar(mat = eebo, keyword = "rights", method = "syn") 
#' 
#' #For 10 most frequent context ('signature') words
#' find_similar(mat = eebo, keyword = "rights", method = "sig")
#' 
#' #For full vector of similarity scores of all words
#' find_similar(mat = eebo, keyword = "rights", method = "para", sorted = F, exclude = F)
#' 
#' @export
find_similar = function(mat, keyword, method = "para", sorted = T, exclude = T) {
  if (class(mat) == "docMatrix") { mat = mat@mat }
  if (method %in% c("para", "syn", "sig") == F) { stop(
    "Method must be set to 'para', 'syn' or 'sig'.")}
  if (class(mat) == "docMatrix") { mat <- mat@mat  }
  if (method == "para") {
    vec = mat[keyword,]
    
    # This method calculates a z-score for the row and 
    # compares the output to columns. This means that only
    # keywords are available for output in results.
    avg = apply(mat, 2, mean)
    dev = apply(mat, 2, sd)
    vec = (vec - avg) / dev # convert to z-score
    results = apply(mat[names(vec),], 2, cor, vec)
  }
  if (method == "syn") {
    vec = mat[keyword,]
    results = apply(mat, 1, cor, vec)
  }
  if (method == "sig") {
    if (keyword %in% colnames(mat) == F) {
      print("Observed signature not available for selected keyword. Estimating signature based on similar words.")
      words = find_similar(mat, keyword, method = "para")
      words = names(words[1:3])
      vec = apply(mat[,words],1,mean)
      results = vec[-which(names(vec) %in% words)]
    } else {
      results = mat[,keyword]
    }
  }
  if (sorted == T) { results = sort(results, decreasing = T)[1:10] }
  if (exclude == T & length(which(names(results) %in% keyword) > 0) ) { results = results[-which(names(results) %in% keyword)] }
  return(results)
}