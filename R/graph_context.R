#' Create a graph of similar words
#' 
#' Build dendrogram that represents the conceptual structure of a keyword.
#' 
#' @slot mat A word-context matrix.
#' 
#' @slot keyword A string.
#' 
#' @slot use A string ('para', 'syn', or 'sig'.) See \code{\link{find_similar}}.
#' 
#' @return A dendrogram, showing structure of 30 most-similar words.
#' 
#' @section What it does:
#' This function runs \code{find_similar} over a word-context matrix and looks for the
#' thirty most similar terms. After finding the thirty most similar terms, it clusters
#' them, performing a simple but effective word-sense disambiguation (WSD).
#' @export
graph_context = function(mat, keyword, use = "all") {
  if (class(mat) == "docMatrix") { mat = mat@mat }
  if (use %in% c("all", "para", "syn", "sig") == F) {
    stop("The 'use' argument must specify method to be deployed: 'all', 'para', 'syn' or 'sig'.")
  }
  if (use == "all") {
    para = find_similar(mat, keyword, method = "para", exclude = F)
    syn = find_similar(mat, keyword, method = "syn", exclude = F)
    sig = find_similar(mat, keyword, method = "sig", exclude = F)
    words = unique(names(c(para, syn, sig)))
  }
  
  if (use == "para") {
    para = find_similar(mat, keyword, method = "para", exclude = F, sorted = F)
    para = sort(para, decreasing = T)[1:30]
    words = names(para)
  }
  
  if (use == "syn") {
    syn = find_similar(mat, keyword, method = "syn", exclude = F, sorted = F)
    syn = sort(syn, decreasing = T)[1:30]
    words = names(syn)
  }
  
  if (use == "sig") {
    sig = find_similar(mat, keyword, method = "sig", exclude = F, sorted = F)
    sig = sort(sig, decreasing = T)[1:30]
    words = names(sig)
  }
  
  mat = mat[words,]
  correlations = matrix(0, length(words), length(words))
  for (i in 1:nrow(correlations)) {
    correlations[i,] = apply(mat, 1, cor, mat[i,]) # This similar to second-order correlation described in Schutze (1998)
  }
  rownames(correlations) = words
  hc = hclust(dist(correlations))
  plot(as.dendrogram(hc, hang = 0.02), horiz = T, main = paste(keyword, " (", use, ")")) 
}