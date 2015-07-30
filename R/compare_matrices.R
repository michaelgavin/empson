#' Subtract matrices
#' 
#' Subtract over two word-context matrices to uncover different word-use
#' patterns.
#' 
#' @slot mat1 A word-context matrix
#' 
#' @slot mat2 A word-context matrix
#' 
#' @slot output A string (either "mat" or "vec".) If "mat", function will return
#'              a matrix (mat1 - mat2), made up of the shared rows and shared 
#'              columns. If "vec", function will return a vector
#'              giving the Pearson correlation for each context word (row) shared
#'              by the matrices.
#'              
#' @examples
#' # Get new matrix showing how Locke differs from peers.
#' data(locke)
#' data(nlaw)
#' data(eebo)
#' new_mat <- find_difference(locke, nlaw)
#' 
#' # For quick visualizations, create context dendrograms.
#' graph_context(new_mat, "property")
#' 
#' # For details: look at Z-scores of individual terms to see how Locke's usage differs.
#' sort(zscore(new_mat, "legislative"), decreasing = T)[1:10]
#' 
#' # Get similarity scores to see how differently each word is used.
#' vec = compare_matrices(locke, nlaw, output = "vec")
#' sort(vec)[1:50] # Display 50 least-similar words
#' vec["property"] # See how similar one word is to the context.
#' 
#' @export
compare_matrices <- function(mat1, mat2, output = "mat") {
  if (class(mat1) == "docMatrix") { mat1 = mat1@mat }
  if (class(mat2) == "docMatrix") { mat2 = mat2@mat }
  shared_rows <- intersect(rownames(mat1), rownames(mat2))
  shared_cols <- intersect(colnames(mat1), colnames(mat2))
  mat1 <- mat1[shared_rows, shared_cols]
  mat2 <- mat2[shared_rows, shared_cols]
  
  if (output == "mat") {
    new_mat <- mat1[shared_rows, shared_cols] - mat2[shared_rows, shared_cols]
    return(mat)
  }
  
  if (output == "vec") {
    vec <- c() 
    for (i in 1:nrow(mat1)) {
      sim <- cor(mat1[i,], mat2[i,])
      vec <- c(vec, sim)
    }
    names(vec) <- rownames(mat1)
    return(vec)
  }
}