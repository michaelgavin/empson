#' An S4 class that contains and manages word-frequency data
#' 
#' The \code{docMatrix} creates a special \code{empson} object that
#' represents a document collection as a matrix of words
#' 
#' @slot directory A string that gives the filepath to the main directory 
#' @slot indexFile A string that gives the filepath to the index file for the
#'                      collection.
#' @slot type A string: either "docTerm" for a document-term matrix, or "wordContext"
#'              for a word-context matrix.
#' @slot mat A matrix of words.
#' 
#' @section What it does:
#' The \code{docMatrix} object holds a vector-space model of
#' your document collection. The two most common vector-space models (which
#' are the only two currently supported in \code{empson}) are document-term 
#' matrices and word-context matrices. In a document-term matrix, each column is
#' a document in the collection, and each row is a word found in that document. In
#' a word-context matrix, each column is a keyword, and each row is a context term
#' found within a window of that keyword. 
#' @export
docMatrix <- setClass("docMatrix",
                           slots = c(directory        = "character",
                                     indexFile        = "character",
                                     type             = "character",
                                     mat              = "matrix"
                           ))
