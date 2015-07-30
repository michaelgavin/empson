#====================================================================
# Get term frequency data for the terms in each document across the
# corpus.  The first parameter is a docTexts object containing the full 
# cleaned text for each document in your corpus.  The second describes
# the type (document-term matrix or word-context matrix) that the 
# function will build.

# Returns a docMatrix object.
#====================================================================
#' 
#' Gathers word frequencies into a matrix.
#' 
#' @param dt          A \code{docTexts} object that contains the full
#'                    text of each document in your corpus.
#'                
#' @param type        A string (either "docTerm" or "wordContext") to
#'                    specify the type of matrix to be created. Document-
#'                    term matrices have columns that are documents.
#'                    Word-context matrices have columns that are keywords.
#'                
#' @param wordLimit   A numeric value that limits the number of words (i.e.,
#'                    the number of rows) that will be counted. The default 
#'                    is 5,000 words.
#'                    
#' @param modelSize   A numeric value, used for word-context matrices, that
#'                    sets the number of keywords to be evaluated. The default
#'                    value is 500, and the max is 5,000. Automatically, the 
#'                    most frequently occuring words (not counting stopwords) are
#'                    used for analysis.
#'
#' @param context     A numeric value, for word-context matrices, that sets the
#'                    size of the context window. At 5 (default), the algorithm
#'                    will find words with 5 positions, before or after, each
#'                    of the keywords.
#'                    
#' @param method      A string, either 'raw' or 'proportional'. Determines with the
#'                    word frequencies will be reported as raw integers or in 
#'                    proportion to the whole. Default is 'raw'.                    
#'                                                                                
#' @return dm     a \code{docMatrix} object with frequency
#'                data for the corpus.
#' 
#' @examples
#' dt = docTexts()
#' dm = getDocMatrix(dt)
#' dm = getDocMatrix(dt = dt, type = "docTerm", wordLimit = NULL, method = "proportional")
#' dm = getDocMatrix(dt = dt, type = "wordContext", wordLimit = 5000, modelSize = 500, context = 5, method = "raw")
#' @export
buildMatrix = function(dt, type = "docTerm", wordLimit = 5000, modelSize = 500, context = 5, method = "raw") {
  dm = docMatrix()
  dm@directory       = dt@directory
  dm@indexFile       = dt@indexFile
  dm@mat             = matrix()
  print("Calculating frequencies for each text.  Accessible by dm@mat")
  if (method %in% c("raw", "proportional") == F) {
    stop("Method must be specified either as 'raw' or 'proportional'.")
  }
  if (type %in% c("docTerm", "wordContext") == F) {
    stop("Type must be specified either as 'docTerm' or 'wordContext'.")
  }
  if (type == "docTerm") {
    vocab = names(sort(table(unlist(dt@text)), decreasing = T))
    if (class(wordLimit) != "NULL") { vocab = vocab[1:wordLimit] }
    dm@mat = matrix(0, length(vocab), length(dt@text))
    rownames(dm@mat) = vocab
    colnames(dm@mat) = names(dt@text)
    for (i in 1:length(dt@text)) {
      if (class(wordLimit) == "NULL") {
        wordfreqs = table(dt@text[i])
        words = names(wordfreqs)
        freqs = as.numeric(wordfreqs)
        if (method == "raw") { dm@mat[words,i] = freqs }
        if (method == "proportional") { dm@mat[words,i] = freqs / sum(freqs)  }
      } else {
        wordfreqs = table(dt@text[i])
        wordfreqs = wordfreqs[intersect(names(wordfreqs),rownames(dm@mat))]
        words = names(wordfreqs)
        freqs = as.numeric(wordfreqs)
        if (method == "raw") { dm@mat[words,i] = freqs }
        if (method == "proportional") { dm@mat[words,i] = freqs / sum(freqs)  }
      }
    }
    return(dm)
  } 
  
  if (type == "wordContext") {
    if (modelSize > 5000) {
      stop("For this application, the model should be built with fewer than 5,000 words. Too high and the machine will run forever.")
    }
    alltext = unlist(dt@text)
    print("Building vocabulary of context words...")
    vocab = names(sort(table(alltext), decreasing = T))
    keywords = vocab[1:modelSize]
    dm@mat = matrix(0, length(vocab), length(keywords))
    rownames(dm@mat) = vocab
    colnames(dm@mat) = keywords
    for (i in 1:length(keywords)) {
      term_kwics = list()
      for (j in 1:length(dt@text)) {
        print(paste("Gathering context data on word", i, "of", length(keywords)))
        text = dt@text[[j]]
        hits = which(text == keywords[i])
        kwics = list()
        if (keywords[i] %in% text) {
          for (k in 1:(length(hits))) {
            start = hits[k] - context
            end = hits[k] + context
            if (start < 1) {
              start = 1
            }
            kwics[[k]] = text[start:end]
          }
          term_kwics[[j]] = kwics
        }
      }
      contexts = table(unlist(term_kwics))
      words = names(contexts)
      freqs = as.numeric(contexts)
      if (method == "raw") { dm@mat[words,i] = freqs }
      if (method == "proportional") { dm@mat[words,i] = freqs / sum(freqs)  }
    }
  }
  if (class(wordLimit) != "NULL") {
    totals = apply(dm@mat, 1, sum)
    topwords = sort(totals, decreasing = T)[1:wordLimit]
    topwords = unique(c(names(topwords), keywords))
    dm@mat = dm@mat[topwords,]
  }
  return(dm)
}
