# Functions for empson

# Finds similar terms according to one of three metrics. All three metrics
# together describe the structure of the word in the vector space.
find_similar = function(mat, keyword, method = "para", sorted = T, exclude = T) {
  if (method %in% c("para", "syn", "sig") == F) { stop(
    "Method must be set to 'para', 'syn' or 'sig'.")}
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
      words = names(sort(mat[keyword,], decreasing = T)[1:3])
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
  

# Basic word calculations to find analogous terms using the vector-offset method. Results not very impressive.
word_algebra = function(mat, positive = NULL, negative = NULL, operation = "+", transpose = F, sorted = T, exclude = T) {
  if ( class(positive) == "NULL") {
    stop("You must define at least one positive value.")
  }
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

# Return z-score for a keyword in a term matrix
zscore = function(mat, vec) {
  avg = apply(mat, 1, mean)
  dev = apply(mat, 1, sd)
  score = (vec - avg) / dev
  return(score)
}

# For an individual word (taken from its results .RData file), identify change over time
find_change = function(keyword, chart = T) {
  filename = paste(keyword, ".RData", sep="")
  if (filename %in% dir() == F) {
    stop("Can't find the file. Either you need to update your working directory
         or empson hasn't studied the word you're looking for.")
  }
  load(filename)
  base_mat = results[[1]][as.character(1640:1649),]
  base = apply(base_mat, 2, mean)
  mat = results[[1]][as.character(1650:1699),]
  correlations = apply(mat, 1, cor, base)
  if (chart == T) { 
    plot(names(correlations), correlations, main = keyword, ylab = "", xlab = "") 
  } else {
    return(correlations) 
  }
}

# Build dendrogram that represents contexts using basic word-sense disambiguation (WSD)
graph_context = function(mat, keyword, use = "all") {
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



