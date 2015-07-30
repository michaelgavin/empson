#' Evaluate concept drift over time
#' 
#' For an individual word (taken from its .RData file), identify change over time.
#' 
#' @slot keyword A charcter string, the term you're studying.
#' 
#' @slot base A numeric vector, providing the years that will be used to provide
#'            the term's base signature. Default is 1640:1649.
#'            
#' @slot extent A numeric vector, giving the range of years to chart. Default is
#'              1650:1699.
#'              
#' @slot chart A logical value. If TRUE, will make a simple scatterplot.
#' 
#' @section Caveat:
#' This function requires a very specific file structure. You must have \code{.RData}
#' files in your working directory named by the keyterm. For example, to study the
#' word "rights" you must be in a working directory that has a file called "rights.RData".
#' These files hold a special kind of vector-space model: a year-term model.
#' 
#' As this function is written, few concepts show significant change over time. This is
#' because it simply compares the signature of the term for each year, and evaluates is
#' similarity with the mean of the base years. More interesting would be to look at each
#' year in the context of other terms during the same year, and re-write the values from
#' raw signature to TF-IDF scores or Z-scores. Then, those scores could be re-compiled into
#' a year-term matrix for a single concept. This method, I believe, would more effectively
#' show change over time than comparing simple signatures.
#' 
#' @examples
#' find_change("rights")
#' find_change("rights", base = 1640:1688, extent = 1689:1699)
#' find_change("rights", chart = F)
find_change = function(keyword, base = 1640:1649, extent = 1650:1699, chart = T) {
  filename = paste(keyword, ".RData", sep="")
  if (filename %in% dir() == F) {
    stop("Can't find the file. Either you need to update your working directory
         or empson hasn't studied the word you're looking for.")
  }
  load(filename)
  base_mat = results[[1]][as.character(base),]
  base = apply(base_mat, 2, mean)
  mat = results[[1]][as.character(extent),]
  correlations = apply(mat, 1, cor, base)
  if (chart == T) { 
    plot(names(correlations), correlations, main = keyword, ylab = "", xlab = "") 
  } else {
    return(correlations) 
  }
}