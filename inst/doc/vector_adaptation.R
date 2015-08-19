## ------------------------------------------------------------------------
library(tei2r)
library(empson)
data(eebo)
dir.create(paste(getwd(), "/macflecknoe", sep=""))
mac_dir = paste(getwd(), "/macflecknoe", sep="")
setwd(mac_dir)
results = tcpSearch(term = "MacFlecknoe", field = "Title", write = T)
tcpDownload(results)
dl = buildDocList(directory = getwd(), indexFile = "index.csv")

## ------------------------------------------------------------------------
dt = importTexts(dl)
wit = buildConcordance(dt = dt, keyword = "wit", context = 5)
wit@concordance

## ------------------------------------------------------------------------
results = list()
for (i in 1:length(wit@concordance[[1]])) {
  words = unlist(wit@concordance[[1]][i])
  words = words[which(words %in% rownames(eebo) == T)]
  results[[i]] = vector_adaptation(mat = eebo, positive = words, operation = "+")
}
results

## ------------------------------------------------------------------------
results = list()
for (i in 1:length(wit@concordance[[1]])) {
  words = unlist(wit@concordance[[1]][i])
  words = words[which(words %in% rownames(eebo) == T)]
  results[[i]] = vector_adaptation(mat = eebo, positive = words, operation = "*")
}
results

## ----, echo=FALSE--------------------------------------------------------
graph_context(mat = eebo,keyword = 'wit', use='syn')
graph_context(mat = eebo,keyword = 'wit', use='para')

