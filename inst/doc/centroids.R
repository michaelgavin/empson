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
cents = list()
for (i in 1:length(wit@concordance[[1]])) {
  cents[[i]] = find_centroid(mat = eebo, positive = unlist(wit@concordance[[1]][i]))
}
cents

