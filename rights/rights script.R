data(eebo)
graph_context(mat = eebo, "rights", use = "syn")
graph_context(mat = eebo, "rights", use = "para")
graph_context(mat = eebo, "rights", use = "sig")
results = tcpSearch(term = "A48901", field = "TCP", write = T)
tcpDownload(results)
dl = buildDocList(directory = getwd(), indexFile = "index.csv")
dt = importTexts(dl)
dc = buildConcordance(dt = dt, keyword = "rights", context = 5)

addition = list()
for (i in 1:length(dc@concordance[[1]])) {
  words = unlist(dc@concordance[[1]][i])
  words = words[which(words %in% rownames(eebo) == T)]
  addition[[i]] = vector_adaptation(mat = eebo, positive = words, operation = "+")
}

multiplication = list()
for (i in 1:length(dc@concordance[[1]])) {
  words = unlist(dc@concordance[[1]][i])
  words = words[which(words %in% rownames(eebo) == T)]
  multiplication[[i]] = vector_adaptation(mat = eebo, positive = words, operation = "*")
}

# Output term series into a matrix for .csv
rights_mat = matrix(NA,36,20)
for (i in 1:18) {
  rights_mat[i,] = names(addition[[i]])
}
for (i in 19:36) {
  rights_mat[i,] = names(multiplication[[i-18]])
}

rights_sig = matrix(NA,18,11)
for (i in 1:length(dc@concordance[[1]])) {
  rights_sig[i,] = dc@concordance[[1]][i][[1]] 
}