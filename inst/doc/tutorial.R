## ------------------------------------------------------------------------
library(tei2r)
library(empson)

## ------------------------------------------------------------------------
data(eebo)
data(nlaw)
data(locke)

## ----, fig.show = 'hold', fig.width = 6.5, fig.align = 'center', fig.height = 6.5----
graph_context(mat = eebo, keyword = "succession", use = "all")

