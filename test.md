Begin by importing libraries

``` r
library(tei2r)
library(empson)
```

Create a working directory then set it

``` r
loc_xml = "~/MLH/EEBO-XML/EEBO-TCP XML"
setwd(loc_xml)
```

Now import the stopwords list and TCP index to your R environment

``` r
data(stopwords)
data(tcp)
tcp = tcp[which(tcp$Status == "Free"),]
```

Then download the TCP documents. Careful: this command will take an hour
or two at least on most computers.

``` r
tcpDownload(tcp)
```

Now set other parameters. Establish a year range that you’d like to
study and make a directory for holding your processed text files.

``` r
range = 1640:1699
loc_texts = "~/MLH/EEBO-DTs/"
setwd(loc_texts)
```

The `empson` package comes in with some built-in functions for building
word matrices, but those have a number of quality-control features that
slow down processing speed. Because we’ll be scrolling through thousands
of documents, here are a couple simplified functions.

``` r
# Special Build Doc List Function
easyDocList = function(directory = "", stopwordsFile = "", indexFile ="") {
  source('~/projects/tei2r/R/docList.R')
  dl = docList()
  dl@directory = directory
  dl@indexFile = indexFile
  dl@index = read.csv(dl@indexFile,stringsAsFactors=FALSE)
  dl@filenames = paste(dl@index$TCP, ".xml", sep="")
  dl@paths = paste(dl@directory,"/",dl@filenames,sep="")
  dl@stopwords = stopwords
  return(dl)
}

# Now define special matrix-building function
buildYearlyMatrices <- function(dt, kw, voc, context = 5) {
  vocab = voc
  keywords = kw
  mat = matrix(0, length(vocab), length(keywords))
  rownames(mat) = vocab
  colnames(mat) = keywords
  for (i in 1:length(keywords)) {
    print(paste("Gathering KWICS for",keywords[i]))
    term_kwics = list()
    for (j in 1:length(dt@text)) {
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
    ord = which(words %in% vocab == T)
    words = words[ord]
    freqs = freqs[ord]
    mat[words,i] = freqs
  }
  return(mat)
}
```

Now import the texts for each year. This is a big for-loop that gathers
the texts, normalizes long Ss, removes numbers, punctuation, and
capitalization, and takes out all stopwords and any word with a special
character (the vast majority of which, in EEBO, are transcription errors
or weird glyphs.)

``` r
for (i in 1:length(range)) {
  year = range[i]
  print(year)
  write.csv(tcp[which(tcp$Date == year),], "temp_index.csv")
  dl = easyDocList(directory = loc_xml, indexFile = "temp_index.csv")
  dt = importTexts(dl)
  save(dt, file = paste(as.character(year),".rda", sep=""))
}
```

Now let’s build a vocabulary. This for-loop goes through every year,
counts the words, and captures the words used each year, putting them
into a big vector, called `v`. Once that vector is complete, the
vocabulary for each year was blended into the previous years’ to create
two smaller vectors: `keywords` will be the words chosen for
Keywords-In-Context analysis; `vocab` will be the list of context words
to be included in that analysis.

The purpose of this is to reduce the size of the matrix (otherwise it
becomes very difficult to compute) and to filter out low-frequency
words, the majority of which are irregularities in the transcription and
markup that have little effect on meaning.

Notice that the `keywords` object is built using the `intersect()`
function. The line of code that reads
`keywords = intersect(keywords, v[1:4891])` limits the `keywords` object
to include only words that are among the 4,891 most frequent **in every
single year**. Any word that is not among the 4,891 most frequent words,
even in just one year, will be excluded. This ensures continuity over
time, such that the keywords will be applicable to every year. The
number 4,891 was chosen because it was the threshold at which the corpus
returned exactly 2,000 keywords.

The `vocab` vector is built differently, and results in about 32,000
words. Notice that it, too, uses an `intersect()` operation, though it
intersects with the entire vocabulary, rather than limited to 4,891 most
frequent words. So it throws out old words as it goes. Any word in the
vocabulary that isn’t in the vocabulary of the current year gets tossed,
then in the next line of code, `vocab = c(vocab,v[1:20000])`, uses a
concatenate function, `c()` to add the 20,000 most frequent words in.
This takes the top twenty thousand words in any given year and adds them
into the vocabulary, if any are missing. As it runs, the vocabulary
maintains a size of about 30,000 words, swapping a few thousand in and
out as it goes.

As a consequence, the vocabulary of the `eebo` matrix includes words
that are among the 20,000 most frequent words in any given year, which
are also used in every year thereafter. Each word has to have at least
one year when it spikes, and it can’t completely drop from the lexicon
in any following year. Here are the requirements to be included:

1.  Each word must have at least one spike year when it’s among the
    20,000 most frequent
2.  Each word must be at least present (used at least once) in every
    year after its spike year.

Every word that meets these two criteria makes the cut. These are the
lexical survivors. These rules create a slight bias toward the later
years, which should be kept in mind when interpreting results. In rare
cases this can cause problems when looking at documents from the earlier
decades (1640s and 1650s), because archaic spellings are sometimes
excluded from the model, and unusual words from the 1690s are more
likely to be included than unusual words from the 1640s.

``` r
keywords = c()
vocab = c()
for (i in 1:length(range)) {
  year = range[i]
  print(paste("Working on year", year))
  load(paste(loc_texts, as.character(year),".rda", sep=""))
  v = table(unlist(dt@text))
  v = sort(v, decreasing = T)
  v = names(v)
  if (i == 1) {
    keywords = v[1:4891]
    vocab = v
  } else {
    keywords = intersect(keywords, v[1:4891])
    vocab = intersect(vocab, v)
    vocab = c(vocab,v[1:20000])
    vocab = sort(unique(vocab))
  }
}
```

Because it’s difficult for desktop computers to handle this much data at
a time (and because it’ll be useful down the road to retain year-by-year
data), I begin by compiling a matrix for every year.

First set the location:

``` r
loc_mats = "~/MLH/EEBO-MAT/"
setwd(loc_mats)
```

Now build a matrix for each year

``` r
for (i in 1:length(range)) {
  year = range[i]
  print(paste("Working on year", year))
  load(paste(loc_texts, as.character(year),".rda", sep=""))
  mat = buildYearlyMatrices(dt = dt, vocab = vocab, keywords = keywords)
  save(mat, file = paste(as.character(year),".rda", sep=""))
}
```

And add them together. \`\`\`r eebo = matrix(0, length(vocab),
length(keywords)) colnames(eebo) = keywords row.names(eebo) = vocab for
(i in 1:length(range)) { print(i) year = range\[i\]
load(paste(loc\_mats, as.character(year),“.rda”, sep="")) eebo = eebo +
mat }
