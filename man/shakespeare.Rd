% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shakespeare.R
\docType{data}
\name{shakespeare}
\alias{shakespeare}
\title{Data - Shakespeare (term-document matrix)}
\format{A matrix with 5,000 rows and 500 columns.}
\usage{
shakespeare
}
\description{
A term-document matrix drawn from the Folger Digital Texts
editions of William Shakespeare's works.
}
\details{
\link[=http://www.folgerdigitaltexts.org/]{Folger Digital Texts}
}
\examples{
# Load the matrix, then analyze a term.
data(shakespeare)
similarity(mat = shakespeare, vec = "TN", margin = 2)
}
\keyword{datasets}

