\name{allsymbols}
\alias{allsymbols}
\title{All symbols in an aaa object}
\description{
Function \code{allsymbols()} returns a character vector whose entries
include all symbols of its argument.
}
\usage{
allsymbols(a)
}
\arguments{
  \item{a}{Object of class \code{aaa}}
}
\value{Returns a character vector}
\author{Robin K. S. Hankin}
\examples{
a <- raaaa()
a
allsymbols(a)

a[cbind(allsymbols(a))] == single(a)

}
