\name{zero}
\alias{zero}
\alias{is.zero}
\title{The additive zero in antiassociative algebras}
\description{
Function \code{is.zero()} tests for its argument being the additive
zero.

Package idiom to create the zero element of the antiassociative algebra
is \code{aaa()}.
}
\usage{
is.zero(x)
}
\arguments{
  \item{x}{Object of class \code{aaa}}
}
\value{Returns a Boolean.}
\author{Robin K. S. Hankin}
\note{
In any antiassociative algebra, the only scalar is zero.
}
\examples{
is.zero(raaa())
is.zero(raaa()*0)
is.zero(aaa())

}
