\name{show}
\alias{show}
\alias{print}
\alias{print.aaa}
\alias{aaa_show}
\alias{putsign}
\alias{single_string}
\alias{double_string}
\alias{triple_string}
\alias{show,aaa-method}
\title{Print method for antiassociative algebra objects}
\description{Show methods for \code{aaa} objects}
\usage{
\S4method{show}{aaa}(object)
aaa_show(a)
}
\arguments{
  \item{a,object}{Object of class \code{aaa}}
}
\details{

  A bunch of functionality to print \code{aaa} objects.

  Function \code{putsign()} is a low-level helper function that puts the
  sign (that is, \code{+} or \code{-}) before each element of a numeric
  vector.  Functions \code{single_string()}, \code{double_string()}, and
  \code{triple_string()} process the 1,2, and 3- symbols for printing.
  
}
\value{No return value, called for side-effects}  
\author{Robin K. S. Hankin}
\examples{
aaa_show(raaa())
aaa_show(aaa())
}

