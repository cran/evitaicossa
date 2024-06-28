\name{Compare-methods}
\docType{methods}
\alias{Compare-methods}
\alias{Compare,aaa,aaa-method}
\alias{Compare,aaa,ANY-method}
\alias{Compare,aaa,numeric-method}
\alias{Compare,ANY,aaa-method}
\alias{Compare,numeric,aaa-method}
\alias{aaa_compare_aaa}
\alias{aaa_compare_error}
\alias{aaa_equal_aaa}
\alias{c_aaa_equal}
\title{Comparison methods for antiassociative algebra}
\description{

 Comparison methods generally do not make sense for elements of an
 antiassociative algebra.  The only exception is equality: \code{x == y}
 returns \code{TRUE} if \code{aaa} objects \code{x} and \code{y} are
 identical.

 The test for equality follows the \CRANpkg{frab} package: go through
 the keys of \code{x}, compare the corresponding values of \code{y}, and
 return \code{FALSE} when any difference is detected.  This is faster
 than \code{is.zero(x-y)}.  

 Technically, \code{x==0} makes sense but I thought consistency was more
 important: in the package, numeric values cannot be compared with
 \code{aaa} objects.

 Functions \code{aaa_compare_aaa()} etc. are used in \proglang{S4}
 dispatch; \code{c_aaa_equal()} is a low-level helper function that uses
 \proglang{Rcpp} to call the appropriate \proglang{C} routine.

}
\section{Methods}{
\describe{

\item{\code{signature(e1 = "aaa", e2 = "aaa")}}{}
\item{\code{signature(e1 = "aaa", e2 = "ANY")}}{}
\item{\code{signature(e1 = "aaa", e2 = "numeric")}}{}
\item{\code{signature(e1 = "ANY", e2 = "aaa")}}{}
\item{\code{signature(e1 = "numeric", e2 = "aaa")}}{}
}}


