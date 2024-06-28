\name{Extract}
\alias{s1}
\alias{sc}
\alias{d1}
\alias{d2}
\alias{dc}
\alias{t1}
\alias{t2}
\alias{t3}
\alias{tc}
\alias{s1,aaa-method}
\alias{sc,aaa-method}
\alias{d1,aaa-method}
\alias{d2,aaa-method}
\alias{dc,aaa-method}
\alias{t1,aaa-method}
\alias{t2,aaa-method}
\alias{t3,aaa-method}
\alias{tc,aaa-method}
\alias{single}
\alias{double}
\alias{triple}
\alias{single<-}
\alias{double<-}
\alias{triple<-}
\alias{getthings}
\alias{coeffs}
\alias{extracter}
\alias{overwriter}
\alias{extracter<-}
\alias{c_aaa_extract}
\alias{c_aaa_overwriter}
\alias{[,aaa,ANY,ANY,ANY-method}
\alias{[,aaa,character,ANY,ANY-method}
\alias{[,aaa,matrix,ANY,ANY-method}
\alias{[<-,aaa,ANY,ANY,ANY-method}
\alias{[<-,aaa,character,missing,numeric-method}
\alias{[<-,aaa,matrix,ANY,ANY-method}
\alias{[<-,aaa,missing,ANY,ANY-method}
\alias{[<-,aaa,disord,missing,numeric-method}
\title{Extract or Replace Parts of \code{aaa} objects}
\description{

  Extraction methods for \code{aaa} objects.  The names of the
  two-letter functions and arguments follow a pattern: the initial
  letter (s, d, t) stands for \dQuote{single}, \dQuote{double}, or
  \dQuote{triple}; the second symbol is c for \dQuote{coefficients}, or
  a number (1, 2, 3) denoting first, second, or third.  Thus
  \dQuote{\code{dc()}} gets the coefficients of the double-symbol
  components, and \dQuote{\code{t2()}} gets the second symbol of the
  triple-symbol components.

}
\usage{
\S4method{s1}{aaa}(a)
\S4method{sc}{aaa}(a)
\S4method{d1}{aaa}(a)
\S4method{d2}{aaa}(a)
\S4method{dc}{aaa}(a)
\S4method{t1}{aaa}(a)
\S4method{t2}{aaa}(a)
\S4method{t3}{aaa}(a)
\S4method{tc}{aaa}(a)
single(a)
double(a)
triple(a)
}
\arguments{\item{a}{Object of class \code{aaa}}}
\details{

  An \code{aaa} object is a list of 9 vectors, three numeric and six
  character, which are extractd by functions \code{s1()} etc.

  Functions \code{single()}, \code{double()} and
  \code{triple()} extract the single, double, and triple components
  of their argument, and return the corresponding \code{aaa} object.

  There is no function \code{evitaicossa::coeffs()} because the three
  types of elements are qualitatively different; use \code{sc()},
  \code{dc()}, and \code{tc()} to get the coefficients in \code{disord}
  format.

  Functions \code{getthings()}, \code{extracter()} and
  \code{overwriter()} are lower-level methods, not really intended for
  the end-user.  Function \code{getthings()} takes an \code{aaa} object
  and returns a named list with elements being \code{disord} objects
  corresponding to components \code{s1},\code{sc},\code{d1} etc.
  Function extracter() takes an \code{aaa} object and arguments
  \code{s1}, \code{d1},\code{d2},\code{t1} etc. and returns the
  \code{aaa} object corresponding to the specified index elements.
  Function \code{overwriter} takes

  Functions \code{single()}, \code{double()}, and \code{triple()} return
  the index-1, index-2, and index-3 components of their arguments
  respectively.  Functions \code{single<-()} \emph{et seq.} are the
  corresponding setting methods which overwrite the index-1 (resp. 2,3)
  components with the right hand side.  The right hand side must be
  purely the correct compponent otherwise an error is returned; thus in
  \code{double(a) <- x}, for example, the single-symbol and
  triple-symbol components of \code{x} must be zero.

  Square bracket extraction and replacement methods are more
  user-friendly.  These operate in two distinct modes.  If given named
  arguments (\code{s1}, \code{d1},\code{d2}, \emph{et seq.}) then these
  are interpreted as symbols and coefficients of the different orders.
  If given an unnamed argument, this is interpreted as a character
  vector of length one, two, or three specifying a particular term in
  the object.  See examples.

  }
\value{Return disord or \code{aaa} objects}
\author{Robin K. S. Hankin}
\examples{

x <- linear1(1:3) + (linear1(1:2) + linear2(1:3))^2
x
x[d1=c("a","a"),d2=c("a","b")]
x[s1="a", t1="b", t2="c", t3="c"]


x[s1="a", t1="b", t2="c", t3="c"] <- 88
x
x[c("c","c","b")] <- -777
x


a <- raaaa()
sc(a)
t2(a)
single(a)

single(a) + double(a) + triple(a) == a  # should be TRUE

aaa(d1=d1(a),d2=d2(a), dc=dc(a)) == double(a)

x <- raaaa()
single(x) <- 0
double(x) <- double(raaa())

}
