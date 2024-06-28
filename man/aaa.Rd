\name{aaa}
\alias{aaa}
\alias{aaa_single}
\alias{aaa_double}
\alias{aaa_triple}
\alias{aaa_identity}
\alias{as.aaa}
\alias{lavter}
\alias{thing_to_aaa}
\title{
Function to create objects of class \code{aaa}
}
\description{
Objects of class \code{aaa}
}
\usage{
aaa(s1 = character(0), sc = numeric(0), d1 = character(0), d2 =
character(0), dc = numeric(0), t1 = character(0), t2 = character(0), t3
= character(0), tc = numeric(0))
lavter(cout)
as.aaa(s)
thing_to_aaa(L)
}
\arguments{
  \item{s1,d1,d2,t1,t2,t3}{single, double, triple symbols}
  \item{sc,dc,tc}{single, double, triple coefficients}
  \item{L}{A list with elements \code{s1} etc}
  \item{cout}{list}
  \item{s}{Object that function \code{as.aaa()} will coerce to an
    \code{aaa} object}
}
\details{

  Function \code{lavter()} is the formal creation method for \code{aaa}
  objects; it is the only place that \code{new()} is called.  It takes a
  single argument \code{cout}, which is a list as returned by
  \proglang{C} function \code{retval()}.  But it is a little awkward to
  use and the user should use other functions for creation, which are
  more user-friendly and have sensible defaults:

  \itemize{
    \item Function \code{aaa()} takes named arguments \code{s1} etc,
      with defaults corresponding to \dQuote{not present}
    \item Function \code{thing_to_aaa()} takes a list with names
      \code{s1} etc
    \item Function \code{as.aaa()}
      tries hard to coerce its argument to an \code{aaa} object
}

}
\value{Return objects of class \code{aaa}}
\author{Robin K. S. Hankin}
\examples{

aaa(s1 = "x", sc = 13)
aaa(d1 = "z", d2 = "w", dc = 14)
aaa(t1 = "x", t2 = "y", t3 = "z", tc = 15)

aaa(
    s1 = c("a","d"),
    sc = c( 4 , 2 ),
    d1 = c("a", "a", "a", "b"),
    d2 = c("a", "b", "d", "a"),
    dc = c( 3 ,  4 ,  4 ,  3 ),
    t1 = c("a", "a", "a", "b", "b"),
    t2 = c("c", "d", "d", "c", "c"),
    t3 = c("a", "c", "d", "a", "b"),
    tc = c(-4 , -1 , -4 , 11 , 20 )
)


aaa() # the zero object

aaa(s1=letters,sc=seq_along(letters))
aaa(d1=state.abb,d2=rev(state.abb),dc=seq_along(state.abb))

as.aaa(state.abb)


evita <- aaa(s1=letters[1:5],sc=1:5)
icossa <- aaa(d1=c("fish","chips"),d2=c("x","y"),dc=c(6,7))

evita
evita + icossa
evita * icossa
evita^2

f <- function(o){aaa(state.abb[o],seq_along(o))}
f(8:9) - (f(1:2) - f(6:8)^2)^2

}
