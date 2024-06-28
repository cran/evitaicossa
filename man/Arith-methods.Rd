\name{Arith-methods}
\docType{methods}
\alias{Arith-methods}
\alias{Arith,aaa,aaa-method}
\alias{Arith,aaa,numeric-method}
\alias{Arith,numeric,aaa-method}
\alias{-,aaa,missing-method}
\alias{+,aaa,missing-method}
\alias{aaa_arith_aaa}
\alias{aaa_arith_numeric}
\alias{aaa_negative}
\alias{numeric_arith_aaa}
\alias{aaa_plus_aaa}
\alias{aaa_prod_aaa}
\alias{aaa_plus_numeric}
\alias{aaa_prod_numeric}
\alias{aaa_power_numeric}
\alias{c_aaa_add}
\alias{c_aaa_prod}
\title{Arithmetic methods for \code{aaa} objects}
\description{
Arithmetic methods for objects of class \code{aaa}.

}
\section{Methods}{
\describe{

\item{\code{signature(e1 = "aaa", e2 = "aaa")}}{
  Dispatches to \code{aaa_arith_aaa()}
}

\item{\code{signature(e1 = "aaa", e2 = "numeric")}}{
  Dispatches to \code{aaa_arith_numeric()}
}

\item{\code{signature(e1 = "numeric", e2 = "aaa")}}{
  Dispatches to \code{numeric_arith_aaa()}
}
}

The \proglang{S4} methods call lower-level functions
\code{aaa_plus_aaa()}, \code{aaa_prod_aaa()}, \code{aaa_prod_numeric()},
\code{aaa_negative()}, and \code{aaa_plus_numeric()}.

These functions call the \proglang{Rcpp} functions
\code{aaa_identity()}, \code{c_aaa_add()}, and \code{c_aaa_prod()}.



}
\keyword{methods}

