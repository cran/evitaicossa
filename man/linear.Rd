\name{linear}
\alias{linear}
\alias{linear1}
\alias{linear2}
\alias{linear3}
\title{Linear functions}
\description{
  Linear functions returning single, double, or triple-symbol \code{aaa}
  objects.
}
\usage{
linear1(x)
linear2(x)
linear3(x)
}
\arguments{\item{x}{A numeric vector}}
\details{

These functions return an antiassociative algebra element with the
specified coefficients.  Given a numeric vector \code{v} with elements
\eqn{v_1,v_2,\ldots, v_n}{v_1,...,v_n} then

\code{linear1(v)} returns \eqn{v_1\mathbf{a} + v_2\mathbf{b}+\cdots+
v_n\mathbf{L_n}}{omitted}, where \eqn{\mathbf{L_n}}{L_n} is the
\eqn{n^\mathrm{th}}{n-th} letter of the alphabet.  Similarly,
\code{linear2(v)} returns \eqn{v_1\mathbf{a}\mathbf{a}+\cdots+
v_n\mathbf{L_n}\mathbf{L_n}}{omitted}, and \code{linear3(v)} returns
\eqn{v_1(\mathbf{a}\mathbf{a})\mathbf{a}+\cdots+
v_n(\mathbf{L_n}\mathbf{L_n})\mathbf{L_n}}{omitted}. They are linear in
the sense that

\deqn{
  f(\alpha\mathbf{x}+\beta\mathbf{y})=
\alpha f(\mathbf{x})+\beta f(\mathbf{y})}{f(ax+by)=af(x)+bf(y)}

where \eqn{\alpha,\beta\in\mathbb{R}}{omitted} and
\eqn{\mathbf{x},\mathbf{y}\in\mathbb{R}^n}{omitted}.

}
\value{These functions return an object of class \code{aaa}.}
\author{Robin K. S. Hankin}
\examples{

linear1(sample(8))
linear2(sample(8))
linear3(sample(8))

a <- 3
b <- 7
x <- sample(9)
y <- sample(9)

linear1(a*x + b*y) == a*linear1(x) + b*linear1(y)
linear2(a*x + b*y) == a*linear2(x) + b*linear2(y)
linear3(a*x + b*y) == a*linear3(x) + b*linear3(y)
}
