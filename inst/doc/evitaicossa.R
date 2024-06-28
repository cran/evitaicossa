## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
options(rmarkdown.html_vignette.check_title = FALSE)
library("evitaicossa")
set.seed(1)

## ----out.width='20%', out.extra='style="float:right; padding:10px"',echo=FALSE----
knitr::include_graphics(system.file("help/figures/evitaicossa.png", package = "evitaicossa"))

## ----label=showraaa-----------------------------------------------------------
raaa()

## ----label=makexx1yz----------------------------------------------------------
x  <- as.aaa(c("p","q","r"))
x1 <- aaa(s1 = c("p","r","x"),c(-1,5,6))
y <- aaa(d1 = letters[1:3],d2 = c("foo","bar","baz"),dc=1:3)
z <- aaa(
	t1 = c("bar","bar","bar"),
	t2 = c("q","r","s"),
	t3 = c("foo","foo","bar"),
	tc = 5:7)

## ----label=applyarith---------------------------------------------------------
x
x1
x+x1

## ----label=usemult------------------------------------------------------------
x*(x1+y)

## ----label=checkdistributivity------------------------------------------------
x*(x1+y) == x*x1 + x*y

## ----label=checkremarkable----------------------------------------------------
a <- raaa()
b <- raaa()
x <- raaa()
(a+a*x)*(b+x*b) == a*b

## ----label=showsingledoubletriple---------------------------------------------
a
single(a)
double(a)
triple(a)

## ----label=extract123---------------------------------------------------------
single(a) <- 0
a
double(a) <- double(b) * 1000
a

## ----label=showsquarebracketextract-------------------------------------------
(a <- raaa(s=5))
a[s1=c("c","e"),t1="c",t2="d",t3="d"]

## ----label=showsquarebracketreplace-------------------------------------------
(a <- raaa(s=5))
a[s1="a",d1=c("c","w"),d2=c("d","w")] <- 888
a

## ----unnamedcharacter---------------------------------------------------------
(a <- raaa())

## ----showdissingle------------------------------------------------------------
x
s1(x)
sc(x)

## ----showdisdouble------------------------------------------------------------
list(d1(x),d2(x),dc(x))

## ----label=matrixindex--------------------------------------------------------
l <- letters[1:3]
(a <- aaa(s1=l,sc=1:3, d1=l,d2=rev(l),dc=3:1,t1=l,t2=l,t3=rev(l),tc=1:3))
a[cbind(l,l)]
a[cbind(rev(l),l,l)] <- 88
a

## ----makefoo------------------------------------------------------------------
x <- 3
class(x) <- "foo"
`*.foo` <- function(x,y){x + y + x}
print.foo <- function(x){print(unclass(x))}
c(`(x*x)*x` = (x*x)*x,  `x*(x*x)` = x*(x*x),  `x*x*x` = x*x*x)

