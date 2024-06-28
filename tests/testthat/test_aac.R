# This test suite checks identities that are correct *only* if k = -1

test_that("Test suite aac.R",{  

checker3 <- function(x,y,z){
    expect_true(x*(y*z) == -(x*y)*z, info=list(x,y,z)) } # The sine qua non of the whole package!

checker3a <- function(a,b,x){ expect_true( (a+a*x)*(b+x*b) == a*b) }

for(i in 1:2){
    checker3 (raaa(),raaa(),raaa())
    checker3a(raaa(),raaa(),raaa())
}

checker3(raaa(),raaa(),aaa())
checker3(raaa(),aaa(),aaa())
checker3(aaa(),aaa(),aaa())

checker3a(raaa(),raaa(),aaa())
checker3a(raaa(),aaa(),aaa())
checker3a(aaa(),aaa(),aaa())


`a` <- as.aaa(list(
    s1 = c("a", "b", "d"),
    sc = c( 3 ,  4 ,  4 ),
    d1 = c("c", "d", "d"),
    d2 = c("a", "c", "d"),
    dc = c( 4 ,  1 ,  4 ),
    t1 = c("b", "c", "d"),
    t2 = c("b", "b", "c"),
    t3 = c("d", "c", "a"),
    tc = c( 3 ,  2 ,  4 )))

`b` <- as.aaa(list(
    s1 = c("a", "b", "c"),
    sc = c( 1 ,  1 ,  3 ),
    d1 = c("b", "c"), 
    d2 = c("c", "d"),
    dc = c( 5 ,  4 ),
    t1 = c("a", "b", "c"),
    t2 = c("d", "a", "d"),
    t3 = c("b", "d", "c"),
    tc = c( 3 ,  3 ,  2 )))

`a*b` <- as.aaa(list(s1 = character(0), sc = numeric(0),
           d1 = c("a", "a", "a", "b", "b", "b", "d", "d", "d"),
           d2 = c("a", "b", "c", "a", "b", "c", "a", "b", "c"),
           dc = c( 3 ,  3 ,  9 ,  4 ,  4 , 12 ,  4 ,  4 , 12 ),
           t1 = c("a", "a", "b", "b", "c", "c", "c", "d", "d", "d", "d", "d", "d", "d", "d"),
           t2 = c("b", "c", "b", "c", "a", "a", "a", "b", "c", "c", "c", "c", "d", "d", "d"),
           t3 = c("c", "d", "c", "d", "a", "b", "c", "c", "a", "b", "c", "d", "a", "b", "c"),
           tc = c(-15, -12, -20, -16, 4, 4, 12, -20, 1, 1, 3, -16, 4, 4, 12)))

`b*a` <- as.aaa(list(s1 = character(0), sc = numeric(0),
           d1 = c("a", "a", "a", "b", "b", "b", "c", "c", "c"),
           d2 = c("a", "b", "d", "a", "b", "d", "a", "b", "d"),
           dc = c( 3 ,  4 ,  4 ,  3 ,  4 ,  4 ,  9 , 12 , 12 ),
           t1 = c("a", "a", "a", "b", "b", "b", "b", "b", "c", "c", "c", "c", "c"), 
           t2 = c("c", "d", "d", "c", "c", "c", "d", "d", "c", "d", "d", "d", "d"),
           t3 = c("a", "c", "d", "a", "b", "d", "c", "d", "a", "a", "b", "c", "d"),
           tc = c(-4, -1, -4, 11, 20, 20, -1, -4, -12, 12, 16, -3, 4)))

`a+a*b` <- as.aaa(list(
    s1 = c("a", "b", "d"),
    sc = c( 3 ,  4 ,  4 ),
    d1 = c("a", "a", "a", "b", "b", "b", "c", "d", "d", "d", "d"),
    d2 = c("a", "b", "c", "a", "b", "c", "a", "a", "b", "c", "d"),
    dc = c( 3 ,  3 ,  9 ,  4 ,  4 , 12 ,  4 ,  4 ,  4 , 13 ,  4 ),
    t1 = c("a", "a", "b", "b", "b", "c", "c", "c", "c", "d", "d", "d", "d", "d", "d", "d", "d"),
    t2 = c("b", "c", "b", "b", "c", "a", "a", "a", "b", "b", "c", "c", "c", "c", "d", "d", "d"),
    t3 = c("c", "d", "c", "d", "d", "a", "b", "c", "c", "c", "a", "b", "c", "d", "a", "b", "c"),
    tc = c(-15, -12, -20,  3, -16 ,  4 ,  4 , 12 ,  2 , -20,  5 ,  1 ,  3 , -16,  4 ,  4 , 12 )))


`b+b*a` <- as.aaa(list(
    s1 = c("a", "b", "c"),
    sc = c( 1 ,  1 ,  3 ),
    d1 = c("a", "a", "a", "b", "b", "b", "b", "c", "c", "c"),
    d2 = c("a", "b", "d", "a", "b", "c", "d", "a", "b", "d"),
    dc = c( 3 ,  4 ,  4 ,  3 ,  4 ,  5 ,  4 ,  9 , 12 , 16 ),
    t1 = c("a", "a", "a", "a", "b", "b", "b", "b", "b", "b", "c", "c", "c", "c", "c"),
    t2 = c("c", "d", "d", "d", "a", "c", "c", "c", "d", "d", "c", "d", "d", "d", "d"),
    t3 = c("a", "b", "c", "d", "d", "a", "b", "d", "c", "d", "a", "a", "b", "c", "d"),
    tc = c(-4 ,  3 , -1 , -4 ,  3 , 11 , 20 , 20 , -1 , -4 , -12, 12 , 16 , -1 ,  4 )))

expect_true(a*b == `a*b`)
expect_true(b*a == `b*a`)

expect_true(a+a*b == `a+a*b`)
expect_true(b+b*a == `b+b*a`)

expect_true(extracter(a,c("a","b")) == aaa(s1=c("a","b"),sc=3:4))
expect_true(extracter(a,c("a","b")) == aaa(s1=c("b","a"),sc=4:3))
expect_true(a["d"] == as.aaa("d")*4)

} )
