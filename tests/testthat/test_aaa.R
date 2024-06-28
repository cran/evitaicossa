## This file follows the structure of aaa.R in the free group package.

## Define some checker functions, and call them at the end.  They
## should all return TRUE if the package works, and stop with error if
## a test is failed.  Function checker1() has one argument, checker2()
## two, and so on.  These checks should pass whatever the value of k

test_that("Test suite aaa.R",{

checker1 <- function(x){

    expect_error(x <  x)
    expect_error(x >  x)
    expect_error(x <= x)
    expect_error(x >= x)

    expect_error(x == 0)
    expect_error(x == 3)
    expect_error(x/x)
    expect_error(x^x)
    expect_error(x^(-1))
  
    expect_error(x+1)
    expect_error(x-1)
    expect_error(1+x)
    expect_error(1-x)

    expect_true(x == x, info=x)
    expect_true(x == +x, info=x)
    expect_true(+x == x, info=x)
    expect_true(x == -(-x), info=x)
  
    expect_false( x != x, info=x)

    expect_true(x+x == 2*x, info=x)
    expect_true(x+x == x*2, info=x)
    expect_true(x+x-x == x, info=x)

    expect_true(is.zero(x-x), info=x)
    expect_true(is.zero(x*0), info=x)

    expect_true(x/2 + x/2 == x, info=x)
  
    expect_error(x^0, info=x)
    expect_true(x^1 == x, info=x)
    expect_true(x^2 == x*x, info=x)
    expect_error(x^3, info=x)
    expect_true(is.zero(x^4), info=x)
    expect_true(is.zero(x^5), info=x)
    expect_true(is.zero(x^6), info=x)

    expect_output(print(x))

    expect_true(is.zero(x*linear3(sample(9))))

    expect_true(as.aaa(getthings(x)) == x)

    expect_true(single(x) + double(x) + triple(x) == x)

    expect_true(aaa(s1=s1(x),                  sc=sc(x)) == single(x))
    expect_true(aaa(d1=d1(x),d2=d2(x),         dc=dc(x)) == double(x))
    expect_true(aaa(t1=t1(x),t2=t2(x),t3=t3(x),tc=tc(x)) == triple(x))

    jj <- x
    single(jj) <- 0
    expect_true(is.zero(single(jj)))

    jj <- x
    double(jj) <- 0
    expect_true(is.zero(double(jj)))

    jj <- x
    triple(jj) <- 0
    expect_true(is.zero(triple(jj)))

    as <- allsymbols(x)
    
    expect_true(x[cbind(as)] == single(x))

    expect_true(is.zero(x[letters[1:4]]))
    expect_true(x[character(0)] == x)

    if(is.zero(x)){
        expect_true(single(x) == x)
        expect_true(double(x) == x)
        expect_true(triple(x) == x)
    } else {
        expect_true(x[as.matrix(expand.grid(as      ))] == single(x))
        expect_true(x[as.matrix(expand.grid(as,as   ))] == double(x))
        expect_true(x[as.matrix(expand.grid(as,as,as))] == triple(x))
    }

    expect_error(x[matrix(letters[1:25],5,5)])
    expect_error(x[matrix(letters[1:25],5,5)] <- 33)
    return(TRUE)
}  # checker1() closes


checker2 <- function(x,y){
  expect_true(x == -y+x+y, info=list(x,y))
  expect_true(x+y == x-(-y), info=list(x,y))

  expect_true(x+y == y+x, info=list(x,y))

  expect_true((-x)*y == -(x*y), info=list(x,y))
  expect_true(x*(-y) == -(x*y), info=list(x,y))

  jj <- x
  single(jj) <- single(y)
  expect_true(single(jj) == single(y))

  jj <- x
  double(jj) <- double(y)
  expect_true(double(jj) == double(y))

  jj <- x
  triple(jj) <- triple(y)
  expect_true(triple(jj) == triple(y))

  return(TRUE)
}


checker3 <- function(x,y,z){
  expect_true(x+(y+z) == (x+y)+z, info=list(x,y,z)) # additive associativity

  expect_true(x*(y+z) == x*y + x*z, info=list(x,y,z))  # left distributivity
  expect_true((y+z)*x == y*x + z*x, info=list(x,y,z))  # right distributivity
  return(TRUE)
} # checker3() closes

checker4 <- function(x,y,z,p){
    expect_true(is.zero(x*y*z*p))
}


for(i in 1:1){
    x <- raaa()
    y <- raaa()
    z <- raaa()
    p <- raaa()
    
    checker1(x)
    checker2(x,y)
    checker3(x,y,z)
    checker4(x,y,z,p)

    checker1(raaaa())
}

checker1(aaa())


})
