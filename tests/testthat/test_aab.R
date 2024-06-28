## These checks should pass whatever the value of k

test_that("Test suite aab.R",{ 

checker <- function(a,b,x,y){

    expect_true(linear1(a*x + b*y) == a*linear1(x) + b*linear1(y))
    expect_true(linear2(a*x + b*y) == a*linear2(x) + b*linear2(y))
    expect_true(linear3(a*x + b*y) == a*linear3(x) + b*linear3(y))

    return(TRUE)
}  # checker1() closes

for(i in 1:2){
    checker(sample(9,1),sample(9,1),sample(9),sample(9))
}


expect_true(as.aaa(letters) == linear1(rep(1,26)))



})
