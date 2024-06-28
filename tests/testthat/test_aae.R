# This test suite does some spot-checks that catch some now hopefully
# corrected bugs in teh extraction routines

test_that("Test suite aae.R",{  

    expect_error(aaa(s1=c("a","b"),sc=1:3))
    expect_error(aaa(d1=c("a","b"),d2=c("a","b"),dc=1:3))
    expect_error(aaa(d1=c("a","b"),d2=c("a","b","c"),dc=1:2))
    expect_error(aaa(t1=c("a","b"),t2=c("a","b"),t3=c("a","b"),tc=1:3))
    expect_error(aaa(t1=c("a","b","c"),t2=c("a","b"),t3=c("a","b"),tc=1:2))
    } )


    
