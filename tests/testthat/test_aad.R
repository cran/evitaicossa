# This test suite does some spot-checks that catch some now hopefully
# corrected bugs in teh extraction routines

test_that("Test suite aad.R",{  

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

expect_true(extracter(a,c("a","b")) == aaa(s1=c("a","b"),sc=3:4))
expect_true(extracter(a,c("a","b")) == aaa(s1=c("b","a"),sc=4:3))
expect_true(a["d"] == as.aaa("d")*4)
jj <- a
jj[d1=letters[1:3],d2=letters[1:3]] <- 888
expect_true(jj == as.aaa(list(
                      s1 = c("a", "b", "d"),
                      sc = c( 3 ,  4 ,  4 ),
                      d1 = c("c", "d", "d",letters[1:3]),
                      d2 = c("a", "c", "d",letters[1:3]),
                      dc = c( 4 ,  1 ,  4, 888,888,888),
                      t1 = c("b", "c", "d"),
                      t2 = c("b", "b", "c"),
                      t3 = c("d", "c", "a"),
                      tc = c( 3 ,  2 ,  4 ))))

expect_true(a[] == a)
expect_true(
    a[s1="a",t1=c("b","c"),t2=c("b","b"),t3=c("d","c")] ==
    aaa(
        s1=c("a"    ),                            sc=3,
        t1=c("b","c"),t2=c("b","b"),t3=c("d","c"),tc=3:2)
    )


expect_true(a["b"] == aaa(s1="b",sc=4))
expect_true(a[c("d", "d")] == aaa(d1="d",d2="d",dc=4))
expect_true(a[c("d", "c","a")] == aaa(t1="d",t2="c",t3="a",tc=4))

jj <- a
jj[] <- 5
expect_true(
    jj ==  as.aaa(list(
               s1 = c("a", "b", "d"),
               sc = c( 5 ,  5 ,  5 ),
               d1 = c("c", "d", "d"),
               d2 = c("a", "c", "d"),
               dc = c( 5 ,  5 ,  5 ),
               t1 = c("b", "c", "d"),
               t2 = c("b", "b", "c"),
               t3 = c("d", "c", "a"),
               tc = c( 5 ,  5 ,  5 )
           ))
)

jj <- a
jj["d"] <- 88
expect_true(jj ==  as.aaa(list(
    s1 = c("a", "b", "d"),
    sc = c( 3 ,  4 ,  88),
    d1 = c("c", "d", "d"),
    d2 = c("a", "c", "d"),
    dc = c( 4 ,  1 ,  4 ),
    t1 = c("b", "c", "d"),
    t2 = c("b", "b", "c"),
    t3 = c("d", "c", "a"),
    tc = c( 3 ,  2 ,  4 )))
    )

jj <- a
jj["x"] <- 88
expect_true(jj ==  as.aaa(list(
    s1 = c("a", "b", "d", "x"),
    sc = c( 3 ,  4 ,  4 ,  88),
    d1 = c("c", "d", "d"),
    d2 = c("a", "c", "d"),
    dc = c( 4 ,  1 ,  4 ),
    t1 = c("b", "c", "d"),
    t2 = c("b", "b", "c"),
    t3 = c("d", "c", "a"),
    tc = c( 3 ,  2 ,  4 )))
    )

jj <- a
jj[c("c","a")] <- 88
expect_true(jj ==  as.aaa(list(
    s1 = c("a", "b", "d"),
    sc = c( 3 ,  4 ,  4 ),
    d1 = c("c", "d", "d"),
    d2 = c("a", "c", "d"),
    dc = c(88 ,  1 ,  4 ),
    t1 = c("b", "c", "d"),
    t2 = c("b", "b", "c"),
    t3 = c("d", "c", "a"),
    tc = c( 3 ,  2 ,  4 )))
    )


jj <- a
jj[c("c","b","c")] <- 88
expect_true(jj == as.aaa(list(
    s1 = c("a", "b", "d"),
    sc = c( 3 ,  4 ,  4 ),
    d1 = c("c", "d", "d"),
    d2 = c("a", "c", "d"),
    dc = c( 4 ,  1 ,  4 ),
    t1 = c("b", "c", "d"),
    t2 = c("b", "b", "c"),
    t3 = c("d", "c", "a"),
    tc = c( 3 , 88 ,  4 ))))

jj <- a 
expect_error(jj[c("d","c","a","a")] <- 888)


jj <- a
jj[d1=c("d","a","x"),d2=c("c","c","x")] <- 55
expect_true(jj == as.aaa(list(
    s1 = c("a", "b", "d"),
    sc = c( 3 ,  4 ,  4 ),
    d1 = c("c", "d", "d", "a", "x"),
    d2 = c("a", "c", "d", "c", "x"),
    dc = c( 4 , 55 ,  4 , 55 , 55 ),
    t1 = c("b", "c", "d"),
    t2 = c("b", "b", "c"),
    t3 = c("d", "c", "a"),
    tc = c( 3 ,  2 ,  4 ))))


jj <- a
jj[rbind(
    "a",
    "x"
)] <- 55
expect_true(
    jj == as.aaa(list(
              s1 = c("a", "b", "d", "x"),
              sc = c(55 ,  4 ,  4 , 55 ),
              d1 = c("c", "d", "d"),
              d2 = c("a", "c", "d"),
              dc = c( 4 ,  1 ,  4 ),
              t1 = c("b", "c", "d"),
              t2 = c("b", "b", "c"),
              t3 = c("d", "c", "a"),
              tc = c( 3 ,  2 ,  4 )))
)


jj <- a
jj[rbind(
    c("d", "c"),
    c("a", "c"),
    c("x", "x")
)] <- 55
expect_true(
    jj == as.aaa(list(
              s1 = c("a", "b", "d"),
              sc = c( 3 ,  4 ,  4 ),
              d1 = c("c", "d", "d", "a", "x"),
              d2 = c("a", "c", "d", "c", "x"),
              dc = c( 4 , 55 ,  4 , 55 , 55 ),
              t1 = c("b", "c", "d"),
              t2 = c("b", "b", "c"),
              t3 = c("d", "c", "a"),
              tc = c( 3 ,  2 ,  4 )))
)

jj <- a
jj[rbind(
    c("b", "b", "d"),
    c("d", "d", "a")
)] <- 88
expect_true(jj == as.aaa(list(
                      s1 = c("a", "b", "d"),
                      sc = c( 3 ,  4 ,  4 ),
                      d1 = c("c", "d", "d"),
                      d2 = c("a", "c", "d"),
                      dc = c( 4 ,  1 ,  4 ),
                      t1 = c("b", "c", "d", "d"),
                      t2 = c("b", "b", "c", "d"),
                      t3 = c("d", "c", "a", "a"),
                      tc = c( 88,  2 ,  4 ,  88)))
            )

expect_error(single(a) <- 1)
expect_error(double(a) <- 1)
expect_error(triple(a) <- 1)

} )
