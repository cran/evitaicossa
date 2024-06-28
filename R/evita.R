setClass("aaa",
         slots = c(
             single_indeterminate_name1 = "character",
             single_indeterminate_coeff = "numeric"  ,
             double_indeterminate_name1 = "character",
             double_indeterminate_name2 = "character",
             double_indeterminate_coeff = "numeric"  ,
             triple_indeterminate_name1 = "character",
             triple_indeterminate_name2 = "character",
             triple_indeterminate_name3 = "character",
             triple_indeterminate_coeff = "numeric"
         ),
         validity = function(object){
             l1 <- c(
                 length(object@single_indeterminate_name1),
                 length(object@single_indeterminate_coeff)
             )
             l2 <- c(
                 length(object@double_indeterminate_name1),
                 length(object@double_indeterminate_name2),
                 length(object@double_indeterminate_coeff)
             )
             l3 <- c(
                 length(object@triple_indeterminate_name1),
                 length(object@triple_indeterminate_name2),
                 length(object@triple_indeterminate_name3),
                 length(object@triple_indeterminate_coeff)
             )
             stopifnot(min(l1)==max(l1))
             stopifnot(min(l2)==max(l2))
             stopifnot(min(l3)==max(l3))
         } )

`aaa` <-
    function(
             s1 = character(0), # single_indeterminate_name1
             sc = numeric(0)  , # single_indeterminate_coeff
             d1 = character(0), # double_indeterminate_name1
             d2 = character(0), # double_indeterminate_name2
             dc = numeric(0)  , # double_indeterminate_coeff
             t1 = character(0), # triple_indeterminate_name1
             t2 = character(0), # triple_indeterminate_name2
             t3 = character(0), # triple_indeterminate_name3
             tc = numeric(0)    # triple_indeterminate_coeff
             ){
        return(
            lavter(
                aaa_identity(
                    single_indeterminate_name1 = s1,
                    single_indeterminate_coeff = sc,
                    double_indeterminate_name1 = d1,
                    double_indeterminate_name2 = d2,
                    double_indeterminate_coeff = dc,
                    triple_indeterminate_name1 = t1,
                    triple_indeterminate_name2 = t2,
                    triple_indeterminate_name3 = t3,
                    triple_indeterminate_coeff = tc
                )
            )
        )
    }
       
lavter <- function(cout){   # "lavter" is "retval" in reverse

    sn <- matrix(cout$names1,nrow=1)
    dn <- matrix(cout$names2,nrow=2)
    tn <- matrix(cout$names3,nrow=3)
    
    new("aaa", # the only time new() is called
        single_indeterminate_name1 = sn[1,],
        single_indeterminate_coeff = cout$coeffs1,
        double_indeterminate_name1 = dn[1,],
        double_indeterminate_name2 = dn[2,],
        double_indeterminate_coeff = cout$coeffs2,
        triple_indeterminate_name1 = tn[1,],
        triple_indeterminate_name2 = tn[2,],
        triple_indeterminate_name3 = tn[3,],
        triple_indeterminate_coeff = cout$coeffs3
        ) }

single <- function(a){
    aaa(
        s1 = s1(a),
        sc = sc(a)
    )
}

double <- function(a){
    aaa(
        d1 = d1(a),
        d2 = d2(a),
        dc = dc(a)
    )
}

triple <- function(a){
    aaa(
        t1 = t1(a),
        t2 = t2(a),
        t3 = t3(a),
        tc = tc(a)
    )
}

`single<-` <- function(a,value){
    if(is.numeric(value)){  # single(a) <- 0
        if(value==0){
            value <- aaa()
        } else {
            stop("the only numeric value acceptable on the RHS is zero")
        }
    }
    if(!(is.zero(double(value)) && is.zero(triple(value)))){
        stop("double and triple parts of value must be zero")
    }

    aaa(
        s1 = s1(value),
        sc = sc(value),
        d1 = d1(a),
        d2 = d2(a),
        dc = dc(a),
        t1 = t1(a),
        t2 = t2(a),
        t3 = t3(a),
        tc = tc(a)
    )
}

`double<-` <- function(a,value){
    if(is.numeric(value)){  # double(a) <- 0
        if(value==0){
            value <- aaa()
        } else {
            stop("the only numeric value acceptable on the RHS is zero")
        }
    }
    if(!(is.zero(single(value)) && is.zero(triple(value)))){
        stop("single and triple parts of value must be zero")
    }

    aaa(
        s1 = s1(a),
        sc = sc(a),
        d1 = d1(value),
        d2 = d2(value),
        dc = dc(value),
        t1 = t1(a),
        t2 = t2(a),
        t3 = t3(a),
        tc = tc(a)
    )
}

`triple<-` <- function(a,value){

    if(is.numeric(value)){  # triple(a) <- 0
        if(value==0){
            value <- aaa()
        } else {
            stop("the only numeric value acceptable on the RHS is zero")
        }
    }
    if(!(is.zero(single(value)) && is.zero(double(value)))){
        stop("single and double parts of value must be zero")
    }

    aaa(
        s1 = s1(a),
        sc = sc(a),
        d1 = d1(a),
        d2 = d2(a),
        dc = dc(a),
        t1 = t1(value),
        t2 = t2(value),
        t3 = t3(value),
        tc = tc(value)
    )
}

getthings <- function(a){
    s1 <- a@single_indeterminate_name1
    sc <- a@single_indeterminate_coeff
    d1 <- a@double_indeterminate_name1
    d2 <- a@double_indeterminate_name2
    dc <- a@double_indeterminate_coeff
    t1 <- a@triple_indeterminate_name1
    t2 <- a@triple_indeterminate_name2
    t3 <- a@triple_indeterminate_name3
    tc <- a@triple_indeterminate_coeff

    h_single <- hashcal(list(s1,      sc),ultra_strict=FALSE)
    h_double <- hashcal(list(d1,d2,   dc),ultra_strict=FALSE)
    h_triple <- hashcal(list(t1,t2,t3,tc),ultra_strict=FALSE)
    
    list(
        s1 = disord(s1,h=h_single),
        sc = disord(sc,h=h_single),
        d1 = disord(d1,h=h_double),
        d2 = disord(d2,h=h_double),
        dc = disord(dc,h=h_double),
        t1 = disord(t1,h=h_triple),
        t2 = disord(t2,h=h_triple),
        t3 = disord(t3,h=h_triple),
        tc = disord(tc,h=h_triple)
    )   
}

setGeneric("s1",function(a){standardGeneric("s1")})
setGeneric("sc",function(a){standardGeneric("sc")})
setGeneric("d1",function(a){standardGeneric("d1")})
setGeneric("d2",function(a){standardGeneric("d2")})
setGeneric("dc",function(a){standardGeneric("dc")})
setGeneric("t1",function(a){standardGeneric("t1")})
setGeneric("t2",function(a){standardGeneric("t2")})
setGeneric("t3",function(a){standardGeneric("t3")})
setGeneric("tc",function(a){standardGeneric("tc")})

setMethod("s1",signature(a="aaa"),function(a){getthings(a)$s1})
setMethod("sc",signature(a="aaa"),function(a){getthings(a)$sc})
setMethod("d1",signature(a="aaa"),function(a){getthings(a)$d1})
setMethod("d2",signature(a="aaa"),function(a){getthings(a)$d2})
setMethod("dc",signature(a="aaa"),function(a){getthings(a)$dc})
setMethod("t1",signature(a="aaa"),function(a){getthings(a)$t1})
setMethod("t2",signature(a="aaa"),function(a){getthings(a)$t2})
setMethod("t3",signature(a="aaa"),function(a){getthings(a)$t3})
setMethod("tc",signature(a="aaa"),function(a){getthings(a)$tc})

`allsymbols` <- function(a){
    jj <- getthings(a)
    sort(unique(c(
        elements(jj$s1),
        elements(jj$d1),elements(jj$d2),
        elements(jj$t1),elements(jj$t2),elements(jj$t3)
    )))
}

"aaa_arith_aaa" <- function(e1,e2){
  switch(.Generic,
         "+" = aaa_plus_aaa(e1, e2),
         "-" = aaa_plus_aaa(e1,aaa_negative(e2)),
         "*" = aaa_prod_aaa(e1, e2),
         "/" = stop("aaa/aaa not defined"),
         "^" = stop("aaa^aaa not defined"),
         stop(gettextf("binary operator %s not defined for aaas", dQuote(.Generic)))
         )
}

"aaa_arith_numeric" <- function(e1,e2){  # e1 aaa, e2 numeric
  switch(.Generic,
         "+" = aaa_plus_numeric (e1, e2),
         "-" = aaa_plus_numeric (e1,-e2),
         "*" = aaa_prod_numeric (e1, e2),
         "/" = aaa_prod_numeric (e1, 1/e2),
         "^" = aaa_power_numeric(e1, e2),
         stop(gettextf("binary operator %s not defined for aaas", dQuote(.Generic)))
         )
}

"numeric_arith_aaa" <- function(e1,e2){ # e1 numeric, e2 aaa
  switch(.Generic,
         "+" = aaa_plus_numeric ( e2,e1),
         "-" = aaa_plus_numeric (-e2,e1),
         "*" = aaa_prod_numeric ( e2,e1),
         stop(gettextf("binary operator %s not defined in this case", dQuote(.Generic)))
         )
}

`aaa_negative` <- function(a){
    aaa(
        s1 =  elements(s1(a)),
        sc = -elements(sc(a)),
        d1 =  elements(d1(a)),
        d2 =  elements(d2(a)),
        dc = -elements(dc(a)),
        t1 =  elements(t1(a)),
        t2 =  elements(t2(a)),
        t3 =  elements(t3(a)),
        tc = -elements(tc(a))
    )
}

`aaa_plus_aaa` <- function(a,b){
        return(
            lavter(
                c_aaa_add(  # cf aaa_prod_add() below
                    F1_single_indeterminate_name1 = elements(s1(a)),
                    F1_single_indeterminate_coeff = elements(sc(a)),
                    F1_double_indeterminate_name1 = elements(d1(a)),
                    F1_double_indeterminate_name2 = elements(d2(a)),
                    F1_double_indeterminate_coeff = elements(dc(a)),
                    F1_triple_indeterminate_name1 = elements(t1(a)),
                    F1_triple_indeterminate_name2 = elements(t2(a)),
                    F1_triple_indeterminate_name3 = elements(t3(a)),
                    F1_triple_indeterminate_coeff = elements(tc(a)),
                    F2_single_indeterminate_name1 = elements(s1(b)),
                    F2_single_indeterminate_coeff = elements(sc(b)),
                    F2_double_indeterminate_name1 = elements(d1(b)),
                    F2_double_indeterminate_name2 = elements(d2(b)),
                    F2_double_indeterminate_coeff = elements(dc(b)),
                    F2_triple_indeterminate_name1 = elements(t1(b)),
                    F2_triple_indeterminate_name2 = elements(t2(b)),
                    F2_triple_indeterminate_name3 = elements(t3(b)),
                    F2_triple_indeterminate_coeff = elements(tc(b))
                )
            )
        )
}

`aaa_prod_aaa` <- function(a,b){
        return(
            lavter(
                c_aaa_prod(  # cf aaa_plus_aaa() above
                    F1_single_indeterminate_name1 = elements(s1(a)),
                    F1_single_indeterminate_coeff = elements(sc(a)),
                    F1_double_indeterminate_name1 = elements(d1(a)),
                    F1_double_indeterminate_name2 = elements(d2(a)),
                    F1_double_indeterminate_coeff = elements(dc(a)),
                    F1_triple_indeterminate_name1 = elements(t1(a)),
                    F1_triple_indeterminate_name2 = elements(t2(a)),
                    F1_triple_indeterminate_name3 = elements(t3(a)),
                    F1_triple_indeterminate_coeff = elements(tc(a)),
                    F2_single_indeterminate_name1 = elements(s1(b)),
                    F2_single_indeterminate_coeff = elements(sc(b)),
                    F2_double_indeterminate_name1 = elements(d1(b)),
                    F2_double_indeterminate_name2 = elements(d2(b)),
                    F2_double_indeterminate_coeff = elements(dc(b)),
                    F2_triple_indeterminate_name1 = elements(t1(b)),
                    F2_triple_indeterminate_name2 = elements(t2(b)),
                    F2_triple_indeterminate_name3 = elements(t3(b)),
                    F2_triple_indeterminate_coeff = elements(tc(b))
                )
            )
        )
}

`aaa_equal_aaa` <- function(a,b){
        return(
            c_aaa_equal(  # cf aaa_plus_aaa() above
                F1_single_indeterminate_name1 = elements(s1(a)),
                F1_single_indeterminate_coeff = elements(sc(a)),
                F1_double_indeterminate_name1 = elements(d1(a)),
                F1_double_indeterminate_name2 = elements(d2(a)),
                F1_double_indeterminate_coeff = elements(dc(a)),
                F1_triple_indeterminate_name1 = elements(t1(a)),
                F1_triple_indeterminate_name2 = elements(t2(a)),
                F1_triple_indeterminate_name3 = elements(t3(a)),
                F1_triple_indeterminate_coeff = elements(tc(a)),
                F2_single_indeterminate_name1 = elements(s1(b)),
                F2_single_indeterminate_coeff = elements(sc(b)),
                F2_double_indeterminate_name1 = elements(d1(b)),
                F2_double_indeterminate_name2 = elements(d2(b)),
                F2_double_indeterminate_coeff = elements(dc(b)),
                F2_triple_indeterminate_name1 = elements(t1(b)),
                F2_triple_indeterminate_name2 = elements(t2(b)),
                F2_triple_indeterminate_name3 = elements(t3(b)),
                F2_triple_indeterminate_coeff = elements(tc(b))
            )
        )
}

`aaa_prod_numeric`  <- function(a,b){
    aaa(
        s1 = elements(s1(a))  ,
        sc = elements(sc(a))*b,
        d1 = elements(d1(a))  ,
        d2 = elements(d2(a))  ,
        dc = elements(dc(a))*b,
        t1 = elements(t1(a))  ,
        t2 = elements(t2(a))  ,
        t3 = elements(t3(a))  ,
        tc = elements(tc(a))*b
    )
}

`aaa_plus_numeric`  <- function(a,b){
    stop("there are no scalars in antiassociative algebras (except zero)")
}

`aaa_power_numeric` <- function(e1,e2){ # e1^e2
    if(e2==1){
        return(e1)
    } else if(e2==2){
        return(e1*e1)
    } else if(e2>3){
        return(aaa())
    } else {
        stop("powers not defined in this case")
    }
}

setMethod("+", signature(e1 = "aaa", e2 = "missing"), function(e1,e2){e1              })
setMethod("-", signature(e1 = "aaa", e2 = "missing"), function(e1,e2){aaa_negative(e1)})

setMethod("Arith",signature(e1 = "aaa"  , e2="aaa"    ), aaa_arith_aaa     )
setMethod("Arith",signature(e1 = "aaa"  , e2="numeric"), aaa_arith_numeric )
setMethod("Arith",signature(e1 = "numeric", e2="aaa"  ), numeric_arith_aaa )

`as.aaa` <- function(s){
    if(is.list(s)){
        thing_to_aaa(s)
    } else {
        aaa(s           ,                          rep(1,length(s)),
            character(0),character(0),             numeric(0),
            character(0),character(0),character(0),numeric(0)
            )
    }
}    
    
thing_to_aaa <- function(L){
    aaa(
        L$s1,          L$sc,
        L$d1,L$d2,     L$dc,
        L$t1,L$t2,L$t3,L$tc
        )
}
    
`raaa` <- function(n=4, s=3){
    rc <- function(...){sample(letters[seq_len(n)],s,replace=TRUE)}
    rn <- function(...){sample(        seq_len(n) ,s,replace=TRUE)}
    
    aaa(
        rc(),          rn(),
        rc(),rc()     ,rn(),
        rc(),rc(),rc(),rn()
        )
}

`raaaa` <- function(n=10 ,s=30){raaa(n=n,s=s)}

`is.zero` <- function(x){(length(sc(x))==0) && (length(dc(x))==0) && (length(tc(x))==0)}

`aaa_compare_aaa` <- function(e1,e2){
  switch(.Generic,
         "==" =  aaa_equal_aaa(e1,e2),
         "!=" = !aaa_equal_aaa(e1,e2),
         stop(gettextf("comparison operator %s not defined for aaa objects", dQuote(.Generic)))
         )
}

`aaa_compare_error` <- function(e1,e2){
    stop(gettextf("comparison operator %s not defined in this case", dQuote(.Generic)))
}

setMethod("Compare",signature(e1 = "aaa"     , e2="aaa"    ), aaa_compare_aaa    )
setMethod("Compare",signature(e1 = "aaa"     , e2="numeric"), aaa_compare_error  )
setMethod("Compare",signature(e1 = "numeric" , e2="aaa"    ), aaa_compare_error  )
setMethod("Compare",signature(e1 = "ANY"     , e2="aaa"    ), aaa_compare_error  )
setMethod("Compare",signature(e1 = "aaa"     , e2="ANY"    ), aaa_compare_error  )


linear1 <- function(x){aaa(s1=letters[seq_along(x)],sc=x)}
linear2 <- function(x){aaa(d1 =letters[seq_along(x)],
                           d2 =letters[seq_along(x)],
                           dc = x)
}

linear3 <- function(x){aaa(t1 =letters[seq_along(x)],
                           t2 =letters[seq_along(x)],
                           t3 =letters[seq_along(x)],
                           tc = x)
}

extracter <- function(a,
                      s1 = character(0),
                      d1 = character(0),
                      d2 = character(0),
                      t1 = character(0),
                      t2 = character(0),
                      t3 = character(0)
                      ){
    return(lavter(c_aaa_extract( # the meat
        F1_single_indeterminate_name1 = elements(s1(a)),
        F1_single_indeterminate_coeff = elements(sc(a)),
        F1_double_indeterminate_name1 = elements(d1(a)),
        F1_double_indeterminate_name2 = elements(d2(a)),
        F1_double_indeterminate_coeff = elements(dc(a)),
        F1_triple_indeterminate_name1 = elements(t1(a)),
        F1_triple_indeterminate_name2 = elements(t2(a)),
        F1_triple_indeterminate_name3 = elements(t3(a)),
        F1_triple_indeterminate_coeff = elements(tc(a)),
        s1, d1, d2, t1, t2, t3)))
}

`overwriter` <- function(a,
                      s1 = character(0),
                      sc = numeric(0)  ,
                      d1 = character(0),
                      d2 = character(0),
                      dc = numeric(0)  ,
                      t1 = character(0),
                      t2 = character(0),
                      t3 = character(0),
                      tc = numeric(0),
                      value
                      ){
    return(lavter(c_aaa_overwriter( # the meat
        F1_single_indeterminate_name1 = elements(s1(a)),
        F1_single_indeterminate_coeff = elements(sc(a)),
        F1_double_indeterminate_name1 = elements(d1(a)),
        F1_double_indeterminate_name2 = elements(d2(a)),
        F1_double_indeterminate_coeff = elements(dc(a)),
        F1_triple_indeterminate_name1 = elements(t1(a)),
        F1_triple_indeterminate_name2 = elements(t2(a)),
        F1_triple_indeterminate_name3 = elements(t3(a)),
        F1_triple_indeterminate_coeff = elements(tc(a)),
        s1,         sc,
        d1, d2,     dc,
        t1, t2, t3, tc,
        value
    )))
}

setMethod("[", signature(x="aaa"),
          function(x, ...){
              jj <- list(...)
              if(length(jj) == 0){return(x)}  # a[] returns a
              return(extracter(x,
                     as.character(jj$s1),
                     as.character(jj$d1), as.character(jj$d2),
                     as.character(jj$t1), as.character(jj$t2), as.character(jj$t3)
                     ))
          } )

setMethod("[", signature(x="aaa",i="character"),
          function(x, i, ...){
              if(length(i) == 0){
                  return(x)
              } else if(length(i)>3){
                  return(aaa())
              } else if(length(i) == 1){
                  return(extracter(x,s1=as.character(i[1])                                            ))
              } else if(length(i) == 2){
                  return(extracter(x,d1=as.character(i[1]),d2=as.character(i[2])                      ))
              } else if(length(i) == 3){
                  return(extracter(x,t1=as.character(i[1]),t2=as.character(i[2]),t3=as.character(i[3])))
              } else {
                  stop("this cannot happen")
              }
          } )

setMethod("[", signature(x="aaa",i="matrix"),
          function(x, i, ...){
              stopifnot(is.character(i))
              if(ncol(i) == 1){
                  return(x[s1=i[,1]])
              } else if(ncol(i) == 2){
                  return(x[d1=i[,1],d2=i[,2]])
              } else if(ncol(i) == 3){
                  return(x[t1=i[,1],t2=i[,2],t3=i[,3]])
              } else {
                  stop("index matrix must have 1,2 or 3 columns")
              }
          } )

setReplaceMethod("[", signature("aaa",i="missing"),    # a[d1='a',d2='c'] <- 33
          function(x, i, ..., value){ # NB argument i is _missing_
              stopifnot(is.numeric(value))
              stopifnot(length(value) == 1)
              jj <- list(...)


              if(length(jj) == 0){  # a[] <- 22
                  return(aaa(
                      s1 = elements(s1(x)),
                      sc = rep(value,length(s1(x))), # the meat 
                      d1 = elements(d1(x)),
                      d2 = elements(d2(x)),
                      dc = rep(value,length(d1(x))), # the meat
                      t1 = elements(t1(x)),
                      t2 = elements(t2(x)),
                      t3 = elements(t3(x)),
                      tc = rep(value,length(t1(x)))  # the meat
                      ))
              } else {
                  return(overwriter(x,
                      s1 = as.character(jj$s1),
                      sc = as.numeric  (jj$sc),
                      d1 = as.character(jj$d1),
                      d2 = as.character(jj$d2),
                      dc = as.numeric  (jj$dc),
                      t1 = as.character(jj$t1),
                      t2 = as.character(jj$t2),
                      t3 = as.character(jj$t3),
                      tc = as.numeric  (jj$tc),
                      value # the meat
                  ))                      
              }
          } )

setReplaceMethod("[", signature("aaa",i="character",j="missing",value="numeric"),
          function(x,i,j,value){
              if(length(i) == 1){ # a["c"] <- 888
                  return(overwriter(x,
                                    s1 = as.character(i[1]),
                                    sc = sc(x),
                                    value = value # the meat
                                    ))
              } else if(length(i) == 2){ # a[c("c","d")] <- 888
                  return(overwriter(x,
                                    d1 = as.character(i[1]),
                                    d2 = as.character(i[2]),
                                    dc = dc(x),
                                    value = value # the meat
                                    ))
                  } else if (length(i) == 3){ # a[c("c","d","e")] <- 888
                  return(overwriter(x,
                                    t1 = as.character(i[1]),
                                    t2 = as.character(i[2]),
                                    t3 = as.character(i[3]),
                                    tc = tc(x),
                                    value = value # the meat
                                    ))
                  } else {
                  stop("index argument must be length 1, 2, or 3")
              }
          } )

setReplaceMethod("[", signature(x="aaa",i="matrix"),
          function(x, i, ..., value){
              stopifnot(is.character(i))
              if(ncol(i) == 1){
                  x[s1=i[,1]] <- value
              } else if(ncol(i) == 2){
                  x[d1=i[,1],d2=i[,2]] <- value
              } else if(ncol(i) == 3){
                  x[t1=i[,1],t2=i[,2],t3=i[,3]] <- value
              } else {
                  stop("index matrix must have 1,2 or 3 columns")
              }
              return(x)
          } )

setMethod("[<-", signature("aaa",i="disord",j="missing",value="numeric"),
          function(x,i,j,value){
              stop("not implemented yet")
          } )

