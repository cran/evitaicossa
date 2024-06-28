setMethod("show", "aaa",
          function(object){
              if(is.zero(object)){
                  cat("the zero free antiassociative algebra element.\n")
              } else {
                  cat("free antiassociative algebra element:\n")
                  cat(paste(strwrap(aaa_show(object), getOption("width")), collapse="\n"))
                  cat("\n")
              }
          } )

putsign <- function(x){paste(ifelse(x>0,"+","-"),abs(x),sep="")}  # put sign in

aaa_show <- function(a){
    noquote(paste(
        single_string(a),
        double_string(a),
        triple_string(a),
        sep=" "))
}

single_string <- function(a){
    sc <- elements(sc(a))
    s1 <- elements(s1(a))
    if(length(sc)==0){return("")}
    paste(paste(putsign(sc),s1,sep=""),collapse=" ")
}

double_string <- function(a){
    dc <- elements(dc(a))
    d1 <- elements(d1(a))
    d2 <- elements(d2(a))
    if(length(dc)==0){return("")}
    paste(paste(putsign(dc),d1,".",d2,sep=""),collapse=" ")
}

triple_string <- function(a){
    tc <- elements(tc(a))
    t1 <- elements(t1(a))
    t2 <- elements(t2(a))
    t3 <- elements(t3(a))
    if(length(tc)==0){return("")}
    paste(paste(putsign(tc),"(",t1,".",t2,")",t3,sep=""),collapse=" ")
}
