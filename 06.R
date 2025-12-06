library(magrittr)
test=fread("06.test", header=FALSE)
inp=fread("06.inp", header=FALSE)

f=function(test) {
    ops=tail(test,1)
    nums=head(test, -1)  %>%
        lapply(., as.numeric)
    ans=ifelse(ops=="+", sapply(nums, sum), sapply(nums, prod))
    sum(ans)
}

f(test)
f(inp)

## part 2
test2=scan("06.test", what=character(), sep="\n")
inp2=scan("06.inp", what=character(), sep="\n")

f=function(test, test2) {
    a=head(test2,-1)  %>% strsplit(.,"")  %>% do.call("rbind",.)  %>% apply(., 2,as.numeric)
    ## split problems
    w=apply(is.na(a), 2, all)  %>% which()  %>% c(0,., ncol(a)+1)
    ops=tail(test,1)  %>% as.vector()
    result=numeric(length(ops))
    for(i in seq_along(ops)) {
        sub=a[,(w[i]+1):(w[i+1]-1), drop=FALSE]  %>%
            apply(., 2, paste, collapse="")  %>%
            gsub("NA","",.)  %>%
            as.numeric()
        result[i]=if(ops[i]=="+") sum(sub, na.rm=TRUE) else prod(sub, na.rm=TRUE)
    }
    sum(result)
}

f(test, test2)
f(inp, inp2)
