library(magrittr)

test=scan("5.test", what=character(), blank.lines.skip=FALSE)
inp=scan("5.inp", what=character(), blank.lines.skip=FALSE)

f=function(test) {
    ## split into fresh and available
    w=which(test == "")
    fresh=test[1:(w-1)]  %>% strsplit(., "-")  %>%
        lapply(., as.numeric)  %>%
        unique()  
    available=test[(w+1):length(test)]  %>% as.numeric()
    ## what is fresh and available?
    ok=rep(FALSE,length(available))
    for(i in seq_along(fresh)) {
        ok=ok | (available >= fresh[[i]][1] & available <= fresh[[i]][2])
    }
    ok
}
f(test) %>% sum()
f(inp) %>% sum() 

## part 2
getfresh=function(inp) {
    w=which(inp == "")
    fresh=inp[1:(w-1)]  %>% strsplit(., "-")  %>%
        lapply(., as.numeric)  %>%
        unique()  %>%
        do.call(rbind, .)
}

library(Rcpp)
Rcpp::sourceCpp("05.cpp")

fresh=getfresh(test)
mx=unlist(fresh)  %>% max()
mn=unlist(fresh)  %>% min()
count(mn, mx, fresh)

fresh=getfresh(inp)
mx=unlist(fresh)  %>% max()
mn=unlist(fresh)  %>% min()

fresh=fresh - mn
mn=mn - mn
mx=mx - mn

## count(mn, mx, fresh)

## too slow. try and be more elegant
getfresh=function(inp) {
    w=which(inp == "")
    fresh=inp[1:(w-1)]  %>% strsplit(., "-")  %>%
        lapply(., as.numeric)  %>%
        unique()  
    o=order(sapply(fresh, "[[", 1))
    fresh[o]
}

fresh=getfresh(test)
fresh=getfresh(inp)

## merge any that overlap
f=function(fresh) {
    for(i in length(fresh):2) {
        ## i overlaps i-1 or is contained in i-1
        if(fresh[[i]][1] <= fresh[[i-1]][2]) {
        fresh[[i-1]][2]=max(fresh[[i-1]][2], fresh[[i]][2])
        fresh[[i]]=NULL
        next
        }
    }
    fresh
}

old=Inf
n=length(fresh)
while(n < old) {
    old=length(fresh)
    fresh=f(fresh)
    n=length(fresh)
}

length(fresh)
l=sapply(fresh, "[[", 1)
r=sapply(fresh, "[[", 2)
options(digits=13)
sum(r - l + 1, na.rm=TRUE)

