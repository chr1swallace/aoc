library(magrittr)
test=scan("07.test", what=character(), sep="\n")  %>% strsplit(., "")  %>% do.call("rbind", .)
inp=scan("07.inp", what=character(), sep="\n")  %>% strsplit(., "")  %>% do.call("rbind", .)

f=function(test) {
    ## start position
    st=which(test[1,] == "S")
    ## beam pos
    beam=st
    total_splits=0
    ## next row: split or pass?
    for(i in 2:nrow(test)) {
        sp=ifelse(test[i,beam] == "^", TRUE, FALSE)
        ## print(beam)
        ## print(sp)
        total_splits=total_splits + sum(sp)
        beam=lapply(beam, function(j) if(test[i,j] == "^") { c(j-1, j+1) } else { j })  %>%
            unlist() %>%
            sort ()  %>% 
            unique()
    }

    total_splits 
}

f(test)
f(inp)

## part 2

g=function(test) {
    ## start position
    st=which(test[1,] == "S")
    ## beam pos
    beam=st
    counts=1
    ## next row: every valid split gives two paths - just don't uniquify the beams
    for(i in 2:nrow(test)) {
        sp=ifelse(test[i,beam] == "^", TRUE, FALSE)
        beam=lapply(beam, function(j) if(test[i,j] == "^") { c(j-1, j+1) } else { j })
        counts=rep(counts, times=lengths(beam))
        ## print(beam)
        ## print(counts)
        beam  %<>%  unlist()
        icounts=numeric(ncol(test))
        for(ibeam in unique(beam)) 
            icounts[ibeam]=sum(counts[which(beam == ibeam)])
        counts=icounts[ icounts > 0 ]
        beam=unique(beam)
        ## print(beam)
        ## print(counts)
        cat("\n")
    }
    sum(counts)
}

g(test)
g(inp)
