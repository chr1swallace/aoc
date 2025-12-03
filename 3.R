test=strsplit("987654321111111
811111111111119
234234234234278
818181911112111","\n")[[1]]
inp=scan("3.inp", what=character())

## part 1
digits=strsplit(test,"")
i=digits[[2]]
f=function(i) {
    ## position of largest digit not in last place
    p1=head(i, -1) %>% which.max() 
    ## position of largest digit after that
    p2=tail(i, -p1) %>% which.max() + p1
    paste0(i[c(p1, p2)], collapse="")  %>% as.numeric()
}
joltage=sapply(digits, f)

digits=strsplit(inp,"")
joltage=sapply(digits, f)
sum(joltage)

## part 2
digits=strsplit(test,"")
i=digits[[3]]
g=function(i) {
    l=length(i)
    st=0 # must pick a digit beyond this one
    pos=numeric(12)
    for(j in 12:1) {
        ## position of largest digit after st, that is not in last j places
        pnext=which.max(i[(st+1):(l-j+1)]) + st
        st=pos[13-j]=pnext
    }
    paste(i[pos], collapse="")  %>% as.numeric()
}
sapply(digits, g)

digits=strsplit(inp,"")
joltage=sapply(digits, g)
options(digits=22)
sum(joltage)

