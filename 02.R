library(magrittr)
inp=scan("2.inp", what=character())  %>%
    strsplit(., ",")
inp=inp[[1]]

## largest number
a=as.numeric(sub("-.*", "", inp))
b=as.numeric(sub(".*-", "", inp))
max(b)
nchar(max(b))

## part 1
f=function(i) {
    a=as.numeric(sub("-.*", "", i))
    b=as.numeric(sub(".*-", "", i))
    s=a:b
    invalid=rep(FALSE, length(s))
    for(j in 1:5) {
        d=10^j + 1
        invalid=invalid | (s %% d == 0 & nchar(s) == j*2)
    }
    s[invalid]
}

f("11-22")
f("95-115")
f("998-1012")
f("1188511880-1188511890")
f("222220-222224")
f("1698522-1698528")
f("446443-446449") # has one invalid ID, 446446.
f("38593856-38593862")

invalid=lapply(inp, f)
unlist(invalid)  %>% sum()

## part 2
g=function(i) {
    a=as.numeric(sub("-.*", "", i))
    b=as.numeric(sub(".*-", "", i))
    s=a:b
    invalid=rep(FALSE, length(s))
    ## two repeats
    for(j in 1:5) {
        d=10^j + 1
        invalid=invalid | (s %% d == 0 & nchar(s) == j*2)
    }
    ## three repeats
    for(j in 1:3) {
        d=10^(2*j) + 10^j + 1
        invalid=invalid | (s %% d == 0 & nchar(s) == 3*j)
    }
    ## four repeats
    for(j in 1:2) {
        d=10^(3*j) + 10^(2*j) + 10^j + 1
        invalid=invalid | (s %% d == 0 & nchar(s) == 4*j)
    }
    ## five repeats
    for(j in 1:2) {
        d=10^(4*j) + 10^(3*j) + 10^(2*j) + 10^j + 1
        invalid=invalid | (s %% d == 0 & nchar(s) == 5*j)
    }
    ## six-ten repeats
    for(j in 6:10) {
        d=rep(1,j)  %>% paste0(., collapse="")  %>% as.numeric()
        invalid=invalid | (s %% d == 0 & nchar(s) == j)
    }
    s[invalid]
}

g("11-22")
g("95-115")
g("998-1012")
g("1188511880-1188511890")
g("824824821-824824827") # now has one invalid ID, 824824824.
g("2121212118-2121212124") # 2121212121

invalid=lapply(inp, g)
unlist(invalid)  %>% sum()
