library(magrittr)
library(data.table)

reader=function(f) {
    test=fread(f, header=FALSE)
    setnames(test, c("x","y","z"))
    d=dist(as.matrix(test))  %>% as.matrix()
    diag(d)=Inf
    d[upper.tri(d)]=Inf
    d
}

inp=reader("08.inp")
test=reader("08.test")


f=function(d, nmax=10, nprod=3) {
    connections=matrix(0, nrow=nrow(d), ncol=nrow(d))
    for(i in 1:nmax) {
        w=which(d==min(d), arr.ind=TRUE)
        connections[w]=1
        d[w]=Inf
    }
    connections=connections + t(connections)
    d2=as.dist(1-connections)
    hc=hclust(d2,method="single")
    circuits=cutree(hc, h=.5)  %>% table()  %>% sort(., decreasing=TRUE)
    prod(circuits[1:nprod])
}

f(test)
f(inp,nmax=1000)

## part 2

## need a quicker test for whether a matrix is completely connected
library(igraph)
connected=function(connections) {
    g=graph_from_adjacency_matrix(connections)
    is_connected(g)
}

reader=function(f) {
    test=fread(f, header=FALSE)
    setnames(test, c("x","y","z"))
    test
}

getd=function(test) {
    d=dist(as.matrix(test))  %>% as.matrix()
    diag(d)=Inf
    d[upper.tri(d)]=Inf
    d
}

f=function(test, nmin=10) {
    d=getd(test)
    connections=matrix(0, nrow=nrow(d), ncol=nrow(d))
    i=1
    while(i <= nmin || !connected(connections)) {
        w=which(d==min(d), arr.ind=TRUE)
        connections[w]=1
        d[w]=Inf
        i=i+1
    }
    rbind(test[w[,1], ], test[w[,2], ])
}

inp=reader("08.inp")
test=reader("08.test")

f(test)[,1]  %>% prod()
f(inp,nmin=1000)[,1]  %>% prod()
