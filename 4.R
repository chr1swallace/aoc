library(magrittr)
test=strsplit("..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.", "\\n")[[1]]
inp=scan("4.inp", what=character())

f=function(test) {
mat=gsub("@", "1", gsub("\\.", "0", test))  %>%
    strsplit(., "")  %>%
    lapply(., as.integer)  %>%
    do.call(rbind, .)
## pad
mat=rbind(0, mat, 0)
mat=cbind(0, mat, 0)

## indices of 1s
w=which(mat == 1, arr.ind=TRUE)
## count surrounding 1s
offset=list(c(-1,0), c(-1,-1), c(0,-1), c(1,-1),
            c(1,0), c(1,1), c(0,1), c(-1,1))
counts=lapply(offset, function(o) {
    mat[cbind(w[,1] + o[1], w[,2] + o[2])]==1
})
total=Reduce(`+`, counts)
sum(total < 4) # 13
}

f(test)
f(inp)

## part 2

f=function(mat) {
## indices of 1s
w=which(mat == 1, arr.ind=TRUE)
## count surrounding 1s
offset=list(c(-1,0), c(-1,-1), c(0,-1), c(1,-1),
            c(1,0), c(1,1), c(0,1), c(-1,1))
counts=lapply(offset, function(o) {
    mat[cbind(w[,1] + o[1], w[,2] + o[2])]==1
})
total=Reduce(`+`, counts)
remove=which(total < 4)
mat[cbind(w[remove,1], w[remove,2])]=0
mat
}

g=function(test) {
   mat=gsub("@", "1", gsub("\\.", "0", test))  %>%
    strsplit(., "")  %>%
    lapply(., as.integer)  %>%
       do.call(rbind, .)
   initones=sum(mat)
   ## pad
   mat=rbind(0, mat, 0)
   mat=cbind(0, mat, 0)
   ones=initones
   oldones=Inf
   while(ones<oldones && ones > 0) {
       mat=f(mat)
       ## print(mat)
       oldones=ones
       ones=sum(mat)
   }
   endones=sum(mat)
   initones - endones
}

g(test)
g(inp) 
