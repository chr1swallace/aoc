library(magrittr)
inp=scan("1.inp", what=character())
num=as.integer(sub("L","-", sub("R", "", inp)))
now=50

## part 1
steps=now + cumsum(num)
atzero=sum(steps %% 100 ==0)
atzero    

## part 2
plot(1:length(steps), steps)
max(num) # 999
min(num) # -998

## look carefully at test input
test=strsplit("L68
L30
R48
L5
R60
L55
L1
L99
R14
L82", "\\n")[[1]]
num=as.integer(sub("L","-", sub("R", "", test)))
steps=cumsum(c(50,num))

steps=cumsum(c(50,num))
zero=steps %% 100 == 0
## if steps == 0, make this have same sign as previous value
steps_forward=steps_backward=steps
steps_forward[ zero ]=0.99 * steps[zero] + 0.01 * shift(steps, 1)[ zero ]
steps_backward[ zero ]=0.99 * steps[zero] + 0.01 * shift(steps, -1)[ zero ]
d_f=(steps_forward %/% 100)  %>% diff()
d_b=(steps_backward %/% 100)  %>% diff()

res=data.frame(raw=raw_steps,
               steps=steps,
               steps100=steps %/% 100,
               d_f=c(NA, d_f),
               d_b=c(NA, d_b),
               iszero=as.numeric(zero),
               calc=c(NA,pmin(abs(d_f), abs(d_b))) + as.numeric(zero),
               goal=c(NA,1,0,1,0,1,1,0,1,0,1))
res

## run for real
num=as.integer(sub("L","-", sub("R", "", inp)))
steps=cumsum(c(50,num))
zero=steps %% 100 == 0
## if steps == 0, make this have same sign as previous value
steps_forward=steps_backward=steps
steps_forward[ zero ]=0.99 * steps[zero] + 0.01 * shift(steps, 1)[ zero ]
steps_backward[ zero ]=0.99 * steps[zero] + 0.01 * shift(steps, -1)[ zero ]
d_f=(steps_forward %/% 100)  %>% diff()
d_b=(steps_backward %/% 100)  %>% diff()
calc=c(NA,pmin(abs(d_f), abs(d_b))) + as.numeric(zero)
sum(calc[-1]) # -1 for the 50 starting place
