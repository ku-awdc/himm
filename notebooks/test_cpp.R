library("tidyverse")
library("runjags")

Nani <- 100
Ntime <- 5

p1 <- 0.2
beta <- 0.05
gamma <- 0.08

se <- 0.9
sp <- 0.99

states <- matrix(nrow=Nani, ncol=Ntime)
states[,1] <- rbinom(Nani, 1, p1)
for(t in 2:Ntime){
  states[,t] <- rbinom(Nani, 1, (1-gamma)*states[,t-1] + beta*(1-states[,t-1]))
}

Obs <- states
Obs[] <- rbinom(Nani*Ntime, 1, states*se + (1-states)*(1-sp))

h1 <- himm:::Himm_Nx5$new(Nani,Ntime)
h1$pointer_index
h1$calculate()
h1$log_density

h1$addData(Obs)
h1$calculate()
h1$log_density

h1$zs
h1$test(h1$obsprev(1))
sapply(0:((2^Ntime)-1), h1$calculate_zi) |> sum()
himm:::active_index()
himm:::show_pointer(1)

h1$test(h1$obsprev(1))
h1$getZis() |> sum()

Obs[1,]
sum(h1$getObsProbs()[,1] * h1$getZis())
sum(h1$getObsProbs()[,2] * h1$getZis())

prvs <- seq(0,1,length=101)
pbs <- sapply(prvs, function(x){
  h1$test(x)
  apply(h1$getObsProbs() * h1$getZis(), 2, sum) |> prod()
})
plot(prvs, pbs, type="l")
prvs[which.max(pbs)]
h1$obsprev(1)

plot(seq(0,1,length=101), exp(sapply(seq(0,1,length=101), h1$test)), type="l")


m <- "
model{

  Index ~ dhimm(p1, bconst, bfreq, g, se, sp)
  p1 ~ dbeta(1,1)
  bconst ~ dbeta(1,1)
  bfreq ~ dbeta(1,1)
  g ~ dbeta(1,1)
  se ~ dbeta(10,1)
  sp ~ dbeta(20,1)

  #data# Index
  #monitor# p1, bconst, bfreq, g, se, sp, Index
}
"

Index <- 1
library(rjags)
library(runjags)
res <- run.jags(m)
res
