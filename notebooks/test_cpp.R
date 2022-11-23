library("tidyverse")
library("runjags")
library("himm")

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


h2 <- himm:::SimpleForward$new(Nani,Ntime)
h2$pointer_index
h2$calculate()
h2$log_density

h2$addData(Obs)
h2$calculate()
h2$log_density

h1$test(0.1)
h2$test(0.1)

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
  bfreq <- 0
  g ~ dbeta(1,1)
  se <- 0.9 #~ dbeta(10,1)
  sp <- 0.99 #~ dbeta(20,1)

  #data# Index
  #monitor# p1, bconst, bfreq, g, se, sp, Index
}
"

himm:::active_index()

Index <- 2
library(rjags)
library(runjags)
res <- run.jags(m)
res
