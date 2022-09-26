## Parameters:

Nani <- 100
Ntime <- 10

p1 <- 0.2
beta <- 0.05
gamma <- 0.05

se <- 0.9
sp <- 0.99

states <- matrix(nrow=Nani, ncol=Ntime)
states[,1] <- rbinom(Nani, 1, p1)
for(t in 2:Ntime){
  states[,t] <- rbinom(Nani, 1, (1-gamma)*states[,t-1] + beta*(1-states[,t-1]))
}

Obs <- states
Obs[] <- rbinom(Nani*Ntime, 1, states*se + (1-states)*(1-sp))


mod <- "

model{

  # DataIndex ~ dhimm(p1, b1, b2, b3, gamma, se, sp)

  # Process layer:
  for(a in 1:Nani){
    state[a,1] ~ dbern(p1)
    for(t in 2:Ntime){
      state[a,t] ~ dbern((1-gamma)*state[a,t-1] + beta*(1-state[a,t-1]))
    }
  }
  for(t in 1:Ntime){
    prev[t] <- mean(state[1:Nani,t])
  }

  # Obs layer:
  for(a in 1:Nani){
    for(t in 1:Ntime){
      Obs[a,t] ~ dbern(se*state[a,t] + (1-sp)*(1-state[a,t]))
    }
  }

  # priors:
  p1 ~ dbeta(1,1)
  beta ~ dbeta(1,1)
  gamma ~ dbeta(1,1)

  se <- 0.9
  sp <- 0.99

  #data# Nani, Ntime, Obs
  #monitor# beta, gamma, p1, prev
}
"

library(runjags)

res <- run.jags(mod)
res



## TOOD:  3-state model
