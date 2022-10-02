library("tidyverse")
library("runjags")


## 2-state model
# Parameters:

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

res <- run.jags(mod)
res



## Multiple state model
# 1: uninfected
# 2: contagious
# 3: environmental
# 4: both
# We actually code this as 2 independent 2-state models

Nani <- 100
Ntime <- 10
Nmat <- 2

p1 <- c(0.1, 0.05)
beta_fix <- c(0, 0.05)
beta_freq <- c(0.1, 0)
gamma <- c(0.1, 0.95)

# Se and Sp are actually probabilities for SCC>200 conditional
# on non-infected, contagious, environmental, both status:
obsprob <- c(0.1, 0.9, 0.9, 0.9)

states <- array(NA_integer_, dim=c(Nani,Ntime,2L), dimnames=list(paste0("Animal",1:Nani), paste0("Time",1:Ntime), c("Contagious","Environmental")))

for(i in 1:2) states[,1,i] <- rbinom(Nani, 1, p1[i])

for(t in 2:Ntime){
  for(i in 1:2){
    # Combine fixed and density-dependent transmission:
    probinf <- 1 - ((1-beta_freq[i])^(sum(states[,t-1,i])/Nani) * (1-beta_fix[i]))
    states[,t,i] <- rbinom(Nani, 1, (1-gamma[i])*states[,t-1,i] + probinf*(1-states[,t-1,i]))
  }
}

obscat <- apply(states,c(1,2), function(x) case_when(x["Environmental"]==1 & x["Contagious"]==1 ~ 4, x["Environmental"]==1 ~ 3, x["Contagious"]==1 ~ 2, TRUE ~ 1))
Obs <- obscat
Obs[] <- rbinom(Nani*Ntime, 1, obsprob[obscat])


## Observation probabilities:

# D = 1:
prob <- array(dim=c(2))
prob[1] <- 0.1  # 1-sp
prob[2] <- 0.8  # se
prob

# D = 2:
prob <- array(dim=c(2,2))
prob[1,1] <- 0.05  # probability of positive test for -/- animal
prob[2,1] <- 0.8   # probability of positive test for +/- animal
prob[1,2] <- 0.7  # probability of positive test for -/+ animal
prob[2,2] <- 0.9   # probability of positive test for +/+ animal
prob

# D = 3:
prob <- array(dim=c(2,2,2))
# etc


# For mastitis:
Obsprob <- array(dim=c(2,2))
Obsprob[1,1] <- 0.1  # probability of positive test for -/- animal
Obsprob[2,1] <- 0.9   # probability of positive test for +/- animal
Obsprob[1,2] <- 0.9  # probability of positive test for -/+ animal
Obsprob[2,2] <- 0.9   # probability of positive test for +/+ animal
Obsprob


mod <- "
model{

  # DataIndex ~ dhimm(p1, b1, b2, b3, gamma, se, sp)

  # Process layer:
  for(t in 1:Ntime){
      infn[t] <- sum(state[1:Nani,t,1])
      infprob[t] <- 1 - (1 - beta[1])^(infn[t]/Nani)
  }
  for(a in 1:Nani){
    # Contagious mastitis:
    state[a,1,1] ~ dbern(p1[1])
    for(t in 2:Ntime){
      # TODO: ifelse for this infprob
      state[a,t,1] ~ dbern((1-gamma[1])*state[a,t-1,1] + infprob[t-1]*(1-state[a,t-1,1]))
    }

    # Environmental mastitis:
    state[a,1,2] ~ dbern(p1[2])
    for(t in 2:Ntime){
      state[a,t,2] ~ dbern((1-gamma[2])*state[a,t-1,2] + beta[2]*(1-state[a,t-1,2]))
    }
  }

  # Obs layer:
  for(a in 1:Nani){
    for(t in 1:Ntime){
      Obs[a,t] ~ dbern(Obsprob[state[a,t,1]+1, state[a,t,2]+1])
    }
  }

  # priors:
  for(i in 1:2){
    p1[i] ~ dbeta(1,1)
    beta[i] ~ dbeta(1,1)
  }

  # Model obviously works better with fixed gammas but this seems to work OK:
  gamma[1] ~ dbeta(2,5)
#  gamma[1] <- 0.1
  gamma[2] ~ dbeta(10,2)
#  gamma[2] <- 0.95

  # NB: Obsprob could be parameters to estimate rather than data
  Obsprob[1,1] <- 1-sp
  Obsprob[1,2] <- se
  Obsprob[2,1] <- se
  Obsprob[2,2] <- se
  sp ~ dbeta(5,1)
  se ~ dbeta(5,1)

  #data# Nani, Ntime, Obs
  #monitor# beta, gamma, p1, se, sp
}
"


res <- run.jags(mod)
res

