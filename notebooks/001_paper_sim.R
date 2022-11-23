library('himm')

# FIX: set seed in the jags code

simest <- function(i){

  cat("Running iteration ", i, "...\n")

  set.seed(i)

  # from simulate_basic help
  data <- simulate_basic(
    N_animals = 100L,
    N_time = 10L,
    p1 = 0.1,
    beta_const = 0.05,
    beta_freq = 0.0,
    gamma = 0.08,
    sensitivity = 0.8,
    specificity = 0.99
  )

  Nani <- nrow(data)
  Obs <- data

  mod <- "
model{

  for(a in 1:Nani){
    # Starting negative:
    logalpha[a,1,1] <- log(1 - p1) + log(Obs[a,1]*(1-sp) + (1-Obs[a,1])*sp)
    # Starting positive:
    logalpha[a,1,2] <- log(p1) + log(Obs[a,1]*se + (1-Obs[a,1])*(1-se))

    # Define arbitrarily:
    accumulator[a,1,1] <- 0
    accumulator[a,1,2] <- 0

    for(t in 2:Ntime){

      # Status negative to negative:
      accumulator_n[a,t,1] <- logalpha[a,t-1,1] + log(1 - beta) + log(Obs[a,t]*(1-sp) + (1-Obs[a,t])*sp)
      # Status positive to negative:
      accumulator_n[a,t,2] <- logalpha[a,t-1,2] + log(gamma) + log(Obs[a,t]*(1-sp) + (1-Obs[a,t])*sp)
      # Sum probabilities:
      logalpha[a,t,1] <- log(exp(accumulator_n[a,t,1]) + exp(accumulator_n[a,t,2]))

      # Status negative to positive:
      accumulator_p[a,t,1] <- logalpha[a,t-1,1] + log(beta) + log(Obs[a,t]*se + (1-Obs[a,t])*(1-se))
      # Status positive to positive:
      accumulator_p[a,t,2] <- logalpha[a,t-1,2] + log(1-gamma) + log(Obs[a,t]*se + (1-Obs[a,t])*(1-se))
      # Sum probabilities:
      logalpha[a,t,2] <- log(exp(accumulator_p[a,t,1]) + exp(accumulator_p[a,t,2]))

    }

    final_logalpha[a] <- -log(exp(logalpha[a,Ntime,1]) + exp(logalpha[a,Ntime,2]))
    Zeros[a] ~ dpois(final_logalpha[a])
  }

  # priors:
  p1 ~ dbeta(1,1)
  beta ~ dbeta(1,1)
  gamma ~ dbeta(1,1)

  se <- 0.9
  sp <- 0.99

  #data# Nani, Ntime, Obs, Zeros
  #monitor# beta, gamma, p1

}
"

  Zeros <- rep(0, Nani)
  res_forward <- run.jags(mod)
  res_forward



  # indicator model

  mod_liv <- "

model{

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
  #monitor# beta, gamma, p1
}
"
  res_liv <- run.jags(mod_liv)

  tidyfy <- function(results, model){
    summary(results) |>
      as.data.frame() |>
      rownames_to_column("Parameter") |>
      mutate(Model = model) |>
      mutate(Time = as.numeric(results$timetaken, units="secs")) |>
      select(Model, Parameter, Median, Lower95, Upper95, SSeff, Time) |>
      mutate(SSperSec = SSeff / Time)
  }


  bind_rows(
    tidyfy(res_liv, "JAGS_LIV"),
    tidyfy(res_forward, "JAGS_Zeros")
  ) |>
    mutate(Dataset = i) |>
    arrange(Parameter)

}


1:50 |>
  as.list() |>
  lapply(simest) |>
  bind_rows() ->
all_results


