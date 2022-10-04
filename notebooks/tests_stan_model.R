library(tidyverse)
library(cmdstanr)


# toy dataset
scc <- tibble(
  cow_id = rep(1:10, each = 10),
  time = rep(1:10, 10),
  test_res = rbinom(n = 100, size = 1, p = .5)
 )

# indices for the cow level dataset
by_cow <- scc %>%
           mutate(id = 1:nrow(scc)) %>%
           group_by(cow_id) %>%
           summarise(
             row_first = min(id),
             row_last = max(id)
           )

stan_data <- list(
  K = 4,
  Nobs = nrow(scc),
  Nani = length(unique(scc$cow_id)),
  y = scc$test_res,
  row_first = by_cow$row_first,
  row_sec = by_cow$row_first + 1,
  row_last = by_cow$row_last,
  Sp_a = 10,
  Sp_b = 1,
  Se1_a = 10,
  Se1_b = 1,
  Se2_a = 1,
  Se2_b = 10,
  Se3_a = 1,
  Se3_b = 20
 )


stan_model_txt <-
"data{

  int<lower = 1> K;      // number of states
  int<lower = 1> Nobs;   // number of observations
  int<lower = 1> Nani;   // number of animals
  array[Nobs] int<lower = 0, upper = 1> y; // test results
  array[Nani] int<lower = 0, upper = Nobs> row_first; // id of first row for each cow
  array[Nani] int<lower = 0, upper = Nobs> row_sec;   // id of second row for each cow
  array[Nani] int<lower = 0, upper = Nobs> row_last;  // id of last row for each cow

    // test characteristics
  real<lower = 1> Sp_a;
  real<lower = 1> Sp_b;
  real<lower = 1> Se1_a;
  real<lower = 1> Se1_b;
  real<lower = 1> Se2_a;
  real<lower = 1> Se2_b;
  real<lower = 1> Se3_a;
  real<lower = 1> Se3_b;

}
parameters{

  array[K] real<lower = 0, upper = 1> test_char; // test characteristics.
  // Probabilities of positive test result conditionnally on latent state
  simplex[K] pi1;    // initial state probabilities
  array[K] simplex[K] B;   // transition matrix
  // B[i, j] = p(z_t = j | z{t-1} = i)


}
transformed parameters{
  // definition of logalpha
  matrix[Nobs, K] logalpha;

  {

   // loop over all animals
    for(a in 1:Nani){

      array[K] real accumulator;

      // first test in sequence
      for(k1 in 1:K){

        logalpha[row_first[a], k1] = log(pi1[k1]) + bernoulli_lpmf(y[row_first[a]] | test_char[k1]);

      }


      for(t in row_sec[a]:row_last[a]){   // test at times 2 to T
        for(j in 1:K){     // current state
          for(i in 1:K){   // state at t-1

            accumulator[i] = logalpha[t-1, i] + log(B[i, j]) + bernoulli_lpmf(y[t] | test_char[j]);

          }

          logalpha[t, j] = log_sum_exp(accumulator);

        }

       } // end of loop for time

      } // end of loop for animals

  }

  }
model{

    // prior distributions for test characteristics
    // vector of the probability of getting a positive test result
    // conditionnaly on the latent state
    // the first element of the vector is 1 - specificity
    // the other ones are the sensitivities of the different tests
    test_char[1] ~ beta(Sp_b, Sp_a);
    test_char[2] ~ beta(Se1_a, Se1_b);
    test_char[3] ~ beta(Se2_a, Se2_b);
    test_char[4] ~ beta(Se3_a, Se3_b);

   // loop over all animals
   for(aa in 1:Nani){

    target += log_sum_exp(logalpha[row_last[aa]]);

   }

  }"

stan_file <- write_stan_file(stan_model_txt)

stan_model <- cmdstan_model(stan_file)

stan_fit <- stan_model$sample(
  data = stan_data,
  chains = 3
   )

