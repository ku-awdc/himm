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
"functions{

 vector normalise(vector x){

  return x / sum(x);

 }

}
data{

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
  simplex[K] pi1;          // initial state probabilities
  array[K] simplex[K] B;   // transition matrix
                           // B[i, j] = p(z_t = j | z{t-1} = i)


}
transformed parameters{
  // definition of alpha and logalpha for the forward algorithm
  array[Nobs] vector[K] logalpha;
  array[Nobs] vector[K] alpha;
  // definition of beta and logbeta for the backward algorithm
  array[Nobs] vector[K] logbeta;
  array[Nobs] vector[K] beta;
  // definition of gamma and loggamma for the forward-backaward algorithm
  array[Nobs] vector[K] loggamma;
  array[Nobs] vector[K] gamma;

   // loop over all animals
    for(a in 1:Nani){

   { // forward algorithm

      array[K] real accumulator;

      // first test in sequence
      for(k1 in 1:K)
        logalpha[row_first[a], k1] = log(pi1[k1]) + bernoulli_lpmf(y[row_first[a]] | test_char[k1]);

      // test at times 2 to T
      for(t in row_sec[a]:row_last[a]){
       for(j in 1:K){     // current state
         for(i in 1:K){   // state at t-1

            accumulator[i] = logalpha[t-1, i] + log(B[i, j]) + bernoulli_lpmf(y[t] | test_char[j]);

          } // i

          logalpha[t, j] = log_sum_exp(accumulator);

        } // j

       } // end of loop for time

      } // end of loop for the forward algorithm


   { // backward algorithm

      array[K] real accumulator;

     // last test in sequence
      for(k1 in 1:K)
        logbeta[row_last[a], k1] = 1;

     // backward from the last time point in sequence
     for(tforward in 0:(row_last[a] - 2)){

      int t;
      t = row_last[a] - tforward;

      for(j in 1:K){     // previous state, from j to i
        for(i in 1:K){   // current state

         accumulator[i] = logbeta[t, i] + log(B[j, i]) + bernoulli_lpmf(y[t] | test_char[i]);

          } // i

          logbeta[t - 1, j] = log_sum_exp(accumulator);

        } // j

      } // tforward


   } // backward algorithm


      } // end of loop for animals

   // forward-backward
   for(t in 1:Nobs){

    alpha[t] = softmax(logalpha[t]);
    beta[t] = softmax(logbeta[t]);
    loggamma[t] = alpha[t] .* beta[t];
    gamma[t] = normalise(loggamma[t]);

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

rslts <- as_tibble(stan_fit$summary())
