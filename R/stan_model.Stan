// states
// 1: healthy
// 2: contagious infection
// 3: environmental infection
// 4: contagious and environmental infections
data{

 int<lower = 1> K;      // number of states
 int<lower = 1> Nobs;   // number of observations
 int<lower = 1> Nani;   // number of animals
 int<lower = 0, upper = 1> y; // test results

}
parameters{

 array[K] real<lower = 0, upper = 1> test_char; // test characteristics.
                                                // Probabilities of positive test result conditionnally on latent state
 simplex[K] pi1;    // initial state probabilities
 simplex[k] B[K];   // transition matrix
                    // B[i, j] = p(z_t = j | z{t-1} = i)

}
transformed parameters{
  // definition of logalpha
  matrix[Nobs, K] logalpha;

  {

  // accumulator
  array[Nobs, K] real accumulator;

  // loop over all animals
  // for(a in 1:Nani){ // initially considering a single animal to make things simpler

  real accumulator[K];
  // first test in sequence
  for(k1 in 1:K){

  logalpha[1, k1] = log(pi1[k1]) + bernoulli_lpmf(y[1] | test_char[k1]);

  }


  for(t in 2:T){         // test at times 2 to T
    for(j in 1:K){     // current state
      for(i in 1:K){   // state at t-1

        accumulator[i] = logalpha[t-1, i] + log(B[i, j]) + bernoulli_lpmf(y[t] | test_char[j]);

      }

      logalpha[t, j] = log_sum_exp(accumulator);

    }

  } // end of loop for time

  // } // end of loop for animals

}
model{

// prior distributions for test characteristics
// vector of the probability of getting a positive test result
// conditionnaly on the latent state
// the first element of the vector is 1 - specificity
// the other ones are the sensitivities of the different tests
test_char[1] ~ beta(Sp_a, Sp_b);
test_char[2] ~ beta(Se1_a, Se1_b);
test_char[3] ~ beta(Se2_a, Se2_b);
test_char[4] ~ beta(Se3_a, Se3_b);

  target += log_sum_exp(logalpha[T]);

}
