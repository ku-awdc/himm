#' HMM data in one of two formats:  (1) simple matrix, (2) data frame
#' @name simulate_hmm
#' @aliases simulate_basic
#'
#' @param N_animals
#' @param N_time
#' @param p1
#' @param beta_const
#' @param beta_freq
#' @param gamma
#' @param se
#' @param sp
#'
#' @examples
#' Obs <- simulate_basic()


#' @rdname simulate_hmm
#' @export
simulate_basic <- function(N_animals = 100L, N_time = 10L, p1 = 0.1, beta_const = 0.05, beta_freq = 0.02, gamma = 0.08, sensitivity = 0.8, specificity = 0.99){

  states <- matrix(nrow=N_animals, ncol=N_time)
  states[,1] <- rbinom(N_animals, 1, p1)

  for(t in 2:N_time){
    beta <- 1 - ((1 - beta_freq * sum(states[,t-1])/N_animals) * (1 - beta_const))
    stopifnot(beta >= 0.0, beta <= 1.0)
    states[,t] <- rbinom(N_animals, 1, (1-gamma)*states[,t-1] + beta*(1-states[,t-1]))
  }

  Obs <- states
  Obs[] <- rbinom(N_animals*N_time, 1, states*sensitivity + (1-states)*(1-specificity))

  return(Obs)
}


#' @rdname simulate_hmm
#' @export
simulate_hmm <- function(N_animals = 100L, N_time = 10L, N_diseases = 2L, p1 = 0.1, beta_const = 0.05, beta_freq = 0.02, gamma = 0.08, sensitivity = 0.8, specificity = 0.99){

  stopifnot(length(N_animals)==1L, length(N_time)==1L, length(N_diseases)==1L)

  if(length(p1)==1L) p1 <- rep(p1, N_diseases)
  if(length(beta_const)==1L) beta_const <- rep(beta_const, N_diseases)
  if(length(beta_freq)==1L) beta_freq <- rep(beta_freq, N_diseases)
  if(length(gamma)==1L) gamma <- rep(gamma, N_diseases)
  if(length(sensitivity)==1L) sensitivity <- rep(sensitivity, N_diseases)
  if(length(specificity)==1L) specificity <- rep(specificity, N_diseases)

  stopifnot(length(p1)==N_diseases)
  stopifnot(length(beta_const)==N_diseases)
  stopifnot(length(beta_freq)==N_diseases)
  stopifnot(length(gamma)==N_diseases)
  stopifnot(length(sensitivity)==N_diseases)
  stopifnot(length(specificity)==N_diseases)

  states <- array(dim=c(N_diseases, N_animals, N_time))
  states[,,1] <- rbinom(N_diseases*N_animals, 1L, p1)

  for(t in 2L:N_time){
    beta <- 1.0 - ((1.0 - beta_freq * apply(states[,,t-1L],1L,sum)/N_animals) * (1.0 - beta_const))
    stopifnot(length(beta)==N_diseases, beta >= 0.0, beta <= 1.0)
    states[,,t] <- rbinom(N_diseases*N_animals, 1L, (1.0-gamma)*states[,,t-1L] + beta*(1L-states[,,t-1L]))
  }

  Obs <- states
  Obs[] <- rbinom(N_diseases*N_animals*N_time, 1L, states*sensitivity + (1L-states)*(1.0-specificity))
  Obs <- apply(Obs,c(2,3),function(x) as.integer(any(x)))

  return(Obs)
}
