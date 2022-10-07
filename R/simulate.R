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
