#' Simulate a sequence of coin tosses
#'
#' @param n_sims The number of simulations
#' @param n  The length of the coin toss sequence
#' @param prob the probability of a head (1)
#'
#' @return An list of length \code{n_sims}, where each
#' element is a vector of length \code{n} containing a 
#' coin toss sequence: 0 (tails), 1 (heads).
#' @export
#'
#' @examples
#' sim_coin_tosses(n_sims = 2, n = 4, prob = 1)
sim_coin_tosses <- function(n_sims, n, prob){
  rerun(n_sims, 
    rbinom(n, size = 1, prob = prob))
}

max_run_length <- function(x){
  # Finds length of longest run in x
  max(rle(x)$lengths)
}

sim_run_lengths <- function(n_sims, n, prob = 0.5){
  # Returns length of longest run in n_sims sequences
  # of coin toss sequences of  n flips, where each flip
  # has prob p of being heads.
  sim_coin_tosses(n_sims, n, prob) %>% 
    map_dbl(~ max_run_length(.))
}