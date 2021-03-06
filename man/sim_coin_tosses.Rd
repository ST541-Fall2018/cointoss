% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulation.R
\name{sim_coin_tosses}
\alias{sim_coin_tosses}
\title{Simulate a sequence of coin tosses}
\usage{
sim_coin_tosses(n_sims, n, prob)
}
\arguments{
\item{n_sims}{The number of simulations}

\item{n}{The length of the coin toss sequence}

\item{prob}{the probability of a head (1)}
}
\value{
An list of length \code{n_sims}, where each
element is a vector of length \code{n} containing a 
coin toss sequence: 0 (tails), 1 (heads).
}
\description{
Simulate a sequence of coin tosses
}
\examples{
sim_coin_tosses(n_sims = 2, n = 4, prob = 1)
}
