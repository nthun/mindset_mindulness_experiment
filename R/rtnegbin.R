# Generate a truncated negative binomial distribution
rtnegbin <- function(n, size, mu, max_obs){
  pmin(
    stats::rnbinom(n = n, size = size, mu = mu), 
    max_obs)
}
