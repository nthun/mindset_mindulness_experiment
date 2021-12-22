# Generate zero inflated, truncated negative binomial distribution
source("R/rtnegbin.R")
rtzitnb <- function(n, size, mu, max_obs, zprop){
  
  zero_one <- rbinom(n, 1, 1-zprop)
  ifelse(zero_one == 0, 
         0, 
         rtnegbin(n = sum(zero_one == 1), 
                  size = size, 
                  mu = mu, 
                  max_obs = max_obs-1) + 1)
  
}


# Test
# rtzitnb(10000, .5, 4, 17, .34) %>% qplot()
