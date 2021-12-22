# A zero inflated tidyer.

library(tidyverse)
tidy.zinf <- function(x, exponentiate = FALSE, conf.int = NULL, conf.level = .95){
  
  sum_model <- summary(x)$coefficients 

  out <-
  map(sum_model, 
      ~as.data.frame(.x) %>% 
        rownames_to_column("term") %>% 
        rename(
               estimate = Estimate,
               std.error = `Std. Error`,
               statistic = `z value`,
               p.value = `Pr(>|z|)`)) %>% 
    bind_rows(.id = "model")

  if (conf.int == TRUE) {
    cf <-
      confint(x, level = conf.level) %>% 
      as.data.frame() %>% 
      rownames_to_column("model_term") %>% 
      separate(model_term, 
               into = c("model", "term"), 
               sep = "_", 
               extra = "merge") %>% 
      rename(conf.low = 3, conf.high = 4)
    
    out <- left_join(out, cf, by = c("model", "term"))
   }
    
  if (exponentiate == TRUE) {
    out <- 
      mutate(out, across(any_of(c("estimate", "conf.low", "conf.high")), exp))
  }

  as_tibble(out)
  
}

