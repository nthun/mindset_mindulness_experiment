# Generate a dataset of integer values with specified distribution paramaters
# OUTPUT: A tibble in long format
generate_dataset(n = 100, 
                 groups = c("a", "b", "c"),
                 means = c(3, 3.08, 3.12), sds = c(1, 1, 1))

if (!require(purrr)) install.packages("purrr")
if (!require(dplyr)) install.packages("dplyr")
source(here::here("R/draw_values.R"))

generate_dataset <- function(group_size, 
                             means, 
                             sds, 
                             floor = 1, 
                             ceiling = 5,
                             groups){

  # Check parameters
  stopifnot(any(!is.na(group_size), !is.na(means), !is.na(sds), !is.na(groups)))
  stopifnot(length(groups) == length(means), length(means) == length(sds))
  stopifnot(is.numeric(group_size), is.numeric(means), is.numeric(sds))
  
  # Generate dataset
  purrr::map(seq_along(groups),
             ~dplyr::tibble(group = groups[.x],
                            value = draw_values(group_size, means[.x], sds[.x]))) %>% 
    dplyr::bind_rows()

}

# Test if the function generates the dataset with similar means
# 

# means = c(3, 2.1, 4.2)
# sds = c(1, .5, 1)
# 
# generate_dataset(group_size = 1000, 
#                  means = means, 
#                  sds = sds, 
#                  groups = c("Control", "Treatment 1", "Treatment 2")) %>% 
#   group_nest(group) %>% 
#   mutate(pop_mean = means,
#          pop_sd = sds,
#          samp_mean = map_dbl(data, ~mean(.x$value)),
#          samp_sd = map_dbl(data, ~sd(.x$value)),
#          ttest_p = map2_dbl(data, pop_mean, ~t.test(x = .x$value, mu = .y)$p.value))
  
