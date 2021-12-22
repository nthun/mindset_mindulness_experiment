# Power analysis for the experimental study
library(tidyverse)
library(furrr) # For parallel processing
library(broom) # For tidying stat output
library(directlabels) # For plotting direct labels
library(extraDistr) # For truncated poisson distribution

theme_set(theme_light()) # For clean plot theme
plan(multisession(workers = availableCores())) # Setting parallel processing

# Define parameters ----------------------------------------------------------------

samples = 10000 # Number of samples for each n
sample_increment = 20 # The increment of cell sizes for the simulation
max_n = 200 # The maximum cell size
lambda = 4 # The average number of extra tasks finished by the control group
max_lambda = 15 # Maximum number of extra tasks
difference = 1.7 # The difference of the treatment group from the control
alpha = .05 # Level of significance

# Simulate samples ------------------------------------------------------------------
# Generate a design matrix that defines the samples that will be simulated
power_matrix <-
  crossing(cell_size = seq.int(sample_increment, max_n, sample_increment),
           sample = 1:samples) 

# Calculate the p values for each sample, based on the population parameters
simulation_data <-
  power_matrix %>% 
  as.list() %>% 
  # Use parallel processing to generate datasets(samples) based on parameters
  future_pmap(.progress = TRUE, 
              ~crossing(group = fct_inorder(c("Fixed-Control", 
                                              "Fixed-Mindfulness",
                                              "Growth-Control",
                                              "Growth-Mindfulness")),
                        id = 1:..1) %>% 
                # Generate the number of extra tasks using truncated poisson distribution
                # as the maximum number of extra tasks is 15.
                mutate(extra_task = c(rtpois(n = ..1, lambda = lambda, b = max_lambda), 
                                      rtpois(n = ..1, lambda = lambda + difference/2, b = max_lambda), 
                                      rtpois(n = ..1, lambda = lambda + difference/2, b = max_lambda), 
                                      rtpois(n = ..1, lambda = lambda + difference, b = max_lambda))) %>% 
                separate(group, into = c("mindset", "intervention"))
  )


# Test the hypothesis on each dataset (use parallel processing)
simulation_results <-
  simulation_data %>% 
  future_map(.progress = TRUE, 
                # Use poisson regression to test hypothesis
                ~glm(extra_task ~ intervention * mindset, family = "poisson", data = .x) %>% 
               # Get Relative Risks and confidence intervals for the predictors
                broom::tidy(exponentiate = TRUE, conf.int = TRUE))

# Calculate relative risks
# rr_intervention <-
#   simulation_results %>% 
#   map_dfr(.progress = TRUE, 
#                  ~filter(.x, term == "interventionMindfulness") %>% 
#                   select(estimate, conf.low, conf.high)) %>% 
#   # Put the RR values into the design matrix
#   bind_cols(power_matrix, .)
# 
# rr_intervention %>% 
#   group_by(cell_size) %>% 
#   summarise(mean(estimate))
# 
# rr_mindset <-
#   simulation_results %>% 
#   map_dfr(.progress = TRUE, 
#           ~filter(.x, term == "mindsetGrowth") %>% 
#             select(estimate, conf.low, conf.high)) %>% 
#   # Put the RR values into the design matrix
#   bind_cols(power_matrix, .)
# 
# rr_mindset %>% 
#   group_by(cell_size) %>% 
#   summarise(mean(estimate))

# Calculate power
p_intervention <- 
  simulation_results %>% 
  future_map_dbl(.progress = TRUE, 
                 ~filter(.x, term == "interventionMindfulness") %>% 
                  # Calculate one-sided Wald statistic
                  mutate(larger = pnorm(statistic, lower.tail = FALSE)) %>% 
                  pull(larger)) %>% 
  # Put the p values into the design matrix
  bind_cols(power_matrix, p = .)

p_mindset <- 
  simulation_results %>% 
  future_map_dbl(.progress = TRUE, 
                 ~filter(.x, term == "mindsetGrowth") %>% 
                   # Calculate one-sided Wald statistic
                   mutate(larger = pnorm(statistic, lower.tail = FALSE)) %>% 
                   pull(larger)) %>% 
  # Put the p values into the design matrix
  bind_cols(power_matrix, p = .)

# Calculate the power, based on the samples -----------------------------------------
# Calculate the average p value and power for each cell size in cases where both or any group difference is significant

result_summary <-
  left_join(p_intervention, p_mindset, by = c("cell_size", "sample")) %>% 
  mutate(sig_int = p.x <= alpha,
         sig_mind = p.y <= alpha,
         sig_both = sig_int & sig_mind,
         sig_any = sig_int | sig_mind) %>% 
  group_by(cell_size) %>% 
  summarise(power_both = mean(sig_both),
            p_both = mean(p.x),
            power_any = mean(sig_any),
            p_any = mean(p.y))

# Interpolate power values to see exact cut off point -------------------------------
# Interpolate data, so we can tell exact N without calculating it to exact number
interpolated_power <-
  tibble(cell_size = 1:max_n) %>% 
  left_join(result_summary, by = "cell_size") %>% 
  # Use linear interpolation
  mutate(power_both = stats::approx( x = cell_size,
                                     y = power_both,
                                     xout = cell_size)$y,
         # p_both = stats::approx(x = cell_size,
         #                        y = p_both,
         #                        xout = cell_size)$y,
         power_any = stats::approx( x = cell_size,
                                    y = power_any,
                                    xout = cell_size)$y,
         # p_any = stats::approx(x = cell_size,
         #                       y = p_any,
         #                       xout = cell_size)$y
         ) %>% 
  drop_na()

# Calculate the exact number of participants needed in each cell for specific power
interpolated_power %>% 
  transmute(cell_size,
            `both = .80` = power_both > .80,
            `both = .90` = power_both > .90,
            `both = .95` = power_both > .95,
            `both = .99` = power_both > .99,
            `any = .80` = power_any > .80,
            `any = .90` = power_any > .90,
            `any = .95` = power_any > .95
            ) %>% 
  gather(power, value, -cell_size, convert = TRUE) %>% 
  filter(value) %>% 
  group_by(power) %>% 
  summarise(required_cell_size = first(cell_size))
  

# Visualize the results -------------------------------------------------------------
# Simulated data
result_summary %>% 
  gather(parameter, value, power_both:power_any) %>% 
  separate(parameter, c("parameter", "test")) %>% 
  ggplot() +
  aes(x = cell_size, 
      y = value, 
      color = parameter,
      label = parameter) +
  geom_line(size = 1.1) +
  geom_dl(method = "smart.grid") +
  guides(color = FALSE) +
  geom_hline(yintercept = c(.05, .80, .90, .95), linetype = "dashed") +
  scale_y_continuous(NULL, limits = c(0, 1), 
                     labels = scales::percent_format(), 
                     breaks = seq(.1, 1, .1)) +
  labs(title = "Power as a function of cell size and main effects") +
  facet_wrap(~test) +
  theme_bw()



# Recalculate power  ------------------------------------------------------
# TODO: change effect, not sample size
samples = 1000 # Number of samples for each n
sample_increment = 20 # The increment of cell sizes for the simulation
max_n = 120 # The maximum cell size
mu = 3.5728 # Estimated from the data using MLE
size = 0.5758 # Estimated from the data using MLE
max_obs = 17 # Maximum number of extra tasks
difference = 1.7 # The difference of the treatment group from the control
alpha = .05 # Level of significance
zprop = .34 # Proportion of zero values
max_diff = 6.2 # Maximum difference 
inc_diff = .4

power_matrix <-
  crossing(cell_size = 95,
           difference = seq.int(difference, max_diff, inc_diff),
           sample = 1:samples)

source("R/rtnegbin.R")
source("R/tidy.zinf.R")
source("R/rtzitnb.R")

# Create data
simulation_data <-
  power_matrix %>% 
  as.list() %>% 
  # Use parallel processing to generate datasets(samples) based on parameters
  future_pmap(.progress = TRUE, 
              ~crossing(group = fct_inorder(c("Fixed-Control", 
                                             "Fixed-Mindfulness",
                                             "Growth-Control",
                                             "Growth-Mindfulness")),
                        id = 1:..1) %>% 
# Generate the number of extra tasks using truncated negative binomial distribution
# as the maximum number of extra tasks is 17.
              mutate(extra_task = c(rtzitnb(n = ..1, size = size, mu = mu, max_obs = max_obs, zprop = zprop), 
                                    rtzitnb(n = ..1, size = size, mu = mu + ..2/2, max_obs = max_obs, zprop = zprop), 
                                    rtzitnb(n = ..1, size = size, mu = mu + ..2/2, max_obs = max_obs, zprop = zprop), 
                                    rtzitnb(n = ..1, size = size, mu = mu + ..2, max_obs = max_obs, zprop = zprop))) %>% 
              separate(group, into = c("mindset", "intervention"))
  )

simulation_data[[1000]] %>% 
  ggplot() +
  aes(x = extra_task) +
  geom_histogram() +
  facet_grid(mindset ~ intervention)
  
simulation_data[[1000]] %>% 
  group_by(mindset, intervention) %>% 
  summarise(avg_extra = mean(extra_task),
            sd_extra = sd(extra_task))

processed_final %>% 
  group_by(mindset, intervention) %>% 
  summarise(avg_extra = mean(extra_tasks),
            sd_extra = sd(extra_tasks))

simulation_data[[2000]]$extra_task %>% qplot()
processed_final$extra_tasks %>% qplot()

library(pscl)

# Calculate statistics
simulation_results <-
  simulation_data %>% 
  future_map(.progress = TRUE, 
             # Use poisson regression to test hypothesis
             ~zeroinfl(extra_task ~ intervention * mindset, dist = "negbin",
                       data = .x) %>% 
             # Get Relative Risks and confidence intervals for the predictors
               tidy.zinf(exponentiate = FALSE, conf.int = FALSE))


# Calculate power
p_intervention <- 
  simulation_results %>% 
  map_dbl(.progress = TRUE, 
           ~filter(.x, term == "interventionMindfulness" & model == "count") %>% 
             # Calculate one-sided Wald statistic
             mutate(larger = pnorm(statistic, lower.tail = FALSE)) %>% 
             pull(larger)) %>% 
  # Put the p values into the design matrix
  bind_cols(power_matrix, p = .)

p_mindset <- 
  simulation_results %>% 
  map_dbl(.progress = TRUE, 
           ~filter(.x, term == "mindsetGrowth" & model == "count") %>% 
           # Calculate one-sided Wald statistic
           mutate(larger = pnorm(statistic, lower.tail = FALSE)) %>% 
           pull(larger)) %>% 
  # Put the p values into the design matrix
  bind_cols(power_matrix, p = .)

result_summary <-
  left_join(p_intervention, p_mindset, by = c("cell_size", "sample", "difference")) %>% 
  mutate(sig_int = p.x <= alpha,
         sig_mind = p.y <= alpha,
         sig_both = sig_int & sig_mind,
         sig_any = sig_int | sig_mind) %>% 
  group_by(difference) %>% 
  summarise(power_both = mean(sig_both, na.rm = TRUE),
            power_any = mean(sig_any, na.rm = TRUE))

result_summary

result_summary %>% 
  gather(parameter, value, power_both:power_any) %>% 
  separate(parameter, c("parameter", "test")) %>% 
  ggplot() +
  aes(x = difference, 
      y = value, 
      color = parameter,
      label = parameter) +
  geom_line(size = 1.1) +
  geom_dl(method = "smart.grid") +
  guides(color = FALSE) +
  geom_hline(yintercept = c(.05, .80, .90, .95), linetype = "dashed") +
  scale_y_continuous(NULL, limits = c(0, 1), 
                     labels = scales::percent_format(accuracy = 1), 
                     breaks = seq(.1, 1, .1)) +
  labs(title = "Power as a function of cell size and main effects") +
  facet_wrap(~test)


# Sandbox ------------------------------------------------------------


qplot(processed_final$extra_tasks)

sum(processed_final$extra_tasks == 0)/nrow(processed_final)

zero_one <- rbinom(400, 1, 1-zprop)
ifelse(zero_one == 0, 
        0, 
        rtnegbin(n = sum(zero_one == 1), 
                 size = .5758, 
                 mu = 4, 
                 max_obs = 16) + 1) %>% 
qplot()


mean(zero_one)
  


simulation_data[1000:1015] %>% 
  bind_rows(.id = "nr") %>% 
  ggplot() +
  aes(x = extra_task) +
  geom_histogram() +
  facet_wrap(~nr)
  

rtnegbin(n = ..1, size = size, mu = mu, max_obs = max_obs)

rtnegbin(n = ..1, size = size, mu = mu, max_obs = max_obs)

