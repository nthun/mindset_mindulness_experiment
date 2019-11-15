# Power analysis for the experimental study
library(tidyverse)
library(furrr) # For parallel processing
library(broom) # For tidying stat output
library(directlabels) # For plotting direct labels
library(extraDistr) # For truncated poisson distribution

theme_set(theme_light()) # For clean plot theme
plan(multisession(workers = availableCores())) # Setting parallel processing

# Define parameters -----------------------------------------------------------------

samples = 1000 # Number of samples for each n
cell_sizes = 30 # The increment of cell sizes for the simulation
max_n = 200 # The maximum cell size
lambda = 3 # The average number of extra tasks finished by the control group
difference = .7 # The difference of the treatment group from the control
alpha = .05

# Simulate samples ------------------------------------------------------------------
# Generate a design matrix that defines the samples that will be simulated
power_matrix <-
  crossing(cell_size = seq.int(cell_sizes, max_n, cell_sizes),
           sample = 1:samples) 

# Calculate the p values for each sample, based on the population parameters
simulation_data <-
  power_matrix %>% 
  as.list() %>% 
  # Use parallel processing to generate datasets(samples) based on parameters
  future_pmap(.progress = TRUE, 
              ~crossing(group = fct_inorder(c("Fixed-Control", 
                                              "Growth-Mindfulness")),
                        id = 1:..1) %>% 
                # Generate the number of extra tasks using truncated poisson distribution
                # as the maximum number of extra tasks is 15.
                mutate(extra_task = c(rtpois(n = ..1, lambda = lambda, b = 15), 
                                      rtpois(n = ..1, lambda = lambda + difference, b = 15)))
                
  )


# Test the hypothesis on each dataset
simulation_results <-
  simulation_data %>% 
  future_map(.progress = TRUE, 
                # Use poisson regression to test hypothesis
                ~glm(extra_task ~ group, family = "poisson", data = .x) %>% 
                broom::tidy(exponentiate = TRUE))

risk_ratios <-
  simulation_results %>% 
  map_dbl(.progress = TRUE, 
                 ~filter(.x, term == "groupMindfulness") %>% 
                  pull(estimate)) %>% 
  # Put the RR values into the design matrix
  bind_cols(power_matrix, rr = .)

risk_ratios %>% 
  group_by(cell_size) %>% 
  summarise(rr_mean = mean(rr),
            rr_se = sd(rr)/sqrt(n()))


risk_ratios %>% 
  ggplot() +
  aes(x = cell_size, y = rr, group = cell_size) +
  geom_boxplot() +
  expand_limits()
  

p_values <- 
  simulation_results %>% 
  future_map_dbl(.progress = TRUE, 
                  ~filter(.x, term == "groupMindfulness") %>% 
                  # Calculate one-sided Wald statistic
                  mutate(larger = pnorm(statistic, lower.tail = FALSE)) %>% 
                  pull(larger)) %>% 
  # Put the p values into the design matrix
  bind_cols(power_matrix, p = .)


# Calculate the power, based on the samples -----------------------------------------
# Calculate the average p value and power for each cell size
result_summary <-
  p_values %>% 
  mutate(significant = p <= alpha) %>% 
  group_by(cell_size) %>% 
  summarise(power = mean(significant),
            p = mean(p))

# Interpolate power values to see exact cut off point -------------------------------
# Interpolate data, so we can tell exact N without calculating it to exact number
interpolated_power <-
  tibble(cell_size = 1:max_n) %>% 
  left_join(result_summary, by = "cell_size") %>% 
  # Use cubic spline interpolation
  mutate(spline_power = stats::spline(
                        x = cell_size,
                        y = power,
                        xout = cell_size)$y)

# Calculate the exact number of participants needed in each cell for specific power
interpolated_power %>% 
  transmute(cell_size,
            `.80` = spline_power > .80,
            `.90` = spline_power > .90,
            `.95` = spline_power > .95) %>% 
  gather(power, value, -cell_size, convert = TRUE) %>% 
  filter(value) %>% 
  group_by(power) %>% 
  summarise(required_cell_size = first(cell_size))
  

# Visualize the results -------------------------------------------------------------
result_summary %>% 
  gather(parameter, value, power:p) %>% 
  ggplot() +
  aes(x = cell_size, 
      y = value, 
      color = parameter,
      label = parameter) +
  geom_line(size = 1.1) +
  geom_point(size = 2) +
  geom_dl(method = c("last.points")) +
  guides(color = FALSE) +
  geom_hline(yintercept = c(.05, .80, .90, .95), linetype = "dashed") +
  scale_y_continuous(limits = c(0, 1), 
                     labels = scales::percent_format(), 
                     breaks = seq(.1, 1, .1))





# Sandbox ---------------------------------------------------------------------------

cell_sizes = 100

# increase the number of groups

df <- 
  crossing(group = fct_inorder(c("Fixed-Control", 
                                 "Fixed-Mindfulness",
                                 "Growth-Control",
                                 "Growth-Mindfulness")),
         id = 1:cell_sizes) %>% 
  mutate(extra_task = c(rtpois(n = cell_sizes, lambda = lambda, b = 15), 
                        rtpois(n = cell_sizes, lambda = lambda + difference/2, b = 15), 
                        rtpois(n = cell_sizes, lambda = lambda + difference/2, b = 15), 
                        rtpois(n = cell_sizes, lambda = lambda + difference, b = 15))) %>% 
  separate(group, c("mindset", "intervention"))
  

df %>% 
  group_by(mindset, intervention) %>% 
  summarise(mean(extra_task))

glm(extra_task ~ intervention * mindset, family  = "poisson", data = df) %>% 
  tidy()
  
