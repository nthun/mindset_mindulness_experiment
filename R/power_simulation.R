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
cell_sizes = 10 # The increment of cell sizes for the simulation
max_n = 200 # The maximum cell size
lambda = 3 # The average number of extra tasks finished by the control group
difference = .7 # The difference of the treatment group from the control
alpha = .05

# Simulate samples ------------------------------------------------------------------
# Generate a design matrix that defines the samples that will be simulated
power_matrix <-
  crossing(cell_size = seq.int(30, max_n, cell_sizes),
           sample = 1:samples) 

# Calculate the p values for each sample, based on the population parameters
simulation_result <-
  power_matrix %>% 
  as.list() %>% 
  # Use parallel processing to generate datasets(samples) based on parameters
  # Then test the hypothesis on each dataset, and return the one-sided p-value
  future_pmap_dbl(.progress = TRUE, 
                  ~crossing(.x, 
                            group = fct_inorder(c("Control", "Mindfulness")),
                            id = 1:..1) %>% 
                # Generate the number of extra tasks using truncated poisson distribution
                # as the maximum number of extra tasks is 10.
                mutate(extra_task = c(rtpois(n = ..1, lambda = lambda, b = 15), 
                                      rtpois(n = ..1, lambda = lambda + difference, b = 15))) %>%
                # Use poisson regression to test hypothesis
                glm(extra_task ~ group, family = "poisson", data = .) %>% 
                broom::tidy() %>% 
                filter(term == "groupMindfulness") %>% 
                # Calculate one sided Wald statistic
                mutate(larger = pnorm(statistic, lower.tail = FALSE)) %>% 
                pull(larger)) %>% 
  # Put the p values into the design matrix
  bind_cols(power_matrix, p = .)


# Calculate the power, based on the samples -----------------------------------------
# Calculate the average p value and power for each cell size
result_summary <-
  simulation_result %>% 
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

