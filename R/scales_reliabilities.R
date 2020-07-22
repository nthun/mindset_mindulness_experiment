library(readr)
library(dplyr)
library(psych)

processed_data_labels <- read_csv("data/processed_data_labels.csv")


# Reverse coding items ---------------------------------------------------------

processed_reversed <-
  processed_data_labels %>% 
# MS scale
  mutate(across(c(ms_1, ms_3, ms_4), 
                ~recode(.x, 
                        `1` = 6,
                        `2` = 5,
                        `3` = 4,
                        `4` = 3,
                        `5` = 2,
                        `6` = 1)),
# cognitive affective MFS (CAMS) scale
          across(c(cams_6, cams_7), 
                 ~recode(.x, 
                        `1` = 4,
                        `2` = 3,
                        `3` = 2,
                        `4` = 1)),
# self compassion scale
          across(c(sc_1, sc_4, sc_8, sc_9, sc_11, sc_12),
                 ~recode(.x, 
                         `1` = 5,
                         `2` = 4,
                         `3` = 3,
                         `4` = 2,
                         `5` = 1)),
# Rosenberg scale
          across(c(se_3, se_5, se_8, se_9), 
                ~recode(.x, 
                        `0` = 3,
                        `1` = 2,
                        `2` = 1,
                        `3` = 0)),
# GRIT scale
          across(c(grit_2, grit_4, grit_7, grit_8), 
                 ~recode(., 
                         `1` = 5,
                         `2` = 4,
                         `3` = 3,
                         `4` = 2,
                         `5` = 1
                 ))
    )

#Calculating alphas

processed_data %>% 
  select(response_id, ms_1:agt_p_12) %>% 
  # pivot_longer(-response_id,
  #              names_to = "item",
  #              values_to = "values") %>% 
  # extract(item, 
  #         into = c("questionnaire", NA), 
  #         regex = "(\\w+)_(\\d+)", 
  #         remove = FALSE) %>% 
  group_nest(questionnaire)
  alpha()
  
processed_data %>% 
  select(starts_with("se_")) %>% 
  alpha()

MS <- data.frame(processed_reversed[, 13:16])
alpha(MS)
CAMS <- data.frame(processed_reversed[, 17:28])
alpha(CAMS)
SC <- data.frame(processed_reversed[, 29:40])
alpha(SC)
RISC <- data.frame(processed_reversed[, 50:60])
alpha(RISC)
GRIT <- data.frame(processed_reversed[, 61:68])
alpha(GRIT)
MC <- data.frame(processed_reversed[, 69:71])
alpha(MC)


#Calculating means

processed_final <- processed_reversed %>%
  mutate(ms_mean = rowMeans(x = select(.data = ., starts_with(match = "ms_"))))
processed_final <- processed_final %>%
  mutate(rosenberg_mean = rowMeans(x = select(.data = ., starts_with(match = "se_"))))
processed_final <- processed_final %>%
  mutate(cams_mean = rowMeans(x = select(.data = ., starts_with(match = "cams_"))))
processed_final <- processed_final %>%
  mutate(sc_mean = rowMeans(x = select(.data = ., starts_with(match = "sc_"))))
processed_final <- processed_final %>%
  mutate(risc_mean = rowMeans(x = select(.data = ., starts_with(match = "risc_"))))
processed_final <- processed_final %>%
  mutate(grit_mean = rowMeans(x = select(.data = ., starts_with(match = "grit_"))))
processed_final <- processed_final %>%
  mutate(mc_mean = rowMeans(x = select(.data = ., starts_with(match = "mc_"))))
