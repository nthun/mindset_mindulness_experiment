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
          across(c(cams_2, cams_6, cams_7), 
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
                 )),
    meditation = fct_relevel(meditation, "Soha")
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
  
processed_reversed %>% 
  select(starts_with("se_")) %>% 
  alpha()

processed_reversed %>% 
  select(starts_with("ms_")) %>% 
  alpha()

processed_reversed %>% 
  select(starts_with("sc_")) %>% 
  alpha()

processed_reversed %>% 
  select(starts_with("cams_")) %>% 
  alpha()

processed_reversed %>% 
  select(starts_with("grit_")) %>% 
  alpha()

processed_reversed %>% 
  select(starts_with("risc_")) %>% 
  alpha()

processed_reversed %>% 
  select(starts_with("agt_p")) %>% 
  alpha()


# Outro experiences EFA --------------------------------------------------------

processed_data %>% 
  select(starts_with("outro_experiences_")) %>% 
  GGally::ggpairs()

processed_data %>% 
  select(starts_with("outro_experiences_")) %>% 
  nfactors(rotate = "oblimin", fm = "wls")


# 3 factor solution
fa_3 <-
  processed_data %>% 
  select(starts_with("outro_experiences_")) %>% 
  psych::fa(nfactors = 3, fm = "wls")


fa_scores <- 
  fa_3$scores %>% 
  as_tibble() %>% 
  rename(out_ = everything()) 

  
# Calculating scales by mean ---------------------------------------------------

processed_final <- 
  processed_reversed %>%
  mutate(ms_mean = rowMeans(x = select(., starts_with("ms_"))),
         se_mean = rowMeans(x = select(., starts_with("se_"))),
         cams_mean = rowMeans(x = select(., starts_with("cams_"))),
         sc_mean = rowMeans(x = select(., starts_with("sc_"))),
         risc_mean = rowMeans(x = select(., starts_with("risc_"))),
         grit_mean = rowMeans(x = select(., starts_with("grit_"))),
         agp_t_mean = rowMeans(x = select(., starts_with("agt_p_")))
        ) %>% 
  bind_cols(fa_scores)

write_excel_csv(processed_final, "data/processed_final.csv")


# Sandbox ----------------------------------------------------------------------


zeroinfl(extra_tasks ~ mindset * intervention * se_mean, dist = "negbin",
                  data = processed_final) %>% 
  summary()

zeroinfl(extra_tasks ~ mindset * intervention * cams_mean, dist = "negbin",
         data = processed_final) %>% 
  summary()

temp <-
  MASS::glm.nb(extra_tasks ~ 
             gender + age +
             mindset * intervention *
             scale(cams_mean) +
             scale(se_mean) +
             scale(sc_mean) +
             scale(grit_mean)
           , 
data = processed_final)

# temp2 <-
  zeroinfl(extra_tasks ~ 
           gender + age +
           mindset * intervention *
           # scale(se_mean)
           scale(cams_mean)
           # scale(sc_mean) +
           # scale(grit_mean)
           , 
           dist = "negbin",
           data = processed_final) %>% 
    summary()

tab_model(temp2, bootstrap = TRUE)

summary(temp)
check_collinearity(temp)

qplot((processed_final$meditation))

check_overdispersion(temp)
check_zeroinflation(temp)

processed_final %>% 
  ggplot() +
  aes(x = cams_mean, y = extra_tasks, color = intervention) +
  geom_point(alpha = .5) +
  geom_smooth(method = lm) +
  facet_grid(.~intervention)
  



