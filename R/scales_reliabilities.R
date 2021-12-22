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
  

# IQ tasks
iq_correct_answers <- read_csv("data/iq_correct_answers.csv")

iq_scores <-
  mfs %>% 
  select(response_id, iq_correct_answers$task) %>% 
  pivot_longer(cols = -response_id,
               names_to = "task",
               values_to = "answer") %>% 
  # Unanswered - e.g. out of time - questions will count as bad answers
  mutate(answer = if_else(is.na(answer), 0, answer)) %>% 
  left_join(iq_correct_answers, by = "task") %>% 
  group_by(response_id, block) %>% 
  summarise(pct_correct = mean(answer == correct_answer)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = "block",
              values_from = "pct_correct", 
              names_prefix = "iq_")

iq_scores %>% 
  pivot_longer(-response_id) %>% 
  ggplot() +
  aes(x = value, fill = name) +
  geom_density(alpha = .5) +
  scale_x_continuous(labels = scales::percent_format())

temp <- 
processed_final %>%
  select(response_id, gender, age, education, term_no, mindset, intervention, 
         iq_assessed,
         ends_with("_mean"), iq_final:iq_warmup, extra_tasks) %>% 
  mutate(iq_change = mean(c(iq_real, iq_final)) - iq_warmup)
  
temp %>% 
  ggplot() +
  aes(x = iq_change) +
  geom_density()
  
temp_lm <-
  lm(scale(iq_change) ~ mindset * intervention + scale(extra_tasks), data = temp)

summary(temp_lm)

performance::check_model(temp_lm)

library(sjPlot)
tab_model(temp_lm, bootstrap = TRUE)

temp %>% 
  ggplot() +
  aes(x = scale(cams_mean), y = iq_change) +
  facet_grid(mindset ~ .) +
  geom_point(alpha = .5) +
  geom_smooth(method = lm) +
  geom_hline(yintercept = 0)

# Achievent goals
agt_p_sap <- c("agt_p_5","agt_p_8")
agt_p_sav <- c("agt_p_2", "agt_p_11")
agt_p_oap <- c("agt_p_3","agt_p_7")
agt_p_oav <- c("agt_p_6", "agt_p_9")
agt_p_tap <- c("agt_p_1","agt_p_10")
agt_p_tav <- c("agt_p_4","agt_p_12")

processed_final <- 
  processed_final %>% 
  mutate(sap_p_mean = rowMeans(select(., agt_p_sap)),
         agt_p_mean = rowMeans(select(., agt_p_sav)),
         agt_p_mean = rowMeans(select(., agt_p_oap)),
         agt_p_mean = rowMeans(select(., agt_p_oav)),
         agt_p_mean = rowMeans(select(., agt_p_tap)),
         agt_p_mean = rowMeans(select(., agt_p_tav))
  )
