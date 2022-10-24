# Analyze the experiment

library(tidyverse)
library(broom)
library(sjPlot)
library(pscl)
library(performance)
library(ggbeeswarm)

theme_set(theme_light())

# Read data
processed_final <- read_csv("data/processed_final.csv")

# Data analysis
mod_1 <- glm(extra_tasks ~ mindset * intervention, 
           family = "poisson", 
           data = processed_final)

mod_2 <- glm(extra_tasks ~ mindset + intervention, 
             family = "poisson",
             data = processed_final)

anova(mod_1, mod_2, test = "Chisq")

tab_model(mod_1, mod_2, 
          show.aic = TRUE, 
          show.stat = TRUE, 
          p.style = "numeric", 
          show.loglik = TRUE, 
          show.dev = TRUE, 
          show.zeroinf = TRUE)

check_overdispersion(mod_1)
check_zeroinflation(mod_1)

r2(mod_1)

processed_final %>% 
  ggplot() +
  aes(x = extra_tasks) +
  geom_histogram(bins = 20)


processed_final %>% 
  ggplot() +
  aes(x = mindset, 
      y = extra_tasks, 
      fill = mindset) +
  geom_violin(alpha = .7) +
  geom_quasirandom(alpha = .5) +
  geom_boxplot(fill = "white", outlier.alpha = 0, width = .2, alpha = .5) +
  guides(fill = FALSE) +
  facet_wrap(~intervention)

# 35% of participants completed 0 tasks

mod_3 <- MASS::glm.nb(extra_tasks ~ mindset * intervention,
                      data = processed_final)

# Zero inflated poisson model
mod_4 <- zeroinfl(extra_tasks ~ mindset * intervention, dist = "poisson",
         data = processed_final) 

# Zero inflated negative binomial model (should be used)
mod_5 <- zeroinfl(extra_tasks ~ mindset * intervention, dist = "negbin",
                  data = processed_final)

tab_model(mod_3, mod_4, mod_5,
          show.aic = TRUE, 
          show.stat = TRUE, 
          p.style = "numeric", 
          show.loglik = TRUE,
          show.dev = TRUE, 
          show.fstat = TRUE,
          # bootstrap = TRUE,
          # iterations = 1000,
          show.zeroinf = TRUE)

modelsummary::modelsummary(mod_3, mod_4, mod_5)

# One sided tests for the mindset intervention
# Mod 4
pnorm(1.13, lower.tail = FALSE)

performance::check_model(mod_3)

# Obtain % correct score from iq tests -----------------------------------------

library(lmerTest)
mfs_raw %>% 
  select(mc_1:mc_3) %>% 
  view()

# Manipulation check
processed_final %>% 
  select(response_id, mindset, intervention, ms_mean, cams_mean, mc_1, mc_3) %>% 
  lm(-mc_3 ~ mindset, data = .) %>% 
  summary()

mfs_raw %>% 
  select(mc_1) %>% 
  view()
  
processed_final %>% 
  ggplot() +
  aes(x = mindset, y = mc_3, fill = mindset) +
  geom_boxplot() +
  scale_fill_viridis_d() +
  labs(title = "Your intelligence is an attribute that you cannot change over time",
       y = "1-6 Likert scale")


processed_final %>% 
  select(response_id, mindset, intervention, ms_mean, cams_mean, mc_1, mc_3) %>% 
  lm(mc_1 ~ intervention + cams_mean, data = .) %>% 
  summary()

processed_final %>% 
  select(response_id, mindset, matches("agq_\\w{3}"), matches("agt_p_\\w{3}")) %>% 
  pivot_longer(agq_sap:agt_p_tav, 
               names_to = "variable") %>% 
  nest_by(variable) %>% 
  ungroup() %>% 
  mutate(model = map(data, ~lm(value ~ mindset, data = .x) %>% 
                       broom::tidy()
                     )) %>% 
  unnest(model) %>% 
  filter(term != "(Intercept)")


processed_final %>% 
  select(response_id, mindset, matches("agq_\\w{3}"), matches("agt_p_\\w{3}")) %>% 
  pivot_longer(agq_sap:agt_p_tav, 
               names_to = "variable") %>% 
  nest_by(variable) %>% 
  ungroup() %>% 
  mutate(model = map(data, ~lm(value ~ mindset, data = .x) %>% 
                       broom::tidy())) %>% 
  unnest(model) %>% 
  filter(term != "(Intercept)")

# Difference between practice real test achievenent goals
processed_final %>% 
  select(response_id, mindset, matches("agq_\\w{3}"), matches("agt_p_\\w{3}")) %>% 
  pivot_longer(agq_sap:agt_p_tav, 
               names_to = "variable") %>% 
  mutate(questionnaire = str_extract(variable, "^.{3}"),
         scale = str_extract(variable, ".{3}$")) %>% 
  group_by(response_id, scale) %>% 
  arrange(response_id, scale, questionnaire) %>% 
  mutate(difference = lead(value) - value) %>% 
  drop_na(difference) %>% 
  ungroup() %>% 
  nest_by(scale) %>% 
  ungroup() %>% 
  mutate(model = map(data, ~lm(difference ~ 1, data = .x) %>% 
                            broom::tidy())) %>% 
  unnest(model)

processed_final %>% 
  glimpse()
  
processed_final %>% 
  mutate(iq_mean = (iq_final + iq_real)/2) %>%
  lm(iq_mean ~ extra_tasks + mindset * intervention + cams_mean, data = .) %>% 
  summary()

processed_final %>% 
  mutate(iq_mean = (iq_final + iq_real + iq_warmup)/2) %>%
  ggplot() +
  aes(x = cams_mean, y = iq_mean, color = mindset) +
  geom_point() +
  geom_smooth(method = lm)

zeroinfl(extra_tasks ~ mindset * intervention + iq_real, dist = "negbin", data = processed_final) %>% 
  tab_model()

lm(iq_warmup ~ mindset, data = processed_final) %>% 
  tab_model()

processed_final %>% 
  ggplot +
  aes(x = iq_importance) +
  geom_bar()
  
  zeroinfl(extra_tasks ~ mindset * intervention + iq_confid_1, dist = "negbin", data = processed_final) %>% 
  tab_model()

processed_final %>% 
  distinct(iq_importance)

processed_final %>% 
  mutate(iq_importance = case_when(iq_importance == "Egyáltalán nem fontos." ~ 1L,
                                      iq_importance == "Valamennyire fontos." ~ 2L,
                                      iq_importance == "Közepesen fontos." ~ 3L,
                                      iq_importance == "Fontos." ~ 4L,
                                      iq_importance == "Különösen fontos." ~ 5L,
                                      TRUE ~ as.integer(iq_importance))) %>% 
  zeroinfl(extra_tasks ~ mindset * intervention + scale(iq_importance), 
           dist = "negbin", data = .) %>% 
  tab_model()

library(corrr)
processed_final %>% 
  select(iq_final, iq_real, iq_warmup) %>% 
  correlate()

processed_final %>% glimpse()

model1 <- lm(out_1 ~ mindset * intervention , data = processed_final) 

model1%>% 
  summary()
  

library(broom)

augment(model1) %>%
  group_by(mindset, intervention) %>% 
  summarise(avg_fit = mean(.fitted),
            se_fit = sd(.fitted)/sqrt(n())) %>% 
  ungroup() %>% 
  ggplot() +
  aes(x = mindset, y = avg_fit, fill = intervention) +
  geom_linerange(aes(ymax = avg_fit + se_fit, 
                     ymin = avg_fit - se_fit), position = position_dodge(width = 1)) +
  geom_col(position = "dodge", alpha = .7)

augment(model1) %>%
  group_by(mindset, intervention) %>% 
  summarise(avg_fit = mean(out_1),
            se_fit = sd(out_1)/sqrt(n())) %>% 
  ungroup() %>% 
  ggplot() +
  aes(x = mindset, y = avg_fit, fill = intervention) +
  geom_linerange(aes(ymax = avg_fit + se_fit, 
                     ymin = avg_fit - se_fit), position = position_dodge(width = 1)) +
  geom_col(position = "dodge", alpha = .7)


library(lmerTest)

processed_final %>% 
  mutate(iq_diff = iq_final - iq_real) %>% 
  lm(iq_diff ~ extra_tasks, data = .) %>% 
  summary()

processed_final %>% 
  transmute(extra_tasks = if_else(extra_tasks == 0, NA_real_, extra_tasks)) %>% 
  ggplot(aes(x = extra_tasks)) + geom_histogram()


cams_mod <- 
  zeroinfl(extra_tasks ~ mindset * intervention * scale(cams_mean),
           dist = "negbin",
           data = processed_final) 

library(pscl)
cams_pred <- predict(cams_mod, type = "response")
qplot(cams_pred)


# output$log.obs <- log(output$obs)
# output$log.preds.count <- log(output$preds.count)

output[!is.na(output$log.obs) & !is.na(output$log.preds.count),] %>% 
  ggplot() + 
  aes(x = log.obs, 
      y = log.preds.count) +
  geom_jitter(alpha = 0.3, width = .15, size = 2) + 
  geom_smooth(col = "blue") + 
  labs(x = "Observed count (non-zero, natural logarithm)", 
       y = "Predicted count (non-zero, natural logarithm)")

  processed_final %>% 
    transmute(extra_tasks,
              .fitted = predict(cams_mod, type = "response"),
              log_obs = log(extra_tasks),
              log_fitted = log(.fitted)) %>%
    filter(extra_tasks != 0) %>% 
  ggplot() +
  aes(x = log_obs, y = log_fitted) +
  geom_point(alpha = .3, size = 2) +
  geom_smooth() +
  # scale_x_continuous(limits = c(1,4)) +
    labs(x = "Observed count (non-zero, natural logarithm)", 
         y = "Predicted count (non-zero, natural logarithm)")

calibration <- 
    processed_final %>% 
    transmute(extra_tasks,
              .fitted = predict(cams_mod, type = "response"),
              log_obs = log(extra_tasks),
              log_fitted = log(.fitted)) %>%
    filter(extra_tasks != 0) %>% 
    lm(log_fitted ~ log_obs, data = .) %>% 
    summary()

calibration$sigma

summary()

install.packages("fitdistrplus")

fitdistrplus::fitdist(processed_final$extra_tasks, distr = "nbinom")
fitdistrplus::fitdist((rpois(10000, 5)), distr = "pois")
fitdistrplus::fitdist(rnorm(100, 10, 2), distr = "norm", method = "mle")

rnbinom(370, mu = 3.57, size = .57) %>% pmin(17) %>% 
  fitdistrplus::fitdist(., distr = "nbinom")

(family = MASS::negative.binomial(theta = 1))

cams_mod <- 
  zeroinfl(extra_tasks ~ intervention * scale(cams_mean),
           dist = "negbin",
           data = processed_final) 

processed_final %>% 
  mutate(.fitted = predict(cams_mod, type = "response")) %>% 
  ggplot() +
  aes(x = cams_mean, y = extra_tasks, color = intervention) +
  geom_point(alpha = .5) +
  # facet_wrap(~mindset) +
  geom_line(aes(y = .fitted), size = 1.2) +
  labs(color = NULL,
       y = "Solved optional extra tasks",
       x = "Dispositional mindfulness") +
  theme(legend.position = "top")

processed_final %>% 
  select(starts_with("cams_")) %>% 
  skim()


processed_final %>% 
  bind_cols(.fitted = predict(moderators_m1$model[[2]])) %>% 
  ggplot()

# EFA out 
fa_3 <-
  processed_final %>% 
  select(starts_with("outro_experiences_")) %>% 
  psych::fa(nfactors = 3, fm = "wls")

fa_3

fa_scores <- 
  fa_3$scores %>% 
  as_tibble() %>% 
  rename(out_ = everything()) 