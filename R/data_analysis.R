# Analyze the experiment

library(tidyverse)
library(osfr)
library(qualtRics)
library(janitor)
library(lubridate)
library(broom)
library(sjPlot)
library(pscl)
library(performance)

theme_set(theme_light())


processed_final <- read_csv("data/processed_final.csv")

# Data analysis
mod_1 <- glm(extra_tasks ~ mindset * intervention + gender, 
           family = "poisson", 
           data = processed_data)

mod_2 <- glm(extra_tasks ~ mindset + intervention + gender, 
             family = "poisson",
             data = processed_data)

anova(mod_1, mod_2, test = "Chisq")

tab_model(mod_1, mod_2, show.aic = TRUE, 
          show.stat = TRUE, 
          p.style = "numeric", 
          show.loglik = TRUE, 
          show.dev = TRUE, 
          show.zeroinf = TRUE)

check_overdispersion(mod_1)
check_zeroinflation(mod_1)

r2(mod_1)

processed_data %>% 
  ggplot() +
  aes(x = extra_tasks) +
  geom_histogram(bins = 20)

library(ggbeeswarm)
processed_data %>% 
  ggplot() +
  aes(x = mindset, 
      y = extra_tasks, 
      fill = interaction(intervention, mindset)) +
  geom_violin(alpha = .5) +
  geom_boxplot(fill = "white", outlier.alpha = 0, width = .2) +
  geom_quasirandom(alpha = .5) +
  guides(fill = FALSE) +
  facet_wrap(~intervention)

# 35% of participants completed 0 tasks

mod_3 <- zeroinfl(extra_tasks ~ mindset * intervention, dist = "poisson",
         data = processed_final) 

mod_4 <- zeroinfl(extra_tasks ~ mindset * intervention, dist = "negbin",
                  data = processed_final)

mod_5 <- hurdle(extra_tasks ~ mindset * intervention, 
                dist = "negbin", zero.dist = "negbin",
                data = processed_data)

summary(mod_5)

doParallel::registerDoParallel()

tab_model(mod_3, mod_4,
          show.aic = TRUE, show.stat = TRUE, p.style = "numeric", 
          show.loglik = TRUE,
          show.dev = TRUE, 
          show.fstat = TRUE,
          # bootstrap = TRUE,
          iterations = 1000,
          show.zeroinf = TRUE)


# Correct p value to one-sided test 

summary(mod_5)

# One sided tests

# Mod 3
pnorm(1.13, lower.tail = FALSE)

# Mod 4
pnorm(1.520, lower.tail = FALSE)

anova(mod_1, mod_2, test = "LRT")


performance::check_distribution(mod_2) %>% plot()
performance::(mod_1)
processed_data %>% 
  summarise(mean(extra_tasks == 0))


# Obtain % correct score from iq tests -----------------------------------------

library(lmerTest)

select()
lmer()

processed_data %>% 
  ggplot() +
  aes(x = mindset, fill = intervention, color = intervention, y = iq_warmup) +
  geom_violin(alpha = .5)
