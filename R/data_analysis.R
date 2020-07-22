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
library(broom)

theme_set(theme_light())


# Define scale recoding -------------------------------------------------------------
# se_
agree_4 <- 
  c("Egyáltalán nem értek egyet" = 1,
    "Nem értek egyet" = 2,
    "Egyetértek" = 3,
    "Teljesen egyetértek" = 4
  )

# ms_, mc_3
agree_6 <- 
  c(      "Egyáltalán nem értek egyet" = 1,
          "Nem értek egyet" = 2,
          "Inkább nem értek egyet" = 3,
          "Inkább egyetértek" = 4,
          "Egyetértek" = 5,
          "Teljes mértékben egyetértek" = 6
  )

# cams_
frequency_4 <-
  c("Egyáltalán nem / Ritkán" = 1,
    "Néha" = 2,
    "Gyakran" = 3,
    "Majdnem mindig" = 4
  )

# sc_
frequency_5 <-
  c("Szinte soha" = 1,
    "Ritkán" = 2,
    "Körülbelül az esetek felében" = 3,
    "Gyakran" = 4,
    "Szinte mindig" = 5
  )

# risc_
true_4 <- 
  c("Egyáltalán nem jellemző rám" = 1,
    "2" = 2,
    "3" = 3,
    "Teljes mértékben jellemző rám" = 4
  )

# grit_
true_5 <-
  c("Egyáltalán nem jellemző rám" = 1,
    "Kevéssé jellemző rám" = 2,
    "Valamennyire jellemző rám" = 3,
    "Nagyon jellemző rám" = 4,
    "Teljes mértékben jellemző rám" = 5
  )

# outro_experiences_
true_6 <- 
  c("Egyáltalán nem" = 1,
    "2" = 2, 
    "3" = 3,
    "4" = 4, 
    "6" = 5,
    "Nagyon" = 6
  )

# previous_iq_score
iq_5 <- c("Nem emlékszem." = NA,
          "Az átlagos alatt volt." = 1,
          "Átlagos volt." = 2,
          "Átlag fölött volt." = 3,
          "Sokkal az átlag fölött volt." = 4
)


# Read data ------------------------------------------------------------------
manual_exclude <- read_lines("data/exclude_by_survey_log.txt")

# Authenticate using a PAT
osf_pat <- read_lines("osf_pat.txt")
osf_auth(token = osf_pat)

# Download data into data folder
osf_retrieve_file("v6kcd") %>%
  osf_download(path = "data/",
               # conflicts = "overwrite",
               progress = TRUE)

# Read data
mfs_raw <- qualtRics::read_survey("data/IQ_MS_MFS_2019_April+6,+2020_13.00.csv")

# Process data
mfs <- 
  mfs_raw %>% 
  clean_names() %>% 
  # Remove trial 
  filter(status != "Survey Preview") %>% 
  # Keep only finished experiments
  filter(finished == TRUE) %>% 
  # Exclude pilot run based on research log
  filter(date(start_date) != "2019-03-18") %>% 
  filter(date(start_date) != "2019-04-01") %>% 
  filter(date(start_date) != "2019-04-08") %>% 
  # Exclude participants based on equipment malfunction (source: lab log), or suspicion (manual coding of open ended suspicion check question)
  filter(!response_id %in% manual_exclude) %>% 
  # Exclude those who did not pay attention during the experiment
  filter(!str_detect(mc_2, "6|7")) %>% 
  # Recode the manipulated variables for better readability
  mutate( mindset = case_when(fl_54_do == "Incrementalmindsetmanipulation" ~ "Growth",
                              fl_54_do == "Fixedmindsetmanipulation" ~ "Fixed"),
          intervention = case_when(fl_56_do == "Mfsintervention" ~ "Mindfulness",
                                   fl_56_do == "Controlintervention"  ~ "Control"))




# Keep only the selected variables
mfs_scales <-
  mfs %>%
  # replace ő-s with o-s
  mutate(across(where(is.character), enc2native)) %>% 
  transmute(
    # ids 
    response_id, start_date, end_date, comput_id,
    # demographic info
    gender = recode(gender, !!!gender_cat), 
    age = year(start_date) - birth_year, 
    education, term_no, field,
    # group allocation
    mindset, intervention, 
    # recode scales to numeric values
    across(matches("^ms_\\d+"), ~recode(.x, !!!agree_6)),
    across(matches("^cams_\\d+"), ~recode(.x, !!!frequency_4)),
    across(matches("^sc_\\d+"), ~recode(.x, !!!frequency_5)),
    across(matches("^se_\\d+"), ~recode(.x, !!!agree_4)),
    across(matches("^risc_\\d+"), ~recode(.x, !!!true_4)),
    across(matches("^grit_\\d+"), ~recode(.x, !!!true_5)),
    across(mc_1:mc_2, ~str_extract(.x, "\\d+") %>% as.numeric()),
    mc_3 = recode(mc_3, !!!agree_6),
    across(matches("^outro_experiences_\\d+"), ~recode(.x, !!!true_6)),
    across(matches("^previous_iq_score"), ~recode(.x, !!!iq_5)),
    across(matches("agt_p_\\d+")),
    # iq related questions
    iq_assessed, iq_importance, iq_confid_1,
    # consumption
    mediatation, coffee, alcohol, drug = q325, 
    # suspicion check questions
    what1, what2, 
  )

# Calculate the number of extra tasks and add it to the rest of the variables
processed_data <-
  mfs %>% 
  select(response_id, matches("iq_prac_[^q]*\\d+$")) %>%
  pivot_longer(names_to = "task", 
               values_to = "answer", 
               values_drop_na = TRUE,
               cols = -response_id) %>% 
  count(response_id, name = "extra_tasks") %>% 
  right_join(mfs_scales, by = "response_id") %>% 
  mutate(extra_tasks = if_else(is.na(extra_tasks), 0L, extra_tasks))

# Processed data ready
# write_excel_csv(processed_data, "data/processed_data_labels.csv")



# Create and compare models ---------------------------------------------------------
processed_data <- read_csv("data/processed_data_labels.csv")

mod_1 <- glm(extra_tasks ~ mindset * intervention, 
           family = "poisson", 
           data = processed_data)

mod_2 <- glm(extra_tasks ~ mindset + intervention, 
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
         data = processed_data) 

mod_4 <- zeroinfl(extra_tasks ~ mindset * intervention, dist = "negbin",
                  data = processed_data)

mod_5 <- hurdle(extra_tasks ~ mindset * intervention, 
                  data = processed_data)

summary(mod_5)

doParallel::registerDoParallel()

tab_model(mod_3, mod_4, mod_5,
          show.aic = TRUE, show.stat = TRUE, p.style = "numeric", 
          show.loglik = TRUE,
          show.dev = TRUE, 
          show.fstat = TRUE,
          bootstrap = TRUE,
          iterations = 1000,
          show.zeroinf = TRUE)


# Correct p value to one-sided test 

summary(mod_5)

# One sided tests

# Mod 3
pnorm(2.366, lower.tail = FALSE)

# Mod 4
pnorm(1.127, lower.tail = FALSE)

anova(mod_1, mod_2, test = "LRT")


performance::check_distribution(mod_2) %>% plot()
performance::(mod_1)
processed_data %>% 
  summarise(mean(extra_tasks == 0))
