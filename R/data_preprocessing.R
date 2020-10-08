library(tidyverse)
library(osfr)
library(qualtRics)
library(janitor)
library(lubridate)
library(psych)

# Define scale recoding -------------------------------------------------------------
# se_
agree_4 <- 
  c("Egyáltalán nem értek egyet" = 0,
    "Nem értek egyet" = 1,
    "Egyetértek" = 2,
    "Teljesen egyetértek" = 3
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

# gender
gender_cat <- c("Férfi" = "Male",
                "Nő" = "Female")

# Read data ------------------------------------------------------------------
manual_exclude <- read_lines("data/exclude_by_survey_log.txt")

# Authenticate using a PAT
osf_pat <- read_lines("osf_pat.txt")
osf_auth(token = osf_pat)

# Download data into data folder
osf_retrieve_file("v6kcd") %>%
  osf_download(path = "data/",
               conflicts = "overwrite",
               progress = TRUE)

# Read data
mfs_raw <- qualtRics::read_survey("data/IQ_MS_MFS_2019_April+6,+2020_13.00.csv")

# Clean the  data
mfs <- 
  mfs_raw %>% 
  clean_names() %>% 
  # Remove trials
  filter(status != "Survey Preview") %>% 
  # Keep only finished experiments
  filter(finished == TRUE) %>% 
  # Exclude pilot runs based on research log
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

# Recode scale values to a number and keep important variables only
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
    across(matches("agq_\\d+")),
    across(matches("agt_p_\\d+")),
    # iq related questions
    iq_assessed, iq_importance, iq_confid_1,
    # consumption
    meditation = mediatation, coffee, alcohol, drug = q325, 
    # suspicion check questions
    what1, what2, 
  )


# Calculate IQ performance -----------------------------------------------------

# Calculate the proportion of correct answers for the IQ tasks
iq_correct_answers <- read_csv("data/iq_correct_answers.csv")

answers <- 
  mfs %>% 
  select(response_id, iq_correct_answers$task) %>% 
  pivot_longer(cols = -response_id,
               names_to = "task",
               values_to = "answer") %>% 
  # Unanswered - e.g. out of time - questions will count as bad answers
  mutate(answer = if_else(is.na(answer), 0, answer)) %>% 
  left_join(iq_correct_answers, by = "task")

iq_scores <-
  answers %>% 
  group_by(response_id, block) %>% 
  summarise(pct_correct = mean(answer == correct_answer), .groups = "drop") %>% 
  pivot_wider(names_from = "block",
              values_from = "pct_correct", 
              names_prefix = "iq_")

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
  mutate(extra_tasks = if_else(is.na(extra_tasks), 0L, extra_tasks)) %>% 
  left_join(iq_scores, by = "response_id") 


# Reverse items and calculate scales -------------------------------------------

processed_reversed <-
  processed_data %>% 
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

# Outro experiences Factror calculate -------------------------------

processed_data %>% 
  select(starts_with("outro_experiences_")) %>% 
  GGally::ggpairs()

processed_data %>% 
  select(starts_with("outro_experiences_")) %>% 
  nfactors(rotate = "oblimin", fm = "wls")


# 3 factor solution is chosen
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
         
         agq_sap = rowMeans(select(., c("agq_5","agq_8"))),
         agq_sav = rowMeans(select(., c("agq_2", "agq_11"))),
         agq_oap = rowMeans(select(., c("agq_3","agq_7"))),
         agq_oav = rowMeans(select(., c("agq_6", "agq_9"))),
         agq_tap = rowMeans(select(., c("agq_1","agq_10"))),
         agq_tav = rowMeans(select(., c("agq_4","agq_12"))),
         
         agt_p_sap = rowMeans(select(., c("agt_p_5","agt_p_8"))),
         agt_p_sav = rowMeans(select(., c("agt_p_2", "agt_p_11"))),
         agt_p_oap = rowMeans(select(., c("agt_p_3","agt_p_7"))),
         agt_p_oav = rowMeans(select(., c("agt_p_6", "agt_p_9"))),
         agt_p_tap = rowMeans(select(., c("agt_p_1","agt_p_10"))),
         agt_p_tav = rowMeans(select(., c("agt_p_4","agt_p_12")))
  ) %>% 
  # Attach factor scores
  bind_cols(fa_scores)

write_excel_csv(processed_final, "data/processed_final.csv")

processed_final
