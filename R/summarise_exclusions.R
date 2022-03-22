# Script to calculate the number of excluded participants based on different criteria

library(tidyverse)
library(qualtRics)
library(janitor)
library(lubridate)

# Read data
mfs_raw <- qualtRics::read_survey("data/IQ_MS_MFS_2019_April+6,+2020_13.00.csv")


exclusions <-
  mfs_raw %>% 
  clean_names() %>% 
  transmute(response_id,
            start_date, comput_id,
            exclude_unfinished = (status == "Survey Preview" | finished != TRUE) |
                                 (date(start_date) %in% date(c("2019-03-18", 
                                                               "2019-04-01", 
                                                               "2019-04-08"))),
            exclude_attention = str_detect(mc_2, "6|7"),
            exclude_manual = response_id %in% manual_exclude,
            what1,
            what2) %>% 
  arrange(-exclude_unfinished,
          exclude_manual,
          exclude_attention)

write_excel_csv(exclusions, "data/exclusions.csv")

exclusions %>% 
  summarise(across(starts_with("exclude"), sum, na.rm = TRUE))

