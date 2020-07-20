# Dependencies
library(tidyverse)

# Import
survey <- read_csv("data/covid_sex_tech.csv")

# CLEAN SOCTECH VARIABLES -------------------------------------------------

# Select variables to analyze
soctech <- survey %>%
  select(starts_with("SOCTECH")) %>%
  select(-ends_with("QUAL")) %>%
  select(-c("SOCTECH_TRANSITION"))

# Label all the values
soctech_label <- within(soctech, {
  SOCTECH_CURENT <- recode(as.character(SOCTECH_CURENT), "0" = "No", "1" = "Yes")
  SOCTECH_SIGNUP <- recode(as.character(SOCTECH_SIGNUP), "0" = "No", "1" = "Yes", "2" = "Unsure",
                           "99" = "Unknown")
  SOCTECH_CHANGE <- recode(as.character(SOCTECH_CHANGE), "1" = "Increased", "2" = "Decreased",
                           "3" = "Same")
  SOCTECH_NEWS <- recode(as.character(SOCTECH_NEWS), "0" = "No", "1" = "Yes", "2" = "Unsure")
})

# Break up SOCTECH_WHICH
soctech_which_break <- str_split(soctech$SOCTECH_WHICH, ",") %>%
  unlist() %>%
  as_tibble()

# DESCRIPTIVE ANALYSES ----------------------------------------------------

# How many participants did not select social media?
sum(is.na(soctech$SOCTECH_WHICH))

# Describe SOCTECH_WHICH
soctech_which_break %>%
  filter(!is.na(value)) %>%
  group_by(value) %>%
  count() %>%
  arrange(desc(n)) %>%
  # 565 - 28 because 28 did not answer this question
  mutate(percent = n / sum(!is.na(soctech_which_break$value)))

# Likert-type items
# - SOCTECH_USEFUL
# - SOCTECH_IMPRESSION

# Possibly NLP for QUAL variables and:
# - SOCTECH_TRANSITION