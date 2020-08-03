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
soctech_label

# Break up SOCTECH_WHICH
soctech_which_break <- str_split(soctech$SOCTECH_WHICH, ",") %>%
  unlist() %>%
  as_tibble()

# DESCRIPTIVE ANALYSES ----------------------------------------------------

# How many participants did not select social media? 
# Note, this returns 28 and a subsequent analysis returns 27
sum(is.na(soctech$SOCTECH_WHICH))

# Describe SOCTECH_WHICH
soctech_which_break %>%
  filter(!is.na(value)) %>%
  group_by(value) %>%
  count() %>%
  arrange(desc(n)) %>%
  # 565 - 28 because 28 did not answer this question
  mutate(percent = n / 565)

# Other social networks use?
soctech_label %>%
  select(SOCTECH_OTHER) %>%
  filter(!is.na(SOCTECH_OTHER))

#######
# Continuous variables
#######

# How much have you used technology (e.g. Whatsapp, HouseParty, Zoom) to keep 
# connected to your social circles? 
soctech_label %>%
  select(SOCTECH_HOW_MUCH) %>%
  # Remove missing values n = 2
  filter(!is.na(SOCTECH_HOW_MUCH)) %>%
  # Sliding scale ranging from 0 to 100
  summarize(
    M = mean(SOCTECH_HOW_MUCH),
    SD = sd(SOCTECH_HOW_MUCH)
  )

# Have social networking platforms been useful during the social isolation?
soctech_label %>%
  select(SOCTECH_USEFUL) %>%
  # Remove missing values n = 27
  filter(!is.na(SOCTECH_USEFUL)) %>%
  # Likert-type item, so calculate M and SD
  summarize(
    M = mean(SOCTECH_USEFUL),
    SD = sd(SOCTECH_USEFUL)
  )

# What is your impression of social networking sites during social lockdown?
soctech_label %>%
  select(SOCTECH_IMPRESSION) %>%
  # Remove missing values n = 27
  filter(!is.na(SOCTECH_IMPRESSION)) %>%
  # Likert-type item, so calculate M and SD
  summarize(
    M = mean(SOCTECH_IMPRESSION),
    SD = sd(SOCTECH_IMPRESSION)
  )

#######
# Discrete variables
#######

# Do you currently have a profile on a social networking platform? 
soctech_label %>%
  select(SOCTECH_CURENT) %>%
  group_by(SOCTECH_CURENT) %>%
  count() %>%
  mutate(percent = n / 565)

# Have you signed up to any new platforms since the social isolation?
soctech_label %>%
  select(SOCTECH_SIGNUP) %>%
  group_by(SOCTECH_SIGNUP) %>%
  count() %>%
  mutate(percent = n / 565)

# How has your use of social networking platforms changed during social isolation?
soctech_label %>%
  select(SOCTECH_CHANGE) %>%
  group_by(SOCTECH_CHANGE) %>%
  count() %>%
  mutate(percent = n / 565)

# Have you used social networking platforms to keep up to date with the latest news?
soctech_label %>%
  select(SOCTECH_NEWS) %>%
  group_by(SOCTECH_NEWS) %>%
  count() %>%
  mutate(percent = n / 565)

# NLP - SOCTECH_TRANSITION ------------------------------------------------

# If you used SNS more, please describe the transition in moving from communication in-person to online?

# NLP - SOCTECH_SIGNUP_QUAL -----------------------------------------------

# If you signed up for new SNS, which platforms have you signed up to?

# NLP - SOCTECH_USEFUL_QUAL -----------------------------------------------

# If SNS has been useful during social isolation, how have they been useful?

# NLP - SOCTECH_NEWS_QUAL -------------------------------------------------

# How have you used SNS to keep up to date with the latest news?

# NLP - SOCTECH_IMPRESSION_QUAL -------------------------------------------

# Please explain your impression of SNS during social lockdown?
