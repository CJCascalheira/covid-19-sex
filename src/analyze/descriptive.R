# Dependencies
library(tidyverse)

# Import
survey <- read_csv("data/covid_sex_tech.csv")

# Drop other columns
survey_1 <- survey[,c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18)]

# Condense ethnicity
survey_2 <- within(survey_1, {
  ETHNIC <- recode(as.character(ETHNIC), "White Irish" = "White Other", 
                   "White Black Caribbean" = "Mixed Ethnicity", "White Black African" = "Mixed Ethnicity", 
                   "White Asian" = "Mixed Ethnicity", "Other Mixed" = "Mixed Ethnicity", 
                   "Indian" = "South Asian", "Pakistani" = "South Asian", "Bangladeshi" = "South Asian", 
                   "Chinese" = "Other Asian", "African" = "African / Carribean", 
                   "Carribean" = "African / Carribean", "Other Black" = "Other Ethnicity",
                   "Other Ethnic" = "Other Ethnicity")
  COUNTRY <- recode(as.character(COUNTRY), "England" = "UK", "Scotland" = "UK", "Wales" = "UK")
  CURRENT_LIVING <- recode(as.character(CURRENT_LIVING), "Living w/ Children" = "Living w/ Family", 
                           "Living w/ Friends" = "Living w/ Non-Family", 
                           "Living w/ Other Family" = "Living w/ Family", 
                           "Living w/ Others" = "Living w/ Non-Family", "6" = "Living w/ Partner")
})

# PARTICIPANT DEMOGRAPHICS ------------------------------------------------

# Narrative descriptive statistics

# Mean and SD of age
survey_2 %>%
  summarize(M = mean(AGE),
            SD = sd(AGE)) %>%
  as.data.frame()

# Average health status
survey_2 %>%
  filter(HEALTH_STATUS <- !is.na(HEALTH_STATUS)) %>%
  summarize(M = mean(HEALTH_STATUS),
            SD = sd(HEALTH_STATUS)) %>%
  as.data.frame()

# Percent health status
survey_2 %>%
  group_by(HEALTH_STATUS) %>%
  summarize(count = n(),
            percent = count / nrow(survey_2))

# Percent self-isolate
survey_2 %>%
  group_by(SELF_ISOLATE) %>%
  summarize(count = n(),
            percent = count / nrow(survey_2))

# Percent key workers
survey_2 %>%
  group_by(KEY_WORKER) %>%
  summarize(count = n(),
            percent = count / nrow(survey_2))

# Tabulated descriptive statistics

# Percent gender
survey_2 %>%
  group_by(GENDER, CISGENDER) %>%
  summarize(count = n(),
            percent = count / nrow(survey_2)) %>%
  arrange(desc(count)) %>%
  as.data.frame()

# Percent ethnicity
survey_2 %>%
  group_by(ETHNIC) %>%
  summarize(count = n(),
            percent = count / nrow(survey_2)) %>%
  arrange(desc(count)) %>%
  as.data.frame()

# Percent country
survey_2 %>%
  group_by(COUNTRY) %>%
  summarize(count = n(),
            percent = count / nrow(survey_2)) %>%
  arrange(desc(count)) %>%
  as.data.frame()

# Percent education
survey_2 %>%
  group_by(EDUCATION) %>%
  summarize(count = n(),
            percent = count / nrow(survey_2)) %>%
  arrange(desc(count)) %>%
  as.data.frame()

# Percent relationship status
survey_2 %>%
  group_by(RELATIONSHIP_STATUS) %>%
  summarize(count = n(),
            percent = count / nrow(survey_2)) %>%
  arrange(desc(count)) %>%
  as.data.frame()

# Percent current living
survey_2 %>%
  group_by(CURRENT_LIVING) %>%
  summarize(count = n(),
            percent = count / nrow(survey_2)) %>%
  arrange(desc(count)) %>%
  as.data.frame()

