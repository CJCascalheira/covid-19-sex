# Dependencies
library(tidyverse)

# Import
survey <- read_csv("data/covid_sex_tech.csv")

# CLEAN SEXUAL HEALTH VARIABLES -------------------------------------------

# Select main variables
sex_health <- survey %>%
  select(starts_with("SEX_HEALTH")) %>%
  select(-ends_with("QUAL"))
sex_health

# Label values across variables
sex_health_label <- within(sex_health, {
  SEX_HEALTH_VISIT <- recode(as.character(SEX_HEALTH_VISIT), "0" = "No", "1" = "Yes")
  SEX_HEALTH_STISCRN <- recode(as.character(SEX_HEALTH_STISCRN), "0" = "Never", "1" = "Monthly",
                               "2" = "Every 3 months", "3" = "Every 6 months", "4" = "Yearly",
                               "5" = "Every few years")
  SEX_HEALTH_STIGET <- recode(as.character(SEX_HEALTH_STIGET), "0" = "No", "1" = "Yes", "2" = "Unsure")
  SEX_HEALTH_STISOCISO <- recode(as.character(SEX_HEALTH_STISOCISO), "0" = "No", "1" = "Yes")
  SEX_HEALTH_HEALTHSOCISO <- recode(as.character(SEX_HEALTH_HEALTHSOCISO), "0" = "No", "1" = "Yes")
})
sex_health_label

# DESCRIPTIVE ANALYSES ----------------------------------------------------

# Have you ever visited a sexual health clinic or contraception/family planning clinic?
sex_health_label %>%
  select(SEX_HEALTH_VISIT) %>%
  group_by(SEX_HEALTH_VISIT) %>%
  count() %>%
  mutate(percent = n / 565)

# How many times in the past year have you attended a sexual health clinic or 
# contraception/family planning clinic?
sex_health_label %>%
  select(SEX_HEALTH_VISITCNT) %>%
  group_by(SEX_HEALTH_VISITCNT) %>%
  count() %>%
  mutate(percent = n / 565)

# How often are you screened for STIs (including HIV)?
sex_health_label %>%
  select(SEX_HEALTH_STISCRN) %>%
  group_by(SEX_HEALTH_STISCRN) %>%
  count() %>%
  mutate(percent = n / 565)

# In the last 12 months, have you been diagnosed with any STIs?
sex_health_label %>%
  select(SEX_HEALTH_STIGET) %>%
  group_by(SEX_HEALTH_STIGET) %>%
  count() %>%
  mutate(percent = n / 565)

# Are you concerned that you may have an STI while in social isolation?
sex_health_label %>%
  select(SEX_HEALTH_STISOCISO) %>%
  group_by(SEX_HEALTH_STISOCISO) %>%
  count() %>%
  mutate(percent = n / 565)

# Are you concerned about your sexual health while in social isolation?
sex_health_label %>%
  select(SEX_HEALTH_HEALTHSOCISO) %>%
  group_by(SEX_HEALTH_HEALTHSOCISO) %>%
  count() %>%
  mutate(percent = n / 565)







# Possibly NLP for
# - SEX_HEALTH_QUAL