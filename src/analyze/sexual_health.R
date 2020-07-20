# Dependencies
library(tidyverse)

# Import
survey <- read_csv("data/covid_sex_tech.csv")

# CLEAN SEXUAL HEALTH VARIABLES -------------------------------------------

# Select main variables
sex_health <- survey %>%
  select(starts_with("SEX_HEALTH")) %>%
  select(-ends_with("QUAL"))

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

# DESCRIPTIVE ANALYSES ----------------------------------------------------
