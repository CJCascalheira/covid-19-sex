# Dependencies
library(tidyverse)

# Import
survey <- read_csv("data/covid_sex_tech.csv")

# CLEAN PARTNER ONLINE VARIABLES ------------------------------------------

# Select primary variables
partner_online <- survey %>%
  select(starts_with(c("PARTNER", "ONLINE", "RELATIONSHIP"))) %>%
  select(-ends_with(c("QUAL", "STATUS")))

# DESCRIPTIVE ANALYSES ----------------------------------------------------
