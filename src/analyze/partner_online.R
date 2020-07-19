# Dependencies
library(tidyverse)

# Import
survey <- read_csv("data/covid_sex_tech.csv")

# CLEAN PARTNER ONLINE VARIABLES ------------------------------------------

# Select primary variables
partner_online <- survey %>%
  select(starts_with(c("PARTNER", "ONLINE"))) %>%
  select(-ends_with(c("QUAL", "STATUS"))) 

# Label all of the values across variables
partner_online_label <- within(partner_online, {
  PARTNER_CONTACT_TECH <- recode(as.character(PARTNER_CONTACT_TECH), "0" = "Used Same", "1" = "Used Same",
                                 "2" = "Used More", "3" = "Used Less")
  ONLINE_CURRENT <- recode(as.character(ONLINE_CURRENT), "0" = "No", "1" = "Yes", "99" = "Unknown")
  ONLINE_BEFORE <- recode(as.character(ONLINE_BEFORE), "0" = "No", "1" = "Yes")
  ONLINE_CHANGE <- recode(as.character(ONLINE_CHANGE), "1" = "Increased", "2" = "Decreased", "3" = "Same")
})

# DESCRIPTIVE ANALYSES ----------------------------------------------------

# Possibly NLP for RELATIONSHIP_TECH and RELATIONSHIP_QUAL