# Dependencies
library(tidyverse)

# Import
survey <- read_csv("data/raw/Data565_new.csv")

# CLEAN DEMOGRAPHIC SECTION -----------------------------------------------

# Add unique ID
survey_1 <- survey %>%
  mutate(ID = row.names(survey)) %>%
  select(ID, everything())
