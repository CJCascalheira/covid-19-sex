# Dependencies
library(tidyverse)

# Import
survey <- read_csv("data/covid_sex_tech.csv")

# CLEAN SEXUAL ACTIVITIES VARIABLES ---------------------------------------

# Select the main variables
sex_act <- survey %>%
  select(starts_with("SA")) %>%
  select(-ends_with("QUAL"))

# Split the values across variables
sex_act_list <- sex_act %>%
  select(-ends_with("ING")) %>%
  map(str_split, ",") %>%
  as_data_frame()

# DESCRIPTIVE ANALYSES ----------------------------------------------------

# Have participants engaged in more sexual fantasizing during lockdown?
within(sex_act, {
  SA_STARTED_FANTASIZING <- recode(as.character(SA_STARTED_FANTASIZING), "0" = "No", "1" = "Yes")
}) %>%
  select(ends_with("ING")) %>%
  group_by(SA_STARTED_FANTASIZING) %>%
  count() %>%
  mutate(percent = n / 565)

# Solo masturbation - but possibly create a FOR loop for this??
sex_act_list %>%
  select(starts_with("SA_SOLO")) %>%
  unlist() %>%
  as_tibble() %>%
  group_by(value) %>%
  count()
