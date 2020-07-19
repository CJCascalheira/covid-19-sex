# Dependencies
library(tidyverse)

# Import
survey <- read_csv("data/covid_sex_tech.csv")

# CLEAN PORN VARIABLES ----------------------------------------------------

# Just the porn variables
porn <- survey %>%
  # Select porn variables
  select(starts_with("PORN")) %>%
  # Remove open text variables
  select(-ends_with("QUAL"))

# Recode the values to match labels in master data set 
porn_label <- within(porn, {
  PORN_CHANGE_BEFORE <- recode(as.character(PORN_CHANGE_BEFORE), "1" = "Never", 
                               "2" = "Less than once a month", "3" = "Less than once a week",
                               "4" = "Once a week", "5" = "Every couple of days", "6" = "Daily",
                               "7" = "Several times a day")
  PORN_CHANGE_LOCKDOWN <- recode(as.character(PORN_CHANGE_LOCKDOWN), "1" = "Increased", "2" = "Unchanged",
                                 "3" = "Decreased")
  PORN_PARTNER_KNOW <- recode(as.character(PORN_PARTNER_KNOW), "0" = "No", "1" = "Yes", "99" = "Unknown")
})

# Break up PORN_CHANGE_WHICH by comma
porn_which_break <- porn_label %>%
  select(ends_with("WHICH")) %>%
  str_split(",") %>%
  noquote() %>%
  unlist(use.names = FALSE) %>%
  as_tibble()

# Clean up the values, removing whitespace and \" and \n
porn_break_vector <- str_replace(porn_which_break$value, "[\"]", "")

# Now as vector, make final sweeping clean
(porn_break_vector_1 <- porn_break_vector %>%
  str_replace("[\"]", "") %>%
  str_replace("[\n]", "") %>%
  str_trim() %>%
  str_replace("[\"]", "") %>%
  str_replace("[c(]", "") %>%
  str_replace("[(]", "") %>%
  str_replace("[)]", "")  
)

# Add clean vector to data frame
porn_which_break$selection <- porn_break_vector_1

# DESCRIPTIVE ANALYSES ----------------------------------------------------

# Describe PORN_CHANGE_WHICH
porn_which_break %>%
  group_by(selection) %>%
  count() %>%
  arrange(desc(n)) %>%
  mutate(percent = n / 565)
