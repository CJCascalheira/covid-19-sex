# Dependencies
library(ltm)
library(tidyverse)

# Import
survey <- read_csv("data/covid_sex_tech.csv")

# Select measures
measures <- survey %>%
  select(ID, starts_with(c("SDI_during_", "LONEV3_", "MPSS_")), -ends_with(c("items", "total")))
measures

# Percent of values missing
missing <- measures %>%
  # Drop the ID column
  select(-ID) %>%
  gather(key = "column", value = "row") %>%
  mutate(
    missing = is.na(row)
  ) %>%
  # Group by the items
  group_by(column) %>%
  # Total number of missing scores per item
  summarize(
    total_missing = sum(missing),
    percent = total_missing / 565
  ) %>%
  filter(percent != 0)
missing

# Highest number of missing values
unique(missing$total_missing)

# Find the variable with 11 missing values
missing %>%
  filter(total_missing == 11)

# Replace missing values at item level with column-wise mean
for (i in 1:length(measures)) {
  # Calculate mean for the column
  m <- measures[,i] %>%
    pull() %>%
    mean(na.rm = TRUE)
  
  # Replace the missing values with the mean
  measures[,i] <- ifelse(is.na(pull(measures[,i])), m, pull(measures[,i]))  
}

# Check for missing values to ensure for loop worked
measures %>%
  gather(key = "column", value = "row") %>%
  filter(is.na(row))

# SCORING - SDI -----------------------------------------------------------

# Total SDI score from 14 items
SDI <- measures %>%
  # Drop extra variables
  select(ID, starts_with("SDI_")) %>%
  gather(key = "items", value = "raw_scores", -ID) %>%
  group_by(ID) %>%
  # Sum raw scores for total sexual desire, including partner and solitary sexuality
  summarize(
    SDI_SCORE = sum(raw_scores)
  )
SDI

# Select all the SDI-2 items
SDI_items <- measures %>%
  select(starts_with("SDI"))
SDI_items

# Cronbach's alpha for SDI-2
cronbach.alpha(SDI_items)

# SCORING - LONE V3 -------------------------------------------------------

# Total LONEV3 score from 20 items
LONEV3 <- measures %>%
  select(ID, starts_with("LONE")) %>%
  # Reverse scores the items based on the article, but note that the values were already reversed scored
  # by LW, so long format for total score
  gather(key = "items", value = "raw_scores", -ID) %>%
  group_by(ID) %>%
  # Sum the raw scores for total loneliness
  summarize(
    LONEV3_SCORE = sum(raw_scores)
  )
LONEV3

# SCORING - MSPSS ---------------------------------------------------------

# Total MSPSS score from 12 items
MSPSS <- measures %>%
  select(ID, starts_with("MPSS")) %>%
  # Long format
  gather(key = "items", value = "raw_scores", -ID) %>%
  group_by(ID) %>%
  # Calculate the total score for MSPSS
  summarize(
    MSPSS_SCORE = sum(raw_scores) / 12
  )
MSPSS

# COMBINE TOTAL SCORES ----------------------------------------------------

# Join the data frames
total_measures <- SDI %>%
  left_join(LONEV3, by = "ID") %>%
  left_join(MSPSS, by = "ID")
total_measures

# Save the total scores as CSV
write_csv(total_measures, path = "data/total_measures.csv")