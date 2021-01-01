# Dependencies
library(ltm)
library(tidyverse)

# Import
survey <- read_csv("data/covid_sex_tech.csv")

# Select measures
measures <- survey %>%
  select(ID, starts_with(c("SDI_during_", "LONEV3_", "MPSS_", "sociosexuality", "SOI")), -ends_with(c("items", "total")))
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

# SELECT - SOI-R -----------------------------------------------------------

# SOI-R total score
SOIR <- measures %>%
  select("ID", "SOIR" = "Sociosexuality")
SOIR

# Prepare facets
facets <- measures %>%
  select(ID, starts_with("SOI")) %>%
  gather(key = "items", value = "raw_scores", -ID)
facets

# Behavior items 1-3
facets_b <- facets %>%
  filter(items %in% c("SOIR_1", "SOIR_2", "SOIR_3")) %>%
  group_by(ID) %>%
  summarize(
    SOIR_behavior = sum(raw_scores)
  )
facets_b

# Attitude items 4-6
facets_a <- facets %>%
  filter(items %in% c("SOIR_4", "SOIR_5", "SOIR_6")) %>%
  group_by(ID) %>%
  summarize(
    SOIR_attitude = sum(raw_scores)
  )
facets_a

# Desire items 7-9
facets_d <- facets %>%
  filter(items %in% c("SOIR_7", "SOIR_8", "SOIR_9")) %>%
  group_by(ID) %>%
  summarize(
    SOIR_desire = sum(raw_scores)
  )
facets_d

# Combine SOIR scores
SOIR_1 <- SOIR %>%
  left_join(facets_b) %>%
  left_join(facets_a) %>%
  left_join(facets_d)
SOIR_1

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

# SDI pre and during lockdown
survey %>%
  select(SDI_pre_total, SDI_during_total) %>%
  summarize(
   pre_m = mean(SDI_pre_total),
   pre_sd = sd(SDI_pre_total),
   pre_min = min(SDI_pre_total),
   pre_max = max(SDI_pre_total),
   during_m = mean(SDI_during_total),
   during_sd = sd(SDI_during_total),
   during_min = min(SDI_during_total),
   during_max = max(SDI_during_total)
  )

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
  left_join(SOIR_1, by = "ID")
total_measures

# Calculate M and SD
total_measures_1 <- total_measures %>%
  select(-ID) %>%
  gather(key = "scale", value = "score") %>%
  group_by(scale) %>%
  summarize(
    M = mean(score),
    SD = sd(score),
    MIN = min(score),
    MAX = max(score)
  )
total_measures_1

# Save the total scores as CSV
write_csv(total_measures, path = "data/total_measures_1.csv")
