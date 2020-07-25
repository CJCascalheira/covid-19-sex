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

#######
# Basic descriptives
#######

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

#######
# Subset descriptives
#######

# If ever visited, how many times in the past year?
sex_health_label %>%
  filter(SEX_HEALTH_VISIT == "Yes") %>%
  group_by(SEX_HEALTH_VISITCNT) %>%
  count() %>%
  mutate(percent = n / 272)

# Among participants who had been to a clinic and who were screen at least every few years,
# is there concern about STIs during social isolation?
sex_health_label %>%
  filter(SEX_HEALTH_VISIT == "Yes" & SEX_HEALTH_STISCRN != "Never") %>%
  group_by(SEX_HEALTH_STISOCISO) %>%
  count() %>%
  mutate(percent = n / 207)
  
# Among participants who had been to a clinic and who were screen at least every few years,
# is there concern about sexual health during social isolation?
sex_health_label %>%
  filter(SEX_HEALTH_VISIT == "Yes" & SEX_HEALTH_STISCRN != "Never") %>%
  group_by(SEX_HEALTH_HEALTHSOCISO) %>%
  count() %>%
  mutate(percent = n / 207)

# Proportion of articipants who visited and had been screened
207 / 565

# WHY NO CONCERN ----------------------------------------------------------

# Select cases in which a reason for no concern was reported
no_concern <- survey %>%
  select(starts_with("SEX_HEALTH")) %>%
  filter(!is.na(SEX_HEALTH_QUAL)) %>%
  select(SEX_HEALTH_QUAL, SEX_HEALTH_STISOCISO, SEX_HEALTH_HEALTHSOCISO) %>%
  # Remove participants who reported a concern
  filter(SEX_HEALTH_STISOCISO != 1 & SEX_HEALTH_HEALTHSOCISO != 1)
no_concern

# Removed participants with concern?
sum(no_concern$SEX_HEALTH_STISOCISO)
sum(no_concern$SEX_HEALTH_HEALTHSOCISO)

# Remove self-reported variants of "N/A"
no_concern_1 <- no_concern %>%
  # Create a column to track not applicable responses
  mutate(not_app = ifelse(
    str_detect(SEX_HEALTH_QUAL, regex("^*n/a|not app", ignore_case = TRUE)), 
    "yes", NA_character_
  )) %>%
  filter(is.na(not_app)) %>%
  # Rename variable for easy coding
  rename(qual = SEX_HEALTH_QUAL) %>%
  # Remove useless variable
  select(qual)
no_concern_1

# Percent of participants reporting reasons
nrow(no_concern_1) / 565

# Code the sentence
no_concern_2 <- no_concern_1 %>%
  mutate(
    no_sex = ifelse(
      str_detect(qual, regex("^*active|no sex|not having|don't have|not had|haven't had|never had|not engag|nothing|been a while|celiba|virgin|not have a sex|single|absence|any sexual acts", 
                             ignore_case = TRUE)),
      1, NA_character_
    ),
    mngmy = ifelse(
      str_detect(qual, regex("^*long-term|long term|monog|same|one|1|commit|serious|relationship|been with", 
                             ignore_case = TRUE)),
      1, NA_character_
    ),
    tested = ifelse(
      str_detect(qual, regex("^*test|screen|checked|clean|protect|careful|limited interac", ignore_case = TRUE)),
      1, NA_character_
    ),
    health = ifelse(
      str_detect(qual, regex("^*health|excellent", ignore_case = TRUE)),
      1, NA_character_
    ),
    lockdown = ifelse(
      str_detect(qual, regex("^*lockdown|covid|isolation", ignore_case = TRUE)),
      1, NA_character_
    )
  )
no_concern_2  

# Other reasons
no_concern_other <- no_concern_2 %>%
  filter(
    is.na(no_sex),
    is.na(mngmy),
    is.na(tested),
    is.na(health),
    is.na(lockdown)
  )
no_concern_other

# Not concerned in general - did this one separate due to use of phrases in other responses
no_concern_none <- no_concern_other %>%
  mutate(
    not_concrn = ifelse(
      str_detect(qual, regex("^*not concern|not worri|no need|never been conce|to be conc|not too worr", ignore_case = TRUE)),
      1, NA_character_
    )
  )
no_concern_none

# Reasons remaining
other_reasons <- no_concern_none %>%
  filter(is.na(not_concrn))
other_reasons

#######
# Calculate common reasons
#######

# First set of common reasons
no_concern_3 <- no_concern_2 %>%
  # Drop responses
  select(-qual) %>%
  gather(key = reason, value = present) %>%
  filter(!is.na(present)) %>%
  group_by(reason) %>%
  count(present) %>%
  ungroup() %>%
  mutate(percent = n / nrow(no_concern_1)) %>%
  # Drop useless column
  select(-present)
no_concern_3

# All common reasons
no_concern_none %>%
  # Second set of common reasons
  count(not_concrn) %>%
  mutate(percent = n / nrow(no_concern_1)) %>%
  filter(!is.na(not_concrn)) %>%
  # Prepare for binding
  mutate(reason = "not_concrn") %>%
  select(-not_concrn) %>%
  select(reason, everything()) %>%
  # Combine all observations
  rbind(no_concern_3) %>%
  arrange(desc(n))
