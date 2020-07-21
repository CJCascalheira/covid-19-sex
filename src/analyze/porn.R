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
  PORN_PARTNER_KNOW <- recode(as.character(PORN_PARTNER_KNOW), "0" = "No", "1" = "Yes", "99" = "N/A")
})

# Break up PORN_CHANGE_WHICH by comma
porn_which_break <- porn_label %>%
  select(ends_with("WHICH")) %>%
  str_split(",") %>%
  noquote() %>%
  unlist(use.names = FALSE) %>%
  as_tibble()
porn_which_break

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
porn_which_break_1 <- porn_which_break %>%
  group_by(selection) %>%
  count() %>%
  arrange(desc(n)) %>%
  mutate(percent = n / 565) %>%
  ungroup()
porn_which_break_1

####### 
# What does Other (please specify) mean?
#######

# Identify nots, don'ts, nevers
porn_other <- porn[c(3, 4)] %>%
  filter(PORN_CHANGE_WHICH == "Other (please specify)") %>%
  group_by(PORN_CHANGE_OTHER) %>%
  count() %>%
  arrange(desc(n)) %>%
  mutate(not_watching = if_else(
      str_detect(PORN_CHANGE_OTHER, regex("^*don*|no|didn't|never|N/a", ignore_case = TRUE)),
          "yes", NA_character_
  ))
porn_other

# How many do not watch? 
porn_which_break_2 <- porn_other %>%
  filter(not_watching == "yes") %>%
  ungroup() %>%
  # Manual add NA and na from the list
  summarize(n = sum(n) + 8 + 3) %>%
  mutate(selection = "no_never_dont", percent = n / 565) %>%
  select(selection, everything()) %>%
  # Add value to tibble
  rbind(porn_which_break_1)
porn_which_break_2

# What do the other participants mean by "Other"?
porn_which_break_3 <- porn_other %>%
  filter(is.na(not_watching)) %>%
  select(-not_watching) %>%
  mutate(stopped = ifelse(
    str_detect(PORN_CHANGE_OTHER, regex("^*stop", ignore_case = TRUE)),
    "yes", NA_character_
  )) %>%
  mutate(same = ifelse(
    str_detect(PORN_CHANGE_OTHER, regex("^*same|haven't|hasn't|unch", ignore_case = TRUE)),
    "yes", NA_character_
  )) %>%
  # Find the positive cases
  filter(stopped == "yes" || same == "yes") %>%
  ungroup() %>%
  group_by(stopped, same) %>%
  summarize(n = sum(n)) %>%
  ungroup() %>%
  mutate(selection = c("stopped", "same")) %>%
  select(selection, n) %>%
  # Manual add the two remaining categories
  rbind(c("rarely", 1)) %>%
  rbind(c("live cam", 1)) %>%
  # Coerce to integer manually
  mutate(n = c(2, 12, 1, 1)) %>%
  # Add percent
  mutate(percent = n / 565) %>%
  rbind(porn_which_break_2) %>%
  # Remove other
  filter(!(selection == "Other please specify"))
porn_which_break_3

# Combine no_never_dont and NA
porn_which_break_4 <- within(porn_which_break_3, {
  selection <- recode(selection, "NA" = "no_never_dont")
}) %>%
  group_by(selection) %>%
  summarize(n = sum(n)) %>%
  # Add percent again
  mutate(percent = n / 565) %>%
  arrange(desc(n))
porn_which_break_4

# New subcategories
porn_which_break_5 <- porn_which_break_4 %>%
  mutate(category = c("consistent", rep("solitary", 3), "directional", "novel",
                      rep("directional", 3), "novel", "consistent", rep("novel", 3), 
                      "directional")) %>%
  select(category, selection, everything()) %>%
  arrange(category, desc(n))
porn_which_break_5

# Aggregate by superordinate category
porn_which_break_5 %>%
  group_by(category) %>%
  summarize(
    n = sum(n),
    percent = sum(percent)
  ) %>%
  arrange(desc(n))

#######
# Understanding the labeled porn variables
#######

# Remember, PORN_CHANGE_BEFORE can be used as a continuous variable
porn_label %>%
  select(PORN_CHANGE_BEFORE) %>%
  group_by(PORN_CHANGE_BEFORE) %>%
  count()

# Frequency distribution of porn viewing after lockdown
porn_label %>%
  select(PORN_CHANGE_LOCKDOWN) %>%
  group_by(PORN_CHANGE_LOCKDOWN) %>%
  count() %>%
  # Unchanged porn consumption behavior, which is different from the free-response question
  mutate(percent = n / 565)

# What proportion of partners know about porn use?
porn_label %>%
  select(PORN_PARTNER_KNOW) %>%
  group_by(PORN_PARTNER_KNOW) %>%
  count()

# NHST --------------------------------------------------------------------

#######
# Logistic regression 
# For more information, see: https://stats.idre.ucla.edu/r/dae/logit-regression/
#######

#######
# Pre-lockdown rates of pornography consumption will predict whether a 
# participant changed their viewing habits. 
#######

# Prepare variables
porn_habits <- within(porn, {
  PORN_CHANGE_LOCKDOWN <- recode(as.character(PORN_CHANGE_LOCKDOWN), "1" = "1", 
                                 "2" = "0", "3" = "1")
  }) %>%
  # Transform to integer
  mutate(PORN_CHANGE_LOCKDOWN = as.integer(PORN_CHANGE_LOCKDOWN)) %>%
  # Remove one missing value
  filter(!is.na(PORN_CHANGE_LOCKDOWN))
porn_habits

# Mean and standard deviation
porn_habits %>%
  select(PORN_CHANGE_BEFORE) %>%
  summarize(
    mean = mean(PORN_CHANGE_BEFORE),
    sd = sd(PORN_CHANGE_BEFORE)
  )

# Specify the logistic regression equation with generalized linear model
porn_habits_logistic <- glm(PORN_CHANGE_LOCKDOWN ~ PORN_CHANGE_BEFORE, 
                            data = porn_habits, family = "binomial")
porn_habits_logistic

# Summary of the model
summary(porn_habits_logistic)

# Wald's chi-square statistic = ((beta - 0) / SE_beta)^2
# Zero omitted for clarity
# Note: same as z value ^ 2
(porn_habits_logistic$coefficients[1] / 0.20642)^2
(porn_habits_logistic$coefficients[2] / 0.05871)^2

# Exponentiate the coefficients to interpret as odds-ratios
exp(
  coef(porn_habits_logistic)
)

# Model fit - chi-square
with(porn_habits_logistic, null.deviance - deviance)

# Model fit - degrees of freedom
with(porn_habits_logistic, df.null - df.residual)

# Model fit - p-value
with(porn_habits_logistic, 
     pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))

#######
# Pre-lockdown rates of pornography consumption will predict increases and decreases.
#######

# Prepare variables
porn_habits_1 <- within(porn, {
  PORN_CHANGE_LOCKDOWN <- recode(as.character(PORN_CHANGE_LOCKDOWN), "1" = "1", 
                                 "2" = "Unchanged", "3" = "0")
}) %>%
  filter(PORN_CHANGE_LOCKDOWN != "Unchange") %>%
  # Transform to integer 
  mutate(PORN_CHANGE_LOCKDOWN = as.integer(PORN_CHANGE_LOCKDOWN)) %>%
  # Remove one missing value
  filter(!is.na(PORN_CHANGE_LOCKDOWN))
porn_habits_1

# Specify the logistic regression model
porn_habits_logistic_2 <- glm(PORN_CHANGE_LOCKDOWN ~ PORN_CHANGE_BEFORE, 
                              data = porn_habits_1, family = "binomial")
porn_habits_logistic_2

# Summarize the model
summary(porn_habits_logistic_2)

#######
# ANOVA
#######

# Prepare data for ANOVA

#######
# Is there a difference in the pre-lockdown rate of porn consumption by relationship status?
#######

#######
# Is there a difference in the pre-lockdown rate of porn consumption by living arrangement?
#######