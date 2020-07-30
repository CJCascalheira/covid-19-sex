# Dependencies
library(tidyverse)

# Import
survey <- read_csv("data/covid_sex_tech.csv")
total_measures <- read_csv("data/total_measures.csv")

# CLEAN PARTNER ONLINE VARIABLES ------------------------------------------

# Select primary variables
partner_online <- survey %>%
  select(ID, starts_with(c("PARTNER", "ONLINE")), RELATIONSHIP_STATUS) %>%
  select(-ends_with(c("QUAL"))) 

# Label all of the values across variables
partner_online_label <- within(partner_online, {
  PARTNER_CONTACT_TECH <- recode(as.character(PARTNER_CONTACT_TECH), "0" = "Used Same", "1" = "Used Same",
                                 "2" = "Used More", "3" = "Used Less")
  ONLINE_CURRENT <- recode(as.character(ONLINE_CURRENT), "0" = "No", "1" = "Yes", "99" = "N/A Monogamous")
  ONLINE_BEFORE <- recode(as.character(ONLINE_BEFORE), "0" = "No", "1" = "Yes")
  ONLINE_CHANGE <- recode(as.character(ONLINE_CHANGE), "1" = "Increased", "2" = "Decreased", "3" = "Same")
})
partner_online_label

# DESCRIPTIVE ANALYSES ----------------------------------------------------

# "Have you used technology more to keep in contact with your partner?"
partner_online_label %>%
  group_by(RELATIONSHIP_STATUS, PARTNER_CONTACT_TECH) %>%
  count() %>%
  mutate(percent = n / 565) %>%
  arrange(desc(n), PARTNER_CONTACT_TECH)

# "Do you currently have a profile on a website used for online dating or 
# finding sexual partners? (e.g. Tinder, Grindr, Match.com)"
partner_online_label %>%
  group_by(RELATIONSHIP_STATUS, ONLINE_CURRENT) %>%
  count() %>%
  mutate(percent = n / 565) %>%
  arrange(desc(n), ONLINE_CURRENT)

# "Did you have a profile on a website used for online dating or finding 
# sexual partners before social lockdown?"
partner_online_label %>%
  group_by(RELATIONSHIP_STATUS, ONLINE_BEFORE) %>%
  count() %>%
  mutate(percent = n / 565) %>%
  arrange(desc(n), ONLINE_BEFORE)

# Has your use of these online sites increased, decreased or remained the 
# same during social lockdown?
partner_online_label %>%
  group_by(RELATIONSHIP_STATUS, ONLINE_CHANGE) %>%
  count() %>%
  mutate(percent = n / 565) %>%
  arrange(desc(n), ONLINE_CHANGE)

# NHST - H4a --------------------------------------------------------------

# Prepare data for logistic regressions
partner_nhst <- partner_online %>%
  # Join with total scores of the continuous measures
  left_join(total_measures, by = "ID") %>%
  # Remove participants who did not specify a relationship status
  filter(!is.na(RELATIONSHIP_STATUS)) %>%
  mutate(
    # Combine participants who are single and dating casually
    RELATIONSHIP_STATUS = recode(RELATIONSHIP_STATUS, "Single" = "single_casual", 
                                 "Casual Relationship" = "single_casual"),
    # 0 = decrease; 1 = increase
    ONLINE_CHANGE = recode(ONLINE_CHANGE, `2` = 0)
  ) %>%
  # Drop unnecessary columns
  select(-c(PARTNER_CONTACT_TECH, ONLINE_BEFORE))
partner_nhst

#######
# Logistic regression 
# For more information, see: https://stats.idre.ucla.edu/r/dae/logit-regression/
#######

# Isolate the variables for first logistic regression
p_nhst_current <- partner_nhst %>%
  filter(ONLINE_CURRENT != 99 & RELATIONSHIP_STATUS != "Serious Relationship")
p_nhst_current

# Specify the logistic regression model
current_logit <- glm(ONLINE_CURRENT ~ SDI_SCORE + LONEV3_SCORE + MSPSS_SCORE, 
                     data = p_nhst_current, family = "binomial")
current_logit

# Summary of the model
summary(current_logit)

# NHST - H4b --------------------------------------------------------------

# Isolate the variables for the second logistic regression
p_nhst_change <- partner_nhst %>%
  filter(!is.na(ONLINE_CHANGE) & ONLINE_CHANGE != 3 & RELATIONSHIP_STATUS != "Serious Relationship")
p_nhst_change  

# Specify the logistic regression model
change_logit <- glm(ONLINE_CHANGE ~ SDI_SCORE + LONEV3_SCORE + MSPSS_SCORE, 
                    data = p_nhst_change, family = "binomial")
change_logit

# Summary of the model
summary(change_logit)





# Possibly NLP for 
# - PARTNER_CONTACT_QUAL
# - ONLINE_QUAL
# - RELATIONSHIP_TECH
# - RELATIONSHIP_QUAL