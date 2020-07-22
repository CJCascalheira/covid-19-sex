# Dependencies
library(tidyverse)

# Import
survey <- read_csv("data/covid_sex_tech.csv")

# CLEAN PARTNER ONLINE VARIABLES ------------------------------------------

# Select primary variables
partner_online <- survey %>%
  select(starts_with(c("PARTNER", "ONLINE")), RELATIONSHIP_STATUS) %>%
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









# Possibly NLP for 
# - PARTNER_CONTACT_QUAL
# - ONLINE_QUAL
# - RELATIONSHIP_TECH
# - RELATIONSHIP_QUAL