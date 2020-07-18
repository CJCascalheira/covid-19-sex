# Dependencies
library(tidyverse)

# Import
survey <- read_csv("data/raw/Data565_new.csv")

# CLEAN DEMOGRAPHIC SECTION -----------------------------------------------

# Add unique ID
survey_1 <- survey %>%
  mutate(ID = row.names(survey)) %>%
  select(ID, everything())

# Drop variables analyzed previously
survey_2 <- survey_1 %>%
  select(-c(IMPACT_FINANCIAL, FinancialCoding, IMPACT_PHYSICAL, PhysicalCoding, ETHNIC_WHITE_QUAL,
            ETHNIC_MIXED_QUAL, ETHNIC_ASIAN_QUAL, ETHNIC_BLACK_QUAL, ETHNIC_OTHER_QUAL, RELIGION, 
            RELIGION_QUAL, RELATIONSHIP_STATUS_QUAL, CURRENT_LIVING_QUAL)) %>%
  rename("IMPACT_FINANCIAL" = "FinancialCodingSmall", "IMPACT_PHYSICAL" = "PhysicalCodingSmall", 
         "ETHNIC" = "ETHNIC_SELECT")

# Recode nominal data values
survey_3 <- within(survey_2, {
  COUNTRY <- recode(as.character(COUNTRY), "1" = "UK", "2" = "England", "3" = "Scotland", "4" = "Wales",
                    "5" = "Nigeria", "6" = "USA", "7" = "Portugal")
  ETHNIC <- recode(as.character(ETHNIC), "1" = "White British", "2" = "White Irish", "3" = "White Other", 
                   "4" = "White Black Caribbean", "5" = "White Black African", "6" = "White Asian", 
                   "7" = "Other Mixed", "8" = "Indian", "9" = "Pakistani", "10" = "Bangladeshi", 
                   "11" = "Chinese", "12" = "Other Asian", "13" = "African", "14" = "Carribean", 
                   "15" = "Other Black", "16" = "Other Ethnic")
  EDUCATION <- recode(as.character(EDUCATION), "1" = "GCSE", "2" = "A-level", "3" = "Degree", 
                     "4" = "Postgrad", "5" = "PhD")
  GENDER <- recode(as.character(GENDER), "1" = "Woman", "2" = "Man", "3" = "Non-Binary")
  CISGENDER <- recode(as.character(CISGENDER), "0" = "No", "1" = "Yes")
  SEX_ORIENT <- recode(as.character(SEX_ORIENT), "1" = "Heterosexual", "2" = "Mostly Heterosexual", 
                       "3" = "Bisexual", "4" = "Mostly Homosexual", "5" = "Homosexual", "6" = "Pansexual")
  RELATIONSHIP_STATUS <- recode(as.character(RELATIONSHIP_STATUS), "1" = "Single", 
                                "2" = "Casual Relationship", "3" = "Serious Relationship")
  CURRENT_LIVING <- recode(as.character(CURRENT_LIVING), "1" = "Living Alone", "2" = "Living w/ Children", 
                           "3" = "Living w/ Friends", "4" = "Living w/ Other Family", 
                           "5" = "Living w/ Others", "6" = "Living w/ Partner", "7" = "Other")
  SELF_ISOLATE <- recode(as.character(SELF_ISOLATE), "0" = "No", "1" = "Yes")
  KEY_WORKER <- recode(as.character(KEY_WORKER), "0" = "No", "1" = "Yes")
})

# EXPORT DATA -------------------------------------------------------------

write_csv(survey_3, path = "data/covid_sex_tech.csv")
