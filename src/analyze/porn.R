# Dependencies
library(tidyverse)

# Import
survey <- read_csv("data/covid_sex_tech.csv")

# CLEAN PORN VARIABLES ----------------------------------------------------

# Just the porn variables
porn <- survey %>%
  # Select porn variables
  select(ID, GENDER, starts_with("PORN")) %>%
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
porn_other <- porn[c(5, 6)] %>%
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
  count() %>%
  filter(PORN_PARTNER_KNOW != "N/A") %>%
  mutate(percent = n / (87 + 174))

# Gender break down of partner knowing about porn use
porn_label %>%
  filter(PORN_PARTNER_KNOW == "No") %>%
  count(GENDER) %>%
  mutate(percent = n / 87)

# Number of participants who had a partner and watched porn
(87 + 174)

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

# Percent of change and not change
porn_habits %>%
  group_by(PORN_CHANGE_LOCKDOWN) %>%
  count() %>%
  # Remember, one participant dropped due to missing data
  mutate(percent = n / 564)

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

# OPEN-ENDED DATA ANALYSES ------------------------------------------------

# Prepare data
porn_qual <- survey %>%
  select(starts_with("PORN")) %>%
  select(PORN_PARTNER_KNOW, ends_with("QUAL"))
porn_qual

#######
# - PORN_PARTNER_QUAL
# Why does your partner not know about your pornography use?
#######

# Select only the participants whose partners do not know
partner_qual <- porn_qual %>%
  filter(PORN_PARTNER_KNOW == 0) %>%
  # Drop unnecessary variables
  select(PORN_PARTNER_QUAL) %>%
  # Code rationales using regular expressions
  mutate(
    self_con_emo = ifelse(str_detect(PORN_PARTNER_QUAL, 
                                     regex("^*embar|shame", ignore_case = TRUE)), 
                          1, 0),
    come_up = ifelse(str_detect(PORN_PARTNER_QUAL,
                                regex("^*come up|comes up|came up|never ask|speak about|discus|talk|ask|convers|haven't told", ignore_case = TRUE)),
                     1, 0),
    comfort = ifelse(str_detect(PORN_PARTNER_QUAL,
                                regex("^*comfort", ignore_case = TRUE)),
                     1, 0),
    private = ifelse(str_detect(PORN_PARTNER_QUAL,
                                regex("^*keep|private|hide|share|want her to", ignore_case = TRUE)),
                     1, 0),
    unsure = ifelse(str_detect(PORN_PARTNER_QUAL,
                               regex("^*unsure|dont know|don't know", ignore_case = TRUE)),
                    1, 0),
    no_reason = ifelse(str_detect(PORN_PARTNER_QUAL,
                                  regex("^*reason", ignore_case = TRUE)),
                       1, 0),
    no_need = ifelse(str_detect(PORN_PARTNER_QUAL,
                                regex("^*unnec|need to", ignore_case = TRUE)),
                     1, 0),
    prcvd_disprv = ifelse(str_detect(PORN_PARTNER_QUAL,
                                     regex("^*conserv|approv|finds it|interested|happy about|argument|hurt in the past|like how much", ignore_case = TRUE)),
                          1, 0),
    sex_drive = ifelse(str_detect(PORN_PARTNER_QUAL,
                                  regex("^*sex drive|kicks|not in the mood", ignore_case = TRUE)),
                       1, 0)
  )
partner_qual

# Determine which reasons still exist, an other category
partner_qual %>%
  filter(
    self_con_emo != 1 &
    come_up != 1 &
    comfort != 1 &
    private != 1 &
    unsure != 1 &
    no_reason != 1 &
    no_need != 1 &
    prcvd_disprv != 1 &
    sex_drive != 1
  )

# Percentage of other
4 / nrow(partner_qual)

# Gather code occurrences into long format
partner_qual_long <- partner_qual %>%
  # Drop unnecessary column
  select(-PORN_PARTNER_QUAL) %>%
  gather(key = "code", value = "present") %>%
  filter(present != 0)
partner_qual_long

# Number of participants with more than one code
nrow(partner_qual_long) - nrow(partner_qual)

# Organize by most common categories
partner_qual_long %>%
  group_by(code) %>%
  count() %>%
  mutate(percent = (n / nrow(partner_qual)) * 100) %>%
  arrange(desc(n))

# Note: the categories no_reason and unsure were combined

#######
# PORN_CHANGE_QUAL
# How has the amount or pornography viewed during social lockdown changed?
#######

# Prepare the data
change_qual <- survey %>%
  select(PORN_CHANGE_LOCKDOWN, PORN_CHANGE_QUAL) %>%
  # Remove participants who reported no change (i.e., 2)
  filter(PORN_CHANGE_LOCKDOWN != 2) %>%
  # Remove participants who did not describe how their porn use changed
  filter(!is.na(PORN_CHANGE_QUAL))
change_qual

# Number of participants reporting a reason
nrow(change_qual) / 202

# 1 = increase
change_inc <- change_qual %>%
  filter(PORN_CHANGE_LOCKDOWN == 1) %>%
  # Conventional content analysis
  mutate(
    boredom = ifelse(str_detect(PORN_CHANGE_QUAL,
                                regex("^*bore|nothing else|nothing bett|to do", ignore_case = TRUE)),
                     1, 0),
    time = ifelse(str_detect(PORN_CHANGE_QUAL,
                             regex("^*free time|more time", ignore_case = TRUE)),
                  1, 0),
    stress = ifelse(str_detect(PORN_CHANGE_QUAL,
                               regex("^*stress|feel bett|relie|frustra", ignore_case = TRUE)),
                    1, 0),
    alone = ifelse(str_detect(PORN_CHANGE_QUAL,
                              regex("^*lone|see.+my|got my|have my|sex anymore|deprived|died|live with my|less intim|lack of any s", ignore_case = TRUE)),
                   1, 0),
    rate = ifelse(str_detect(PORN_CHANGE_QUAL,
                             regex("^*daily|once|increase|a week|a month|other day|watch+.+more|more incli|every now", ignore_case = TRUE)),
                  1, 0),
    partner_fct = ifelse(str_detect(PORN_CHANGE_QUAL,
                                    regex("^*watch togeth|with her|long distance|into my rel|wanting my part", ignore_case = TRUE)),
                         1, 0)
  )
change_inc

# Identify which responses remain
change_inc_other <- change_inc %>%
  filter(
    boredom != 1 &
    time != 1 &
    stress != 1 &
    alone != 1 &
    rate != 1 &
    partner_fct != 1
  )
change_inc_other

# % in other category for participants reporting an increase 
nrow(change_inc_other) / nrow(change_inc)

# Calculate counts
change_inc %>%
  select(-starts_with("PORN")) %>%
  # Long format
  gather(key = "code", value = "present") %>%
  # Filter out non-occurrences 
  filter(present != 0) %>%
  group_by(code) %>%
  count() %>%
  mutate(percent = (n / nrow(change_inc)) * 100) %>%
  arrange(desc(n))

# Example of partner factor
change_inc %>%
  filter(partner_fct == 1) %>%
  View()

# 3 = decrease
change_dec <- change_qual %>%
  filter(PORN_CHANGE_LOCKDOWN == 3) %>%
  # Code the qualitative data using conventional content analysis principles
  mutate(
    not_alone = ifelse(str_detect(PORN_CHANGE_QUAL,
                                  regex("^*alone|with me|more time with|no time|around|living with|more people|kid|child|parent|is home|all.+time|with.+partner|my.+home|partner.+home|time.+myself|dont watch|my boyfr", ignore_case = TRUE)),
                       1, 0),
    sex_drive = ifelse(str_detect(PORN_CHANGE_QUAL,
                                  regex("^*sex drive|urge|mood|too tired|feel like|sex.+desire|sex.+app", ignore_case = TRUE)),
                       1, 0),
    rate = ifelse(str_detect(PORN_CHANGE_QUAL,
                        regex("^*less need|not watch|hardly|occas|haven't watch|less porn|watch less", ignore_case = TRUE)),
             1, 0),
    interest = ifelse(str_detect(PORN_CHANGE_QUAL,
                                 regex("^*interest", ignore_case = TRUE)),
                      1, 0),
    # Language of addiction
    addict = ifelse(str_detect(PORN_CHANGE_QUAL,
                               regex("^*control|addic|giv.+up|cut back|need to use", ignore_case = TRUE)),
                    1, 0),
    unsure = ifelse(str_detect(PORN_CHANGE_QUAL,
                               regex("^*reason|don't know|know.+why|not.+sure", ignore_case = TRUE)),
                    1, 0),
    # Busy or prioritize other activities
    busy = ifelse(str_detect(PORN_CHANGE_QUAL,
                             regex("bus|other things|prefer", ignore_case = TRUE)),
                  1, 0)
  )
change_dec

# Identify which responses remain
change_dec %>%
  filter(
    not_alone != 1 &
    sex_drive != 1 &
    rate != 1 &
    interest != 1 &
    addict != 1 &
    unsure != 1 &
    busy != 1
  )

# % in other category for participants reporting a decrease
3 / nrow(change_dec)

# Calculate frequencies
change_dec %>%
  # Drop unnecessary columns
  select(-starts_with("PORN")) %>%
  gather(key = "code", value = "present") %>%
  # Remove non-occurrences 
  filter(present != 0) %>%
  group_by(code) %>%
  count() %>%
  mutate(percent = (n / nrow(change_dec)) * 100) %>%
  arrange(desc(n))
