# Dependencies
library(car)
library(tidyverse)
library(chisq.posthoc.test)

# Import
survey <- read_csv("data/covid_sex_tech.csv")

# CLEAN SEXUAL ACTIVITIES VARIABLES ---------------------------------------

# Select the main variables
sex_act <- survey %>%
  select(ID, RELATIONSHIP_STATUS, CURRENT_LIVING, GENDER, SEX_ORIENT, starts_with("SA")) %>%
  select(-ends_with("QUAL"))

# Split the values across variables
sex_act_list <- sex_act %>%
  select(-ends_with("ING"), -RELATIONSHIP_STATUS, -GENDER, -SEX_ORIENT) %>%
  map(str_split, ",") %>%
  as_tibble()
sex_act_list

# Drop ID column
sex_act_list <- sex_act_list[-1]
sex_act_list

# DESCRIPTIVE ANALYSES ----------------------------------------------------

# Have participants engaged in more sexual fantasizing during lockdown?
within(sex_act, {
  SA_STARTED_FANTASIZING <- recode(as.character(SA_STARTED_FANTASIZING), "0" = "No", "1" = "Yes")
}) %>%
  select(ends_with("ING")) %>%
  group_by(SA_STARTED_FANTASIZING) %>%
  count() %>%
  mutate(percent = n / 565)

####### GENDER

# Gender of started fantasizing
fantasy_gender <- sex_act %>%
  filter(SA_STARTED_FANTASIZING == 1) %>%
  count(GENDER) %>%
  mutate(percent = n / 194, total = sum(n)) %>%
  filter(GENDER %in% c("Man", "Woman")) %>%
  arrange(desc(n))
fantasy_gender

# Fisher exact probability test
prop.test(fantasy_gender$n, fantasy_gender$total, alternative = "two.sided", correct = FALSE)

#######
# Frequencies of sexual activities
#######

# Prepare the list
sa_list <- list(sex_act_list[1])

# Counts and percents for all sexual activities (SA)
for (i in 1:length(sex_act_list)) {
  
  # Calculate count and percent for each SA
  a <- sex_act_list[,i] %>%
    unlist() %>%
    as_tibble() %>%
    group_by(value) %>%
    count() %>%
    mutate(percent = n / 565)
  
  # Save results to list
  sa_list[i] <- list(a)
}

# Name the tibbles across each list
names(sa_list) <- names(sex_act_list)

# Ignore NA because NA is a product of the operation
sa_list

# SEXUAL ACTIVITY TABLE -------------------------------------------------------

# Create temporary tibble
b <- as_tibble(sa_list)

# Extract the first dataframe from tibble 
c <- b[1][[1]] %>%
  ungroup()

# Number of iterations left
length(sa_list) - 1

# Extract the remaining data using a loop
for (i in 2:14) {
  
  # Repeat the procedure for each, saving to temporary dataframe
  c <- b[i][[1]] %>%
    ungroup() %>%
    rbind(c)
}

# Get the names of sexual activities
sa_names_rev <- names(sex_act_list)

# Prepare new dataframe
sa_names_rev_1 <- tibble(sex_activity = rep(sa_names_rev[1], 5))

# Replicate the names four times, in order, saving as new vector
for (i in 2:length(sa_names_rev)) {
  sa_names_rev_1 <- tibble(sex_activity = rep(sa_names_rev[i], 5)) %>%
    rbind(sa_names_rev_1)
}

# Create dataframe to use for chi-square analysis
sa_chi_table <- c %>% 
  # Add sexual activity names as new variable
  cbind(sa_names_rev_1) %>%
  as.data.frame() %>%
  select(-percent) %>%
  select(sex_activity, everything()) %>%
  spread(key = value, value = n)

# Fix names of table
names(sa_chi_table) <- c("sex_activity", "before", "during", "less", "more", "none")

# Add row names
rownames(sa_chi_table) <- sa_chi_table[,1]

# Drop sex_activity to create pure frequency table
sa_chi_table_1 <- sa_chi_table[,-c(1, 6)]
sa_chi_table_1

# Order by most frequent activity before
sa_chi_table_1 %>%
  rownames_to_column() %>%
  as_tibble() %>%
  arrange(desc(before))

# INCREASES IN SEXUAL ACTIVITIES ------------------------------------------

#######
# Current living situation and sexual fantasizing
#######

# Prepare living status for chi-square
sex_act_living <- sex_act %>%
  select(CURRENT_LIVING, SA_STARTED_FANTASIZING) %>%
  mutate(CURRENT_LIVING = recode(CURRENT_LIVING, 
                                 "Living w/ Children" = "Children or Family",
                                 "Living w/ Other Family" = "Children or Family",
                                 "Living w/ Others" = "Friends or Others",
                                 "Living w/ Friends" = "Friends or Others")) %>%
  table()
sex_act_living

# Execute chi-square analysis
sal_chisq <- chisq.test(sex_act_living)
sal_chisq

# Effect size - Cramer's V
sqrt(
  sal_chisq$statistic[[1]] / (565 * (2 - 1))
)

# Post-hoc pairwise comparisons
chisq.posthoc.test(sex_act_living, method = "bonferroni")

# Observed frequencies of the table
prop.table(sex_act_living)

# Observed and expected counts
sal_chisq$observed
sal_chisq$expected

# Cell chi-square 
for (i in 1:5) {
  x <- ((sal_chisq$observed[i,][2] - sal_chisq$expected[i,][2])^2) / sal_chisq$expected[i,][2]
  print(x)
  
  y <- ((sal_chisq$observed[i,][1] - sal_chisq$expected[i,][1])^2) / sal_chisq$expected[i,][1]
  print(y)
}

# Relative contribution
for (i in 1:5) {
  x <- (((sal_chisq$observed[i,][2] - sal_chisq$expected[i,][2])^2) / (sal_chisq$expected[i,][2] * sal_chisq$statistic)) * 100
  print(x)
  
  y <- (((sal_chisq$observed[i,][1] - sal_chisq$expected[i,][1])^2) / (sal_chisq$expected[i,][1] * sal_chisq$statistic)) * 100
  print(y)
}

#######
# Relationship status and sexual fantasizing
#######

# Prepare relationship status for chi-square
sex_act_relation <- sex_act %>%
  select(RELATIONSHIP_STATUS, SA_STARTED_FANTASIZING) %>%
  table()
sex_act_relation

# Execute chi-square analysis
sar_chisq <- chisq.test(sex_act_relation)
sar_chisq

# Effect size - Cramer's V
# Remember, three participants removed due to NA relationship status
sqrt(
  sar_chisq$statistic[[1]] / (562 * (2 - 1))
)

# Post-hoc pairwise comparisons
chisq.posthoc.test(sex_act_relation, method = "bonferroni")

# Observed frequencies of the table
prop.table(sex_act_relation)

#######
# Increases in solo sexual practices
#######

# Prepare the data frame
sex_act_1 <- sex_act %>%
  select(-SA_STARTED_FANTASIZING) %>%
  mutate(increased = rep(0, 565)) %>%
  select(ID, increased, everything())
sex_act_1

# Overall people endorsing solo sexual activity
sum(!is.na(sex_act_1$SA_SOLOMASTURBATION))
sum(!is.na(sex_act_1$SA_SEXTOYSSOLO))
sum(!is.na(sex_act_1$SA_WATCHPORNSOLO))

# Recode all NA values
sex_act_2 <- sex_act_1 %>%
  # Remove partnered sexual practices
  select(ID, RELATIONSHIP_STATUS, CURRENT_LIVING, increased,
         SA_SOLOMASTURBATION, SA_SEXTOYSSOLO, SA_WATCHPORNSOLO) %>%
  mutate_at(vars(SA_SOLOMASTURBATION, SA_SEXTOYSSOLO, SA_WATCHPORNSOLO),
            function(x) ifelse(
              is.na(x), "none", x
            )
  )
sex_act_2

# Participants who reported a solo sexual activity
sex_act_solo <- sex_act_2 %>%
  gather(key = variable, value = engaged, 
         -c(ID, RELATIONSHIP_STATUS, CURRENT_LIVING, increased)) %>%
  # Tally the increases
  mutate(increased = ifelse(
    str_detect(engaged, regex("^*more", ignore_case = TRUE)), 
    increased + 1, increased + 0
  )) 
sex_act_solo

# Increases in all three solo sexual activities
sex_act_3 <- sex_act_solo %>%
  group_by(ID) %>%
  count(increased) %>%
  filter(increased == 1) %>%
  ungroup()
sex_act_3

# Percent of participants reporting at least one solo sexual practice
nrow(sex_act_3) / 565

# Percent reporting two solo sexual practices
sex_act_3 %>%
  filter(n == 2) %>%
  nrow() / 565

# Percent reporting all three sexual practices
sex_act_3 %>%
  filter(n == 3) %>%
  nrow() / 565

####### GENDER 

# Gender break down of solo sexual activities
solo_sex_gender <- semi_join(sex_act, sex_act_3, by = "ID") %>%
  count(GENDER) %>%
  mutate(percent = n / 172, total = sum(n)) %>%
  arrange(desc(n))
solo_sex_gender

# Fisher exact probability test
prop.test(solo_sex_gender[1:2, ]$n, solo_sex_gender[1:2, ]$total, alternative = "two.sided", correct = FALSE)

#######

# Percent of solo masturbation increase
sex_act_solo %>%
  filter(variable == "SA_SOLOMASTURBATION" & increased == 1) %>%
  nrow() / 565

# Percent of solo sex toy increase
sex_act_solo %>%
  filter(variable == "SA_SEXTOYSSOLO" & increased == 1) %>%
  nrow() / 565

# Percent of watching porn alone increase
sex_act_solo %>%
  filter(variable == "SA_WATCHPORNSOLO" & increased == 1) %>%
  nrow() / 565

#######
# Prepare data for test of significant increases in sexual activity over all
#######

# Prepare the data frame
sex_act_1 <- sex_act %>%
  select(-SA_STARTED_FANTASIZING) %>%
  mutate(increased = rep(0, 565)) %>%
  select(ID, increased, everything())
sex_act_1

# Recode all NA values
sex_act_2 <- sex_act_1 %>%
  mutate_at(vars(SA_SOLOMASTURBATION, SA_MUTUALMASTURBATION,
                 SA_INTERCOURSEWITHPARTNER, SA_INTERCOURSEWITHNONPARTNER,
                 SA_SEXTOYSSOLO, SA_SEXTOYSWITHPARTNER, SA_WATCHPORNSOLO,
                 SA_WATCHPORNWITHPARTNER, SA_SEXTEDWITHPARTNER,
                 SA_SEXTEDWITHNONPARTNER, SA_SENTNUDESTOPARTNER,
                 SA_SENTNUDESTONONPARTNER, SA_ROLEPLAYPARTNER,
                 SA_ROLEPLAYNONPARTNER),
    function(x) ifelse(
      is.na(x), "none", x
    )
  )
sex_act_2

# Participants who reported an increase in sexual activities
sex_act_3 <- sex_act_2 %>%
  gather(key = variable, value = engaged, 
         -c(ID, RELATIONSHIP_STATUS, CURRENT_LIVING, increased)) %>%
# Tally the increases
  mutate(increased = ifelse(
    str_detect(engaged, regex("^*more", ignore_case = TRUE)), 
    increased + 1, increased + 0
  )) %>%
  group_by(ID) %>%
  count(increased) %>%
  filter(increased == 1) %>%
  ungroup()
sex_act_3

# Percent of participants reporting increases in sexual activities
nrow(sex_act_3) / 565

# Reconstruct the data frame w/ participants who reported no increases plus 
# participants who reported an increase greater than 0
sex_act_4 <- sex_act_2 %>%
  filter(!(ID %in% sex_act_3$ID)) %>%
  select(ID, increased) %>%
  mutate(n = rep(0, length(ID))) %>%
  rbind(sex_act_3) %>%
  full_join(
    sex_act_2 %>%
      select(ID, RELATIONSHIP_STATUS, CURRENT_LIVING, GENDER, SEX_ORIENT)
  ) %>%
  select(-increased)
sex_act_4

# MASTURBATION VS. INTERCOURSE --------------------------------------------

# Select the variables of interest
mast_inter <- sex_act_2 %>%
  select(SA_SOLOMASTURBATION, SA_INTERCOURSEWITHPARTNER) %>%
  mutate(
    increased_intercourse = rep(0, 565),
    increased_solo_mast = rep(0, 565)
  ) %>%
  # Calculate increased sexual activity
  mutate(
    increased_intercourse = if_else(str_detect(SA_INTERCOURSEWITHPARTNER, 
                                               regex("more", ignore_case = TRUE)), 1, 0),
    increased_solo_mast = if_else(str_detect(SA_SOLOMASTURBATION,
                                             regex("more", ignore_case = TRUE)), 1, 0)
  ) %>%
  select(starts_with("incre"))
mast_inter

# See contingency table
table(mast_inter)

# Chi-square test of independence 
chisq.test(table(mast_inter))

# COMPARE MEANS - RELATIONSHIP STATUS -------------------------------------

# Remove NA values
sex_act_4_rs <- sex_act_4 %>%
  filter(!is.na(RELATIONSHIP_STATUS)) %>%
  mutate(RELATIONSHIP_STATUS = recode(RELATIONSHIP_STATUS, "Single" = "Single or Casual",
                           "Casual Relationship" = "Single or Casual"))
sex_act_4_rs

# Describe increases in activities by relationship
sex_act_4_rs %>%
  group_by(RELATIONSHIP_STATUS) %>%
  summarize(
    M = mean(n),
    SD = sd(n)
  )

#######
# Assumptions
#######

# Are the distributions normal or at least symmetrical?
ggplot(sex_act_4_rs, aes(x = n)) +
  geom_histogram() +
  facet_grid(~ RELATIONSHIP_STATUS)

# Sample is not approximately equal in size for each group, but is greater than 25 per group
sex_act_4_rs %>%
  group_by(RELATIONSHIP_STATUS) %>%
  count()

# KS test
sa4rs_serious <- sex_act_4_rs %>%
  filter(RELATIONSHIP_STATUS == "Serious Relationship")

sa4rs_casual <- sex_act_4_rs %>%
  filter(RELATIONSHIP_STATUS == "Single or Casual")

ks.test(sa4rs_serious$n, sa4rs_casual$n)

# Homogeneity of variance
leveneTest(n ~ RELATIONSHIP_STATUS, data = sex_act_4_rs)

#######
# Independent t-test
#######

# Run the t-test
rs_t_test <- t.test(n ~ RELATIONSHIP_STATUS, data = sex_act_4_rs, var.equal = TRUE)
rs_t_test

# Effect size = Cohen's d
rs_t_test$statistic * sqrt(
  (341 + 222) / (341 * 222)
)

# COMPARE MEANS - CURRENT LIVING ------------------------------------------

# Prepare data
sex_act_4_cl <- sex_act_4 %>%
  mutate(
    CURRENT_LIVING = recode(CURRENT_LIVING, 
                            "Living w/ Children" = "Children or Family",
                            "Living w/ Other Family" = "Children or Family",
                            "Living w/ Others" = "Friends or Others",
                            "Living w/ Friends" = "Friends or Others")
  )
sex_act_4_cl

#######
# Assumptions
#######

# Do the distributions look normal and symmetrical?
ggplot(sex_act_4_cl, aes(x = n)) +
  geom_histogram() +
  facet_grid(~ CURRENT_LIVING)

# Homogeneity of variance
leveneTest(n ~ CURRENT_LIVING, data = sex_act_4_cl)

# Groups are not equal in size
sex_act_4_cl %>%
  count(CURRENT_LIVING)

# Normality - prepare data
unique_cl <- unique(sex_act_4_cl$CURRENT_LIVING)
unique_cl

# Shapiro-Wilk across variables to check for normality within each group
# Note: SW test not ideal for groups with n > 50
sex_act_4_cl %>%
  filter(CURRENT_LIVING == unique_cl[5]) %>%
  select(n) %>%
  pull() %>%
  shapiro.test()

#######
# One-way ANOVA
#######

# Fit the model to the data
one_way <- aov(n ~ CURRENT_LIVING, data = sex_act_4_cl)
one_way

# Summarize the model
summary(one_way)

# COMPARE MEANS - GENDER --------------------------------------------------

# Remove NA values
sex_act_4_gender <- sex_act_4 %>%
  filter(GENDER %in% c("Woman", "Man"))
sex_act_4_gender

# Describe increases in activities by gender
sex_act_4_gender %>%
  group_by(GENDER) %>%
  summarize(
    M = mean(n),
    SD = sd(n)
  )

#######
# Assumptions
#######

# Are the distributions normal or at least symmetrical?
ggplot(sex_act_4_gender, aes(x = n)) +
  geom_histogram() +
  facet_grid(~ GENDER)

# Sample is not approximately equal in size for each group, but is greater than 25 per group
sex_act_4_gender %>%
  group_by(GENDER) %>%
  count()

# KS test
sa4g_woman <- sex_act_4_gender %>%
  filter(GENDER == "Woman")

sa4g_man <- sex_act_4_gender %>%
  filter(GENDER == "Man")

ks.test(sa4g_woman$n, sa4g_man$n)

# Homogeneity of variance
leveneTest(n ~ GENDER, data = sex_act_4_gender)

#######
# Independent t-test
#######

# Run the t-test
g_t_test <- t.test(n ~ GENDER, data = sex_act_4_gender, var.equal = FALSE)
g_t_test

# Effect size = Cohen's d
g_t_test$statistic * sqrt(
  (339 + 221) / (339 * 221)
)

# COMPARE MEANS - SEXUAL ORIENTATION -------------------------------------

# Remove NA values
sex_act_4_so <- sex_act_4 %>%
  filter(!is.na(SEX_ORIENT)) %>%
  mutate(
    SEX_ORIENT = recode(SEX_ORIENT, "Bisexual" = "A_LGB", "Homosexual" = "A_LGB", 
                        "Mostly Heterosexual" = "A_LGB", "Mostly Homosexual" = "A_LGB",
                        "Pansexual" = "A_LGB")
  )
sex_act_4_so

# Describe increases in activities by sexual orientation
sex_act_4_so %>%
  group_by(SEX_ORIENT) %>%
  summarize(
    M = mean(n),
    SD = sd(n)
  )

#######
# Assumptions
#######

# Are the distributions normal or at least symmetrical?
ggplot(sex_act_4_so, aes(x = n)) +
  geom_histogram() +
  facet_grid(~ SEX_ORIENT)

# Sample is not approximately equal in size for each group, but is greater than 25 per group
sex_act_4_so %>%
  group_by(SEX_ORIENT) %>%
  count()

# KS test
sa4so_hetero <- sex_act_4_so %>%
  filter(SEX_ORIENT == "Heterosexual")

sa4so_lgb <- sex_act_4_so %>%
  filter(SEX_ORIENT == "A_LGB")

ks.test(sa4so_hetero$n, sa4so_lgb$n)

# Homogeneity of variance
leveneTest(n ~ SEX_ORIENT, data = sex_act_4_so)

#######
# Independent t-test
#######

# Run the t-test
so_t_test <- t.test(n ~ SEX_ORIENT, data = sex_act_4_so, var.equal = FALSE)
so_t_test

# Effect size = Cohen's d
so_t_test$statistic * sqrt(
  (446 + 119) / (446 * 119)
)

# WHY FANTASIES CHANGE ---------------------------------------------------

# Prepare data frame
sa_qual <- survey %>%
  select(ID, starts_with(c("SA_START", "SA_FANTA"))) %>%
  filter(!is.na(SA_FANTASIZING_QUAL))
sa_qual

# Percent reporting a qualitative reason
186 / 194

# When people mention "dreams" what do they mean?
dreams <- str_detect(sa_qual$SA_FANTASIZING_QUAL, regex("^*dream", ignore_case = TRUE))
sa_qual[dreams,]

# Manual review indicates that dreams refer to increases in fantasizing

# Script to count common themes
sa_qual_code <- sa_qual %>%
  select(SA_FANTASIZING_QUAL) %>%
  # Script constructed interatively by glancing through the written responses to identify
  # key words for the regular expression
  mutate(
    # More fantasies, greater frequency
    more = ifelse(str_detect(SA_FANTASIZING_QUAL, 
                             regex("^*frequen|often|fanst|more of them|increase|more common|more physic|occur more|dream|it more|sex more|more sex", 
                                   ignore_case = TRUE)), 
                   1, 0),
    # Being outside or in public
    outside = ifelse(str_detect(SA_FANTASIZING_QUAL, 
                                regex("^*outside|place|other person", 
                                      ignore_case = TRUE)), 
                   1, 0),
    # Other people in life
    others = ifelse(str_detect(SA_FANTASIZING_QUAL, 
                               regex("^*people|celebr|women|man|don't care who|person|multi|more partne", 
                                     ignore_case = TRUE)), 
                      1, 0),
    # Longing for partner
    partner = ifelse(str_detect(SA_FANTASIZING_QUAL, 
                                regex("^*my partner|wife|see each other", 
                                      ignore_case = TRUE)), 
                      1, 0),
    # Greater intensity, variety
    intense_var = ifelse(str_detect(SA_FANTASIZING_QUAL, 
                                    regex("^*intense|elabor|vari|vivid|expand|fun|different|new thing|extreme|hardcore|curious|exotic|niche|explor|more desire", 
                                          ignore_case = TRUE)), 
                      1, 0),
    # No change in fantasizing
    none = ifelse(str_detect(SA_FANTASIZING_QUAL, 
                             regex("^*none|not chang|unchang|don't have|haven't", 
                                   ignore_case = TRUE)), 
                      1, 0),
    # Greater intimacy
    intimacy = ifelse(str_detect(SA_FANTASIZING_QUAL, 
                                 regex("^*hug|more romantic|intima|passion", 
                                       ignore_case = TRUE)), 
                      1, 0),
    # Not sure about the change
    unsure = ifelse(str_detect(SA_FANTASIZING_QUAL, 
                               regex("^*don't know|unsure", 
                                     ignore_case = TRUE)), 
                      1, 0),
    # Changes in time, boredom
    time = ifelse(str_detect(SA_FANTASIZING_QUAL, 
                               regex("^*time|bore|without sex|frustrated|house bound|not being bale|lockdown", 
                                     ignore_case = TRUE)), 
                    1, 0),
    # A category of coding to capture other fantasies
    bucket = ifelse(str_detect(SA_FANTASIZING_QUAL, 
                               regex("^*dicks|sexting|escort|dominant|shag|single|mello", 
                                     ignore_case = TRUE)), 
                      1, 0)
  )
sa_qual_code

# Filter script to determine which responses belong in the bucket
sa_qual_code %>%
  filter(
    more == 0 &
    outside == 0 &
    others == 0 &
    partner == 0 &
    intense_var == 0 &
    none == 0 &
    intimacy == 0 &
    unsure == 0 &
    time == 0 &
    bucket == 0
  )

# Frequencies of codes
sa_qual_code %>%
  select(-starts_with("SA_")) %>%
  gather(key = "code", value = "present") %>%
  # Get frequencies
  group_by(code) %>%
  count(present) %>%
  # Remove non-occurrences
  filter(present != 0) %>%
  # Calculate present
  mutate(percent = (n / nrow(sa_qual_code)) * 100) %>%
  # Most common on top of dataframe
  arrange(desc(n))

# Find quotes for table; note, change the name of the variable to explore the quotes with View()
a <- sa_qual_code %>%
  filter(outside == 1)
View(a)
