# Dependencies
library(tidyverse)
library(chisq.posthoc.test)

# Import
survey <- read_csv("data/covid_sex_tech.csv")

# CLEAN VARIABLES ----------------------------------------------------

# Just the porn, fantasy, sex activity variables
solo <- survey %>%
  # Select  variables
  select(ID, PORN_CHANGE_WHICH, SA_STARTED_FANTASIZING, SA_SOLOMASTURBATION, 
         SA_SEXTOYSSOLO, SA_WATCHPORNSOLO)
solo

# Prepare dataframe for analyses
solo_1 <- solo %>%
  # Specify occurrence of solo sexual activities
  mutate(
    # Porn variables
    porn_masturbate = if_else(str_detect(PORN_CHANGE_WHICH,
                                   regex("masturbat*", ignore_case = TRUE)),
                              "yes mastur", "no mastur"
    ),
    porn_alone = if_else(str_detect(PORN_CHANGE_WHICH, 
                                    regex("alone", ignore_case = TRUE)), 
                         "yes alone", "no alone"
    ),
    porn_quick = if_else(str_detect(PORN_CHANGE_WHICH, 
                                    regex("quick", ignore_case = TRUE)), 
                         "yes quick", "no quick"
    ),
    porn_longer = if_else(str_detect(PORN_CHANGE_WHICH, 
                                    regex("longer", ignore_case = TRUE)), 
                         "yes longer", "no longer"
    ),
    # Solo sex variables
    increase_masturbate = if_else(str_detect(SA_SOLOMASTURBATION, 
                                     regex("more during", ignore_case = TRUE)), 
                          "more mastur", "other"
    ),
    increase_toy = if_else(str_detect(SA_SEXTOYSSOLO, 
                                     regex("more during", ignore_case = TRUE)), 
                          "more toy", "other"
    ),
    increase_solo_porn = if_else(str_detect(SA_WATCHPORNSOLO, 
                                     regex("more during", ignore_case = TRUE)), 
                          "morn porn", "other"
    ),
    # Recode started fantasizing
    SA_STARTED_FANTASIZING = recode(SA_STARTED_FANTASIZING, "0" = "no change", "1" = "increased")
  ) %>%
  # Drop variables
  select(-c("PORN_CHANGE_WHICH", "SA_WATCHPORNSOLO", "SA_SEXTOYSSOLO", "SA_SOLOMASTURBATION"))
solo_1

# CONTINGENCY TABLE ANALYSES ----------------------------------------------

#######
# Sexual Fantasy Associations
#######

#######
# Are increases in sexual fantasizing associated with increases in masturbation?
#######
fan_increase_mastur <- table(solo_1$SA_STARTED_FANTASIZING, solo_1$increase_masturbate)
fan_increase_mastur

# Number of missing values
nrow(solo_1) - sum(fan_increase_mastur)

# Execute chi-square test of independence
chisq_fim <- chisq.test(fan_increase_mastur)
chisq_fim

# Effect size - Cramer's V
sqrt(
  chisq_fim$statistic[[1]] / (sum(fan_increase_mastur) * (2 - 1))
)

# Post-hoc pairwise comparisons
chisq.posthoc.test(fan_increase_mastur, method = "bonferroni")

# Observed and expected counts
chisq_fim$observed
chisq_fim$expected

#######
# Are increases in sexual fantasizing associated with increases in solo porn use?
#######
fan_increase_porn <- table(solo_1$SA_STARTED_FANTASIZING, solo_1$increase_solo_porn)
fan_increase_porn

# Number of missing values
nrow(solo_1) - sum(fan_increase_porn)

# Execute chi-square test of independence
chisq_fip <- chisq.test(fan_increase_porn)
chisq_fip

# Effect size - Cramer's V
sqrt(
  chisq_fip$statistic[[1]] / (sum(fan_increase_porn) * (2 - 1))
)

# Post-hoc pairwise comparisons
chisq.posthoc.test(fan_increase_porn, method = "bonferroni")

# Observed and expected counts
chisq_fip$observed
chisq_fip$expected

#######
# Are increases in soliatory porn consumption associated with increased masturbation?
#######
solo_porn_mastur <- table(solo_1$increase_solo_porn, solo_1$increase_masturbate)
solo_porn_mastur

# Number of missing values
nrow(solo_1) - sum(solo_porn_mastur)

# Execute chi-square test of independence
chisq_spm <- chisq.test(solo_porn_mastur)
chisq_spm

# Effect size - Cramer's V
sqrt(
  chisq_spm$statistic[[1]] / (sum(solo_porn_mastur) * (2 - 1))
)

# Post-hoc pairwise comparisons
chisq.posthoc.test(solo_porn_mastur, method = "bonferroni")

# Observed and expected counts
chisq_spm$observed
chisq_spm$expected
