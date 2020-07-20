# Dependencies
library(tidyverse)

# Import
survey <- read_csv("data/covid_sex_tech.csv")

# CLEAN SEXUAL ACTIVITIES VARIABLES ---------------------------------------

# Select the main variables
sex_act <- survey %>%
  select(starts_with("SA")) %>%
  select(-ends_with("QUAL"))

# Split the values across variables
sex_act_list <- sex_act %>%
  select(-ends_with("ING")) %>%
  map(str_split, ",") %>%
  as_tibble()

# DESCRIPTIVE ANALYSES ----------------------------------------------------

# Have participants engaged in more sexual fantasizing during lockdown?
within(sex_act, {
  SA_STARTED_FANTASIZING <- recode(as.character(SA_STARTED_FANTASIZING), "0" = "No", "1" = "Yes")
}) %>%
  select(ends_with("ING")) %>%
  group_by(SA_STARTED_FANTASIZING) %>%
  count() %>%
  mutate(percent = n / 565)

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

# SIGNIFANCE TESTS --------------------------------------------------------

#######
# Chi-square test of independence - prepare
#######

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

#######
# Chi-square test of independence - omnibus execute
#######

# Omnibus frequencies
sa_chi_table_1 %>%
  as_tibble() %>%
  summarize(
    total_b = sum(before),
    total_d = sum(during),
    total_l = sum(less),
    total_m = sum(more),
    grand = total_b + total_d + total_l + total_m
  ) %>%
  gather(key = totals, value = number) %>%
  # Add column percent
  mutate(col_percent = number / 5945) %>%
  # Add pooled percent
  mutate(pool_percent = number / (565 * 14))

# Calculate the omnibus chi-square test for before vs. during across sexual activities
chisq_sa_engaged <- chisq.test(sa_chi_table_1[-c(3:5)])
chisq_sa_engaged

# Total participants
totals_engaged <- chisq_sa_engaged$observed %>%
  as_tibble() %>%
  summarize(
    total_before = sum(before),
    total_during = sum(during),
    grand = total_before + total_during
  )
totals_engaged

# Effect size - Cramer's V
sqrt(
  chisq_sa_engaged$statistic[[1]] / (totals_engaged$grand * (2 - 1))
)

# Calculate the omnibus chi-square test for less vs. more across sexual activities
chisq_sa_change <- chisq.test(sa_chi_table_1[-c(1, 2, 5)])
chisq_sa_change

# Total participants
totals_change <- chisq_sa_change$observed %>%
  as_tibble() %>%
  summarize(
    total_less = sum(less),
    total_more = sum(more),
    grand = total_less + total_more
  )
totals_change

# Effect size - Cramer's V
sqrt(
  chisq_sa_change$statistic[[1]] / (totals_change$grand * (2 - 1))
)

# Visualize chi-square table
sa_chi_table_1 %>%
  rownames_to_column() %>%
  as_tibble() %>%
  gather(key = engagement, value = value, -rowname) %>%
  ggplot(aes(x = engagement, y = rowname)) +
    geom_point(aes(size = value), shape = 21, colour = "black", fill = "skyblue") + 
    theme(
      panel.background = element_blank(), 
      panel.border = element_rect(colour = "blue", fill = NA, size = 1)
    ) +
  scale_size_area(max_size = 20)
