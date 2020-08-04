# Dependencies
library(tidyverse)

# Import
survey <- read_csv("data/covid_sex_tech.csv")

# CLEAN SOCTECH VARIABLES -------------------------------------------------

# Select variables to analyze
soctech <- survey %>%
  select(starts_with("SOCTECH")) %>%
  select(-ends_with("QUAL")) %>%
  select(-c("SOCTECH_TRANSITION"))

# Select qualitative variables to analyze
soctech_qual <- survey %>%
  select(ID, starts_with("SOCTECH")) %>%
  select(ID, ends_with(c("TRANSITION", "QUAL")))

# Label all the values
soctech_label <- within(soctech, {
  SOCTECH_CURENT <- recode(as.character(SOCTECH_CURENT), "0" = "No", "1" = "Yes")
  SOCTECH_SIGNUP <- recode(as.character(SOCTECH_SIGNUP), "0" = "No", "1" = "Yes", "2" = "Unsure",
                           "99" = "Unknown")
  SOCTECH_CHANGE <- recode(as.character(SOCTECH_CHANGE), "1" = "Increased", "2" = "Decreased",
                           "3" = "Same")
  SOCTECH_NEWS <- recode(as.character(SOCTECH_NEWS), "0" = "No", "1" = "Yes", "2" = "Unsure")
})
soctech_label

# Break up SOCTECH_WHICH
soctech_which_break <- str_split(soctech$SOCTECH_WHICH, ",") %>%
  unlist() %>%
  as_tibble()

# DESCRIPTIVE ANALYSES ----------------------------------------------------

# How many participants did not select social media? 
# Note, this returns 28 and a subsequent analysis returns 27
sum(is.na(soctech$SOCTECH_WHICH))

# Describe SOCTECH_WHICH
soctech_which_break %>%
  filter(!is.na(value)) %>%
  group_by(value) %>%
  count() %>%
  arrange(desc(n)) %>%
  # 565 - 28 because 28 did not answer this question
  mutate(percent = n / 565)

# Other social networks use?
soctech_label %>%
  select(SOCTECH_OTHER) %>%
  filter(!is.na(SOCTECH_OTHER))

#######
# Continuous variables
#######

# How much have you used technology (e.g. Whatsapp, HouseParty, Zoom) to keep 
# connected to your social circles? 
soctech_label %>%
  select(SOCTECH_HOW_MUCH) %>%
  # Remove missing values n = 2
  filter(!is.na(SOCTECH_HOW_MUCH)) %>%
  # Sliding scale ranging from 0 to 100
  summarize(
    M = mean(SOCTECH_HOW_MUCH),
    SD = sd(SOCTECH_HOW_MUCH)
  )

# Have social networking platforms been useful during the social isolation?
soctech_label %>%
  select(SOCTECH_USEFUL) %>%
  # Remove missing values n = 27
  filter(!is.na(SOCTECH_USEFUL)) %>%
  # Likert-type item, so calculate M and SD
  summarize(
    M = mean(SOCTECH_USEFUL),
    SD = sd(SOCTECH_USEFUL)
  )

# What is your impression of social networking sites during social lockdown?
soctech_label %>%
  select(SOCTECH_IMPRESSION) %>%
  # Remove missing values n = 27
  filter(!is.na(SOCTECH_IMPRESSION)) %>%
  # Likert-type item, so calculate M and SD
  summarize(
    M = mean(SOCTECH_IMPRESSION),
    SD = sd(SOCTECH_IMPRESSION)
  )

#######
# Discrete variables
#######

# Do you currently have a profile on a social networking platform? 
soctech_label %>%
  select(SOCTECH_CURENT) %>%
  group_by(SOCTECH_CURENT) %>%
  count() %>%
  mutate(percent = n / 565)

# Have you signed up to any new platforms since the social isolation?
soctech_label %>%
  select(SOCTECH_SIGNUP) %>%
  group_by(SOCTECH_SIGNUP) %>%
  count() %>%
  mutate(percent = n / 565)

# How has your use of social networking platforms changed during social isolation?
soctech_label %>%
  select(SOCTECH_CHANGE) %>%
  group_by(SOCTECH_CHANGE) %>%
  count() %>%
  mutate(percent = n / 565)

# Have you used social networking platforms to keep up to date with the latest news?
soctech_label %>%
  select(SOCTECH_NEWS) %>%
  group_by(SOCTECH_NEWS) %>%
  count() %>%
  mutate(percent = n / 565)

# NLP - SOCTECH_SIGNUP_QUAL -----------------------------------------------

# If you signed up for new SNS, which platforms have you signed up to?
#######

# Number of participants answering the question
soctech_qual %>%
  select(SOCTECH_SIGNUP_QUAL) %>%
  filter(!is.na(SOCTECH_SIGNUP_QUAL))

# Percent of new accounts
platforms <- soctech_qual %>%
  select(SOCTECH_SIGNUP_QUAL) %>%
  filter(!is.na(SOCTECH_SIGNUP_QUAL)) %>%
  mutate(
    tiktok = ifelse(str_detect(SOCTECH_SIGNUP_QUAL,
                               regex("tiktok", ignore_case = TRUE)),
      1, 0
    ),
    snap = ifelse(str_detect(SOCTECH_SIGNUP_QUAL,
                             regex("snap", ignore_case = TRUE)),
      1, 0
    ),
    zoom = ifelse(str_detect(SOCTECH_SIGNUP_QUAL,
                             regex("zoom", ignore_case = TRUE)),
      1, 0
    ),
    houseparty = ifelse(str_detect(SOCTECH_SIGNUP_QUAL,
                                   regex("house", ignore_case = TRUE)),
      1, 0
    ),
    discord = ifelse(str_detect(SOCTECH_SIGNUP_QUAL,
                                regex("disco", ignore_case = TRUE)),
      1, 0
    ),
    face_insta = ifelse(str_detect(SOCTECH_SIGNUP_QUAL,
                                   regex("face|insta", ignore_case = TRUE)),
      1, 0
    )
  )
platforms

# Create other category
platforms_other <- platforms %>%
  filter(
    tiktok != 1 &
    snap != 1 &
    zoom != 1 &
    houseparty != 1 &
    discord != 1 &
    face_insta != 1
  )  %>%
  select(SOCTECH_SIGNUP_QUAL) %>%
  mutate(other = rep(1, nrow(.))) %>%
  count(other) %>%
  select(n) %>%
  mutate(platform = "other")
platforms_other

# Combine dataframes
platforms %>%
  select(-SOCTECH_SIGNUP_QUAL) %>%
  gather(key = "platform", value = "occurrence") %>%
  filter(occurrence == 1) %>%
  count(platform) %>%
  rbind(platforms_other) %>%
  mutate(percent = (n / nrow(platforms)) * 100) %>%
  arrange(desc(n))

# NLP - SOCTECH_USEFUL_QUAL -----------------------------------------------

# If SNS has been useful during social isolation, how have they been useful?
#######

# Categorize the open-ended data
s_qual_useful <- soctech_qual %>%
  select(SOCTECH_USEFUL_QUAL) %>%
  filter(!is.na(SOCTECH_USEFUL_QUAL)) %>%
  mutate(
    # Useful because it allows people to stay in touch
    contact = ifelse(str_detect(SOCTECH_USEFUL_QUAL,
                                regex("in (touch|contact)|connect|communicat|contact.+(friend|family|people|mother)|^keep.+(friend|family|people)|friend|family|loved one|contact others|incontact|continue contact|relatives|talk to.+people|cannot see", ignore_case = TRUE)),
      1, 0
    ),
    # useful because it is a distraction / relaxation method during the pandemic
    entertain = ifelse(str_detect(SOCTECH_USEFUL_QUAL,
                                  regex("distra|busy|engag|occupi|relax|entertain|pass.+time|bor|kill.+time|wasting time|use of time|free time|extra time", ignore_case = TRUE)),
      1, 0
    ),
    # Useful to find out about COVID-19
    covid = ifelse(str_detect(SOCTECH_USEFUL_QUAL,
                              regex("covid|lockdown|corona|pandemic|scien|virus|id-19|government", ignore_case = TRUE)),
      1, 0 
    ),
    # A response with no depth, such as none or yes
    none = ifelse(str_detect(SOCTECH_USEFUL_QUAL,
                             regex("none|haven't|yes|n/a|1|2|3|-|na$", ignore_case = TRUE)),
      1, 0
    ),
    # Participants only listed a social media platform
    soc_media = ifelse(str_detect(SOCTECH_USEFUL_QUAL,
                                  regex("snapchat$|gram$|facebook$|twitter$|tiktok$|reddit$", ignore_case = TRUE)),
      1, 0
    ),
    # General news, unspecified to COVID-19
    news = ifelse(str_detect(SOCTECH_USEFUL_QUAL,
                             regex("news$", ignore_case = TRUE)),
      1, 0
    )
  )
s_qual_useful

# Create possible other category
other_use <- s_qual_useful %>%
  filter(
    contact != 1 &
    entertain != 1 &
    covid != 1 &
    none != 1 &
    soc_media != 1 &
    news != 1
  )
other_use
View(other_use)

# Determine which news items remain
# Will not parse with regex, unnecessary because the category will not be in the top three
other_use %>% 
  filter(str_detect(SOCTECH_USEFUL_QUAL, regex("news", ignore_case = TRUE)))

# Calculate frequencies
s_qual_useful %>%
  select(-SOCTECH_USEFUL_QUAL) %>%
  gather(key = "category", value = "occurrence") %>%
  # Keep only positive occurrences 
  filter(occurrence == 1) %>%
  count(category) %>%
  mutate(percent = (n / nrow(s_qual_useful)) * 100) %>%
  # Report the top three, minus the category none, which was a catch-all for poor quality responses
  arrange(desc(n))

# Number of people who explained why social media was useful
nrow(s_qual_useful)

# NLP - SOCTECH_IMPRESSION_QUAL -------------------------------------------

# Please explain your impression of SNS during social lockdown?
#######

# Modified conventional content analysis to categorize impressions
s_qual_impression <- soctech_qual %>%
  select(SOCTECH_IMPRESSION_QUAL) %>%
  filter(!is.na(SOCTECH_IMPRESSION_QUAL)) %>%
  mutate(
    # Positive because keeping in contact
    contact = ifelse(str_detect(SOCTECH_IMPRESSION_QUAL,
                                regex("in (touch|contact)|connect|communicat|contact.+(friend|family|people|mother)|^keep.+(friend|family|people)|friend|family|loved one|contact others|incontact|continue contact|relatives|talk to.+people|cannot see|social contact|active social|isolated", ignore_case = TRUE)),
      1, 0
    ),
    # Positive distraction / relaxation method during the pandemic
    entertain = ifelse(str_detect(SOCTECH_IMPRESSION_QUAL,
                                  regex("distra|busy|engag|occupi|relax|entertain|pass.+time|bor|kill.+time|wasting time|use of time|free time|extra time|something to do|alle.+anxiety", ignore_case = TRUE)),
      1, 0
    ),
    # Positive because they are useful
    useful = ifelse(str_detect(SOCTECH_IMPRESSION_QUAL,
                               regex("useful", ignore_case = TRUE)),
      1, 0
    ),
    # Positive due to humor
    humor = ifelse(str_detect(SOCTECH_IMPRESSION_QUAL,
                              regex("fun|humor|laugh", ignore_case = TRUE)),
      1, 0
    ),
    # Positive due to informed
    inform = ifelse(str_detect(SOCTECH_IMPRESSION_QUAL,
                               regex("(?<!mis)inform|up to date|follow.+news|recent changes", ignore_case = TRUE)),
      1, 0
    ),
    # Positive because great and good
    gg = ifelse(str_detect(SOCTECH_IMPRESSION_QUAL,
                           regex("good|great", ignore_case = TRUE)),
      1, 0
    ),
    # Positive because of coming together in the face of COVID-19
    unity = ifelse(str_detect(SOCTECH_IMPRESSION_QUAL,
                              regex("come together|go.+through.+same", ignore_case = TRUE)),
      1, 0
    ),
    # Ambivalent
    ambiv = ifelse(str_detect(SOCTECH_IMPRESSION_QUAL,
                              regex("good.+bad|positive.+nega|better.+worse|not.+opinion|neutral|50|don't.+opinion|don't always believe", ignore_case = TRUE)),
      1, 0
    ),
    # No change in impressions
    none = ifelse(str_detect(SOCTECH_IMPRESSION_QUAL,
                             regex("much different|don't.+use|don't.+change|same to me|not.+change|not effec", ignore_case = TRUE)),
      1, 0
    ),
    # Negative due to misinformation
    fake = ifelse(str_detect(SOCTECH_IMPRESSION_QUAL,
                             regex("misinformation|fake|incorrect|innac|propaga|untrue|not.+accur|false", ignore_case = TRUE)),
      1, 0
    ),
    # Negative because content is negative
    neg_content = ifelse(str_detect(SOCTECH_IMPRESSION_QUAL,
                                    regex("(bad|negative|scary) news|rubbish|complain|negative about|entirely coronavirus|negative opinion|increase.+nega|negative (affect|effect)|(affect|effect).+ negative|hateful|used neg|censor|scare mong|negative things", ignore_case = TRUE)),
      1, 0
    )
  )
s_qual_impression

# Possible other category
other_imp <- s_qual_impression %>%
  filter(
    contact != 1 &
    entertain != 1 &
    useful != 1 &
    humor != 1 &
    inform != 1 &
    gg != 1 &
    unity != 1 &
    ambiv != 1 &
    none != 1 &
    fake != 1 &
    neg_content != 1
  )
other_imp
View(other_imp)

# Calculate frequencies
s_qual_impression %>%
  select(-SOCTECH_IMPRESSION_QUAL) %>%
  gather(key = "category", value = "occurrence") %>%
  filter(occurrence == 1) %>%
  count(category) %>%
  mutate(percent = (n / nrow(s_qual_impression)) * 100) %>%
  arrange(desc(n))

# NLP - SOCTECH_TRANSITION ------------------------------------------------

# If you used SNS more, please describe the transition in moving from communication in-person to online?
#######

# Number of participants answering the question
soctech_qual %>%
  select(SOCTECH_TRANSITION) %>%
  filter(!is.na(SOCTECH_TRANSITION))

# NLP - SOCTECH_NEWS_QUAL -------------------------------------------------

# How have you used SNS to keep up to date with the latest news?
#######

# Number of participants answering the question
soctech_qual %>%
  select(SOCTECH_NEWS_QUAL) %>%
  filter(!is.na(SOCTECH_NEWS_QUAL))