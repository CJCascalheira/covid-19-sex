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
                              regex("covid|lockdown|corona|pandemic|scien|virus|id-19", ignore_case = TRUE)),
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
                                regex("in (touch|contact)|connect|communicat|contact.+(friend|family|people|mother)|^keep.+(friend|family|people)|friend|family|loved one|contact others|incontact|continue contact|relatives|talk to.+people|cannot see|social contact|active social|isolated|people up to|messaging|interact|message people|talk.+each other|less.+lone|maintain rela|still helps|people.+up to|social aspect.+add|stay together", ignore_case = TRUE)),
      1, 0
    ),
    # Positive distraction / relaxation method during the pandemic
    entertain = ifelse(str_detect(SOCTECH_IMPRESSION_QUAL,
                                  regex("distra|busy|engag|occupi|relax|entertain|pass.+time|bor|kill.+time|wasting time|use of time|free time|extra time|something to do|alle.+anxiety|remove.+mono|escape", ignore_case = TRUE)),
      1, 0
    ),
    # Positive because they are useful
    useful = ifelse(str_detect(SOCTECH_IMPRESSION_QUAL,
                               regex("useful|learn", ignore_case = TRUE)),
      1, 0
    ),
    # Positive due to humor
    humor = ifelse(str_detect(SOCTECH_IMPRESSION_QUAL,
                              regex("fun|humor|laugh|positive things|positivity|happy", ignore_case = TRUE)),
      1, 0
    ),
    # Positive due to informed
    inform = ifelse(str_detect(SOCTECH_IMPRESSION_QUAL,
                               regex("(?<!mis)inform|up to date|follow.+news|recent changes", ignore_case = TRUE)),
      1, 0
    ),
    # Positive because great and good
    gg = ifelse(str_detect(SOCTECH_IMPRESSION_QUAL,
                           regex("good|great|more positive|fan of|no neg|not misle|lot.+positive|10000|generally pos", ignore_case = TRUE)),
      1, 0
    ),
    # Positive because of coming together in the face of COVID-19
    unity = ifelse(str_detect(SOCTECH_IMPRESSION_QUAL,
                              regex("come together|go.+through.+same|people.+together|spread.+support|support.+other|act.+kind|people.+support|hero|stick.+together|less alone|more together|key worker|same boat|camara|encourag|as me|offer.+help", ignore_case = TRUE)),
      1, 0
    ),
    # Ambivalent
    ambiv = ifelse(str_detect(SOCTECH_IMPRESSION_QUAL,
                              regex("ambiv|good.+bad|positive.+nega|better.+worse|not.+opinion|neutral|50|don't.+opinion|don't always believe|^unsure|extremes$|usual mix|bad.+posi", ignore_case = TRUE)),
      1, 0
    ),
    # No change in impressions
    none = ifelse(str_detect(SOCTECH_IMPRESSION_QUAL,
                             regex("much different|don't.+use|don't.+change|same to me|not.+change|not effec|not.+use.+social|opinion.+same|not.+bad|not sure|no real change|same.+as before|hasn.+change|the same$|any diff|same routine|norma|about the same", ignore_case = TRUE)),
      1, 0
    ),
    # Undifferentiated negative
    undiff_neg = ifelse(str_detect(SOCTECH_IMPRESSION_QUAL,
                                   regex("void|too much$|more down", ignore_case = TRUE)),
      1, 0
    ),
    # Negative due to misinformation
    fake = ifelse(str_detect(SOCTECH_IMPRESSION_QUAL,
                             regex("misinformation|fake|incorrect|innac|propaga|untrue|not.+accur|false|believe what|not.+trust|conspir|warrior|stories aren.+true|sensationa|exagg.+media", ignore_case = TRUE)),
      1, 0
    ),
    # Negative because content is negative
    neg_content = ifelse(str_detect(SOCTECH_IMPRESSION_QUAL,
                                    regex("(bad|negative|scary) news|negative content|rubbish|complain|negative about|entirely coronavirus|negative opinion|increase.+nega|negative (affect|effect)|(affect|effect).+ negative|hateful|used neg|censor|scare mong|negative things|show off|negative posts|bad things|negative online|use.+negative|not a lot.+positive|yell at|people.+horrible|feel bad|nonsense post|blame|ads|very neg|stupid opin|outrage|nasti|negative.+people|have an opin|stupid things", ignore_case = TRUE)),
      1, 0
    ),
    # Negative due to COVID-19 
    covid = ifelse(str_detect(SOCTECH_IMPRESSION_QUAL,
                              regex("rules.+breaking|break.+rule|covid.+worr|negat.+covid|anxi.+news|saturated", ignore_case = TRUE)),
      1, 0
    ),
    # Negative because of the structure of social media
    soc_structure = ifelse(str_detect(SOCTECH_IMPRESSION_QUAL,
                                      regex("never.+amazing|time well spent|forever caught|be better$|snapshot|looking.+other.+doing|outdo|leave face|others.+motiva|social media.+unhelpful|sucked in|addict|competi", ignore_case = TRUE)),
      1, 0
    ),
    # Negative due to government
    gov = ifelse(str_detect(SOCTECH_IMPRESSION_QUAL,
                                       regex("gover|politi|government.+fail", ignore_case = TRUE)),
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
    undiff_neg != 1 &
    fake != 1 &
    neg_content != 1 &
    covid != 1 &
    soc_structure != 1 &
    gov != 1
  ) %>%
  select(SOCTECH_IMPRESSION_QUAL) %>%
  mutate(category = rep("other", nrow(.))) %>%
  count(category) %>%
  mutate(percent = (n / nrow(s_qual_impression)) * 100)
other_imp

# Calculate frequencies
s_qual_impression %>%
  select(-SOCTECH_IMPRESSION_QUAL) %>%
  gather(key = "category", value = "occurrence") %>%
  filter(occurrence == 1) %>%
  count(category) %>%
  mutate(percent = (n / nrow(s_qual_impression)) * 100) %>%
  rbind(other_imp) %>%
  arrange(desc(n))

# NLP - SOCTECH_TRANSITION ------------------------------------------------

# If you used SNS more, please describe the transition in moving from communication in-person to online?
#######

# Number of participants answering the question
s_qual_transit <- soctech_qual %>%
  select(SOCTECH_TRANSITION) %>%
  # Remove missing values
  filter(!is.na(SOCTECH_TRANSITION)) %>%
  # Remove participants who stated that they did not use SNS more
  filter(!str_detect(SOCTECH_TRANSITION,
                    regex("NA.$|na$|n/a|dont$|less$|(no|not) more|not applicable", ignore_case = TRUE)))

# Percent answering the question
nrow(s_qual_transit_1) / nrow(soctech_qual)

# Categorize responses
s_qual_transit_1 <- s_qual_transit %>%
  # Generate categories
  mutate(
    # The transition was easy or was not perceived as a change
    easy = ifelse(str_detect(SOCTECH_TRANSITION,
                             regex("easy$|not difficult$|fine$|used to it|easy transition$|hasn't change|haven.+notic|simple|not.+difficult$|hasn.+struggle|same really|easy.+(familiar|same)|used.+(anyway|already)|(already|always) (using|used)|(already|always|familiar).+online|feel the same|not.+different|same as.+always|(pretty|fairly) natural|natural transition|(pretty|quite|relatively) easy|(easier|smooth) transition|a lot.+tech|very organic|stay.+(similar|same)|always have|lots anyways|not easy|always prefer|no.+transi|not.+drastic|(no|not much).+(change|transition)|seamless|transition.+very easy|wasn't.+hard|no.+issues|online anyway|used.+before|hasn't been too|time.+before.+lockdown", ignore_case = TRUE)),
      1, 0
    ),
    # Transition was difficult in general
    difficult = ifelse(str_detect(SOCTECH_TRANSITION,
                                  regex("difficult.+used to|negative.+overall|hard.$|wasnt easy|more effort|harder|^difficult|been (hard|difficult)|i struggle|difficult (toget|to reach)|quite hard", ignore_case = TRUE)),
      1, 0 
    ),
    # Misses the face-to-face interaction
    miss = ifelse(str_detect(SOCTECH_TRANSITION,
                             regex("face to face|face-to-face|less engag|quality.+decrease|prefer.+(physical|in person|person)|less natural|(can't|miss).+hug|miss seeing|flow less|not.+(smooth|personal)|dont like|less connect|see in-person|not.+(seeing|intimate)|miss.+(connec|them)|lack.+physical|takes away|cant be done|doesn't replace|want to meet|see.+more.+in person|empty|thoughtless|taxing", ignore_case = TRUE)),
      1, 0
    ),
    # Difficult due to unsettling---strange, awkward, jarring
    unsettle = ifelse(str_detect(SOCTECH_TRANSITION,
                                 regex("strange|awkward|jarring|weird|odd", ignore_case = TRUE)),
      1, 0
    ),
    # Describes how they connect with others
    describe = ifelse(str_detect(SOCTECH_TRANSITION,
                                 regex("called more|houseparty|zoom|whatsapp|facetime|face time|text.+more|more.+text|video (call|chat)|instant messaging|rely on text|use technology|facetiming|Facebook|family online|90|online now|communic.+online|group call|email|camera|good platform|skype|(schedule|regular) call|discord|messaging app|only (way|thing)|set.+people.+device", ignore_case = TRUE)),
      1, 0
    ),
    # Different experience, but fun and interesting
    enjoy = ifelse(str_detect(SOCTECH_TRANSITION,
                              regex("fun|enjoy|interesting|easier|reconnect with|now.+love|netflix part|quiz|feel.+closer", ignore_case = TRUE)),
      1, 0
    ),
    # Novel experiences not available usually
    new = ifelse(str_detect(SOCTECH_TRANSITION,
                            regex("usually wouldn't|not normally|more time|more frequent|more confiden|celebrate birthdays|weekly (sessions|catch)|set times.+each other", ignore_case = TRUE)),
      1, 0
    )
  )
s_qual_transit_1

# Possible other category
transit_other <- s_qual_transit_1 %>%
  filter(
    easy != 1 &
    difficult != 1 &
    miss != 1 &
    unsettle != 1 &
    describe != 1 &
    enjoy != 1 &
    new != 1
  ) %>%
  # There has been enough categorization to lump the rest into an other category
  # because the percent of responses will be less than 10% (i.e., less representative)
  select(SOCTECH_TRANSITION) %>%
  mutate(category = rep("other", nrow(.))) %>%
  count(category) %>%
  mutate(percent = (n / nrow(s_qual_transit)) * 100)
transit_other
View(transit_other)

# Calculate frequencies
s_qual_transit_1 %>%
  select(-SOCTECH_TRANSITION) %>%
  gather(key = "category", value = "occurrence") %>%
  filter(occurrence == 1) %>%
  count(category) %>%
  mutate(percent = (n / nrow(s_qual_transit)) * 100) %>%
  rbind(transit_other) %>%
  arrange(desc(n))

# Check for nuances in categories by entering the name of the category into
# the following code, iteratively, and then scanning responses to ensure
# proper categorization
example <- s_qual_transit_1 %>%
  filter(miss == 1)
View(example)

# NLP - SOCTECH_NEWS_QUAL -------------------------------------------------

# How have you used SNS to keep up to date with the latest news?
#######

# Number of participants answering the question
s_qual_news <- soctech_qual %>%
  select(SOCTECH_NEWS_QUAL) %>%
  filter(!is.na(SOCTECH_NEWS_QUAL))
View(s_qual_news)

# Based on a scan of the responses, it does not appear that this variable
# will fit with the manuscript