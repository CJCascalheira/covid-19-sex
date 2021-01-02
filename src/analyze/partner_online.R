# Dependencies
library(tidyverse)
library(foreign)

# Import
survey <- read_csv("data/covid_sex_tech.csv")
total_measures <- read_csv("data/total_measures.csv")
original <- read.spss("data/raw/Data565.sav", to.data.frame = TRUE)

# Names original data frame
names(original)

# Prepare the original data frame
original_1 <- as_tibble(original) %>%
  # Add record ID for joining
  mutate(ID = 1:nrow(original)) %>%
  select(
    ID,
    Howlonghaveyoubeeninyourcurrentrelationshippleaseanswerinmonths,
    Howhassociallockdownimpactedontheamountoftimeyouspentwithyourpar,
    Wereyoulivingwithyourpartnerpreviously,
    Areyoucurrentlylivingwithyourpartners
  ) %>%
  # Rename variables
  rename(
    WITH_PARTNER_MONTHS = Howlonghaveyoubeeninyourcurrentrelationshippleaseanswerinmonths,
    IMPACT_TIME_PARTNER = Howhassociallockdownimpactedontheamountoftimeyouspentwithyourpar,
    LIVING_BEFORE_LOCKDOWN = Wereyoulivingwithyourpartnerpreviously,
    LIVE_W_PARTNER = Areyoucurrentlylivingwithyourpartners
  )
original_1

# CLEAN PARTNER ONLINE VARIABLES ------------------------------------------

# Select primary variables
partner_online <- survey %>%
  select(ID, starts_with(c("PARTNER", "ONLINE")), RELATIONSHIP_STATUS, CURRENT_LIVING) %>%
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

# Select qualitative variables
partner_qual <- survey %>%
  select(ID, PARTNER_CONTACT_QUAL, ONLINE_QUAL, RELATIONSHIP_TECH, RELATIONSHIP_QUAL) %>%
  left_join(partner_online_label, by = "ID")
partner_qual

# Combine two data frames
partners_time <- left_join(partner_online_label, original_1) %>%
  mutate(CURRENT_LIVING = recode(CURRENT_LIVING, 
                                 "Living w/ Children" = "Children or Family",
                                 "Living w/ Other Family" = "Children or Family",
                                 "Living w/ Others" = "Friends or Others",
                                 "Living w/ Friends" = "Friends or Others")) 
partners_time

# DESCRIPTIVE ANALYSES ----------------------------------------------------

# How long have you been in your current relationship?
partners_months <- partners_time %>%
  filter(RELATIONSHIP_STATUS != "Single") %>%
  select(RELATIONSHIP_STATUS, WITH_PARTNER_MONTHS) %>%
  filter(!is.na(WITH_PARTNER_MONTHS))
partners_months

# Average time
partners_months %>%
  summarize(
    M = mean(WITH_PARTNER_MONTHS) / 12,
    SD = sd(WITH_PARTNER_MONTHS) / 12,
    min = min(WITH_PARTNER_MONTHS),
    max = max(WITH_PARTNER_MONTHS) / 12
  )

# Were you living with your partner previously?
partners_time %>%
  filter(RELATIONSHIP_STATUS == "Serious Relationship") %>%
  filter(!is.na(LIVING_BEFORE_LOCKDOWN)) %>%
  count(LIVING_BEFORE_LOCKDOWN) %>%
  mutate(
    percent = n / sum(n)
  ) %>%
  arrange(desc(n))

#######

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

# Note - makes no sense to explore MSPSS and LONEV3 given that many participants lived with
# somebody else during the pandemic. 

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

# Describe the measures for this subsample
p_nhst_current %>%
  select(ends_with("SCORE")) %>%
  gather(key = "measure", value = "score") %>%
  group_by(measure) %>%
  summarize(
    M = mean(score),
    SD = sd(score)
  )

# Specify the logistic regression model
current_logit <- glm(ONLINE_CURRENT ~ SDI_SCORE, data = p_nhst_current, family = "binomial")
current_logit

# Summary of the model
summary(current_logit)

# Wald's chi-square statistic = ((beta - 0) / SE_beta)^2
# Zero omitted for clarity
# Note: same as z value ^ 2
((current_logit$coefficients[1]) / 0.528185)^2
((current_logit$coefficients[2]) / 0.008647)^2

# Exponentiate the coefficients to interpret as odds-ratios
exp(
  coef(current_logit)
)

# Model fit - difference in deviance
with(current_logit, null.deviance - deviance)

# Model fit - degrees of freedom
with(current_logit, df.null - df.residual)

# Model fit - significance
with(current_logit, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))

# NHST - H4b --------------------------------------------------------------

# Isolate the variables for the second logistic regression
p_nhst_change <- partner_nhst %>%
  filter(!is.na(ONLINE_CHANGE) & ONLINE_CHANGE != 3 & RELATIONSHIP_STATUS != "Serious Relationship")
p_nhst_change  

# Describe the measures for this subsample
p_nhst_change %>%
  select(ends_with("SCORE")) %>%
  gather(key = "measure", value = "score") %>%
  group_by(measure) %>%
  summarize(
    M = mean(score),
    SD = sd(score)
  )

# Specify the logistic regression model
change_logit <- glm(ONLINE_CHANGE ~ SDI_SCORE, data = p_nhst_change, family = "binomial")
change_logit

# Summary of the model
summary(change_logit)

# NLP - ONLINE_QUAL -------------------------------------------------------

# Please explain how your use of online dating websites has changes, if at all?
#######

# How many provided a reason for their change in online dating use?
partner_qual %>%
  filter(!is.na(ONLINE_QUAL) & ONLINE_CHANGE != "Same" & RELATIONSHIP_STATUS != "Serious Relationship")

# Classify the reasons using regular expressions
p_qual_online <- partner_qual %>%
  # Remove people who reported that their use remained the same & people in serious relationships
  filter(!is.na(ONLINE_QUAL) & ONLINE_CHANGE != "Same" & RELATIONSHIP_STATUS != "Serious Relationship") %>%
  select(ONLINE_CHANGE, ONLINE_QUAL) %>%
  mutate(
    # Generally bored or find the app / person boring
    boredom = ifelse(str_detect(ONLINE_QUAL, 
                                regex("^*bore|conversat|nothing to talk|never enjoyed", ignore_case = TRUE)),
      1, 0
    ),
    time = ifelse(str_detect(ONLINE_QUAL,
                             regex("^*more.+time", ignore_case = TRUE)),
      1, 0
    ),
    # Inability to meet
    meet = ifelse(str_detect(ONLINE_QUAL,
                           regex("^*can.+t meet|can't go|see people|lockdown|lock down|meet.+people|meet up| meet face|ee.+anyone|contact with|leave.+house|keep.+distance", ignore_case = TRUE)),
      1, 0
    ),
    # Found a loving relationship
    love = ifelse(str_detect(ONLINE_QUAL,
                             regex("^*love|relationship", ignore_case = TRUE)),
      1, 0
    )
  )
p_qual_online

# Check for other responses
p_qual_online_1 <- p_qual_online %>%
  filter(
    boredom !=1 &
    time != 1 &
    meet != 1 &
    love != 1
  ) %>%
  # Assign to a category
  mutate(other = rep(1, nrow(.))) %>%
  # Prepare to join with other data frame
  group_by(ONLINE_CHANGE) %>%
  count(other) %>%
  ungroup() %>%
  mutate(percent = n / nrow(p_qual_online),
         category = rep("other", 2)) %>%
  select(ONLINE_CHANGE, category, everything(), -other)
p_qual_online_1

# categories by increase or decrease
p_qual_online %>%
  select(-ONLINE_QUAL) %>%
  gather(key = "category", value = "occurrence", -ONLINE_CHANGE) %>%
  # Remove zeros because the category was not present
  filter(occurrence != 0) %>%
  group_by(ONLINE_CHANGE) %>%
  # How often did the category occur by group
  count(category) %>%
  mutate(percent = (n / nrow(p_qual_online)) * 100) %>%
  ungroup() %>%
  rbind(p_qual_online_1) %>%
  arrange(desc(n))

# Meet
32 + 3

# Boredeom
18 + 3

# Time
15 + 2

# Other
5 + 2

# Love
4 + 2

# TIME SPENT WITH PARTNER -------------------------------------------------

# IMPACT_TIME_PARTNER - scale meaning
# - 0 = a lot less time
# - 100 = a lot more time

# Visual inspection of normality
hist(partners_time$WITH_PARTNER_MONTHS)
hist(partners_time$IMPACT_TIME_PARTNER)

# Does not make sense to execute a correlation

# How did social lockdown impact the amount of time spent with partners?

# Prepare the analysis
partners_time_1 <- partners_time %>%
  filter(RELATIONSHIP_STATUS == "Serious Relationship") %>%
  filter(!is.na(IMPACT_TIME_PARTNER)) %>%
  select(RELATIONSHIP_STATUS, LIVE_W_PARTNER, CURRENT_LIVING, IMPACT_TIME_PARTNER)
partners_time_1

# Just those living with a partner
pt_w_partner <- partners_time_1 %>%
  filter(LIVE_W_PARTNER == "Yes") %>%
  select(IMPACT_TIME_PARTNER) %>%
  pull()
pt_w_partner

# Just participants living without a partner
pt_wo_partner <- partners_time_1 %>%
  filter(LIVE_W_PARTNER == "No") %>%
  select(IMPACT_TIME_PARTNER) %>%
  pull()
pt_wo_partner

# Serious and casual - living with partners?
partners_time %>%
  filter(RELATIONSHIP_STATUS != "Single") %>%
  filter(!is.na(IMPACT_TIME_PARTNER)) %>%
  select(RELATIONSHIP_STATUS, LIVE_W_PARTNER, IMPACT_TIME_PARTNER) %>%
  filter(LIVE_W_PARTNER == "No")

# Therefore, no comparison possible of partner vs. no partner

# Impact of social lockdown on time spent with partner
partners_time_1 %>%
  summarize(
    M = mean(IMPACT_TIME_PARTNER),
    SD = sd(IMPACT_TIME_PARTNER)
  )

# Just those living with just a partner
w_partner_only <- partners_time_1 %>%
  filter(LIVE_W_PARTNER == "Yes") %>%
  filter(CURRENT_LIVING == "Living w/ Partner") %>%
  select(IMPACT_TIME_PARTNER) %>%
  pull()
w_partner_only

# Describe
mean(w_partner_only)
sd(w_partner_only)
median(w_partner_only)
length(w_partner_only)
mean_rank

# Just participants living without a partner
w_partner_plus <- partners_time_1 %>%
  filter(LIVE_W_PARTNER == "Yes") %>%
  filter(CURRENT_LIVING != "Living w/ Partner") %>%
  select(IMPACT_TIME_PARTNER) %>%
  pull()
w_partner_plus

# Describe
mean(w_partner_plus)
sd(w_partner_plus)
median(w_partner_plus)
length(w_partner_plus)

# Wilcoxon test (aka Mann-Whitney U test)
wilcox.test(w_partner_only, w_partner_plus, alternative = "two.sided")

# NLP - PARTNER_CONTACT_QUAL ----------------------------------------------

# How have you maintained contact with your partner?
#######

# Percent of participants in serious relationships providing descriptions 
partner_qual %>%
  # Remove missing values
  filter(!is.na(PARTNER_CONTACT_QUAL)) %>%
  # Remove people who are not in a serious relationship
  filter(RELATIONSHIP_STATUS == "Serious Relationship") %>% 
  # Remove participants who answered with yes-no-na
  filter(
    !(PARTNER_CONTACT_QUAL %in% c("Yes", "Yes.", "yes", "YES", "no", "No", "No change", "N/a", "n/a"))
  ) %>%
  nrow(.) / sum(survey$RELATIONSHIP_STATUS == "Serious Relationship", na.rm = TRUE)

# Classify the routines of maintaining partner contact
p_qual_contact <- partner_qual %>%
  # Repeat the filters
  filter(!is.na(PARTNER_CONTACT_QUAL) & RELATIONSHIP_STATUS == "Serious Relationship" &
         !(PARTNER_CONTACT_QUAL %in% c("Yes", "Yes.", "yes", "YES", "no", "No", "No change", "N/a", "n/a"))) %>%
  select(PARTNER_CONTACT_TECH, PARTNER_CONTACT_QUAL) %>%
  mutate(
    # Talking and speaking assumed to have occurred face-to-face
    together = ifelse(str_detect(PARTNER_CONTACT_QUAL,
                                 regex("same house|liv.+togeth|we.+togeth|face to|speak|talking|verbal|liv.+with|in person|home|everywhere|see each|we live|contact|both in the flat|present|relax together|in the house|constantly together", ignore_case = TRUE)),
      1, 0
    ),
    text = ifelse(str_detect(PARTNER_CONTACT_QUAL,
                  regex("text", ignore_case = TRUE)),
      1, 0
    ),
    video = ifelse(str_detect(PARTNER_CONTACT_QUAL,
                              regex("video|facetim|Skype", ignore_case = TRUE)),
      1, 0
    ),
    phonecall = ifelse(str_detect(PARTNER_CONTACT_QUAL,
                                  regex("(?<!video) call|phone", ignore_case = TRUE)),
      1, 0
    ),
    social_media = ifelse(str_detect(PARTNER_CONTACT_QUAL,
                                     regex("WhatsApp|messenger|social media|snapchat|internet", ignore_case = TRUE)),
      1, 0
    ),
    # Communication patterns have remained the same
    normal = ifelse(str_detect(PARTNER_CONTACT_QUAL,
                               regex("normal|no change|usual|same (?!house)", ignore_case = TRUE)),
      1, 0
    ),
    # Technology mentioned but not specified 
    misc_tech = ifelse(str_detect(PARTNER_CONTACT_QUAL,
                                  regex("virtual|not physical|technology", ignore_case = TRUE)),
      1, 0
    ),
    # Cope with or circumvent social distancing guidelines 
    soc_dist = ifelse(str_detect(PARTNER_CONTACT_QUAL,
                                 regex("break.+rule|drive way|still see|visit|dinner|spending time|quality time", ignore_case = TRUE)),
      1, 0
    )
  )
p_qual_contact

# Create an other category
p_qual_contact_1 <- p_qual_contact %>%
  filter(
    together != 1 &
    text != 1 &
    video != 1 &
    phonecall != 1 &
    social_media != 1 &
    normal != 1 &
    misc_tech != 1 &
    soc_dist != 1
  ) %>%
  select(PARTNER_CONTACT_QUAL) %>%
  # Assign to category
  mutate(category = rep("other", nrow(.))) %>%
  count(category) %>%
  mutate(percent = (n / nrow(p_qual_contact) * 100))
p_qual_contact_1

# Calculate category frequencies
p_qual_contact %>%
  select(-starts_with("PARTNER")) %>%
  gather(key = "category", value = "occurrence") %>%
  # Drop non-occurrence
  filter(occurrence != 0) %>%
  count(category) %>%
  mutate(percent = (n / nrow(p_qual_contact) * 100)) %>%
  rbind(p_qual_contact_1) %>%
  arrange(desc(n))

# Select example categorys
example <- p_qual_contact %>%
  filter(misc_tech == 1)
View(example)

# NLP - RELATIONSHIP_TECH -------------------------------------------------

# How has technology impacted your relationship?
#######

# How many provided description
partner_qual %>%
  select(RELATIONSHIP_TECH, RELATIONSHIP_STATUS) %>%
  filter(!is.na(RELATIONSHIP_TECH) & RELATIONSHIP_STATUS == "Serious Relationship") %>%
  nrow(.)

# Classify the descriptions
p_qual_tech <- partner_qual %>%
  select(RELATIONSHIP_TECH, RELATIONSHIP_STATUS) %>%
  filter(!is.na(RELATIONSHIP_TECH) & RELATIONSHIP_STATUS == "Serious Relationship") %>%
  mutate(
    # No impact perceived
    none = ifelse(str_detect(RELATIONSHIP_TECH,
                             regex("not much|no impact|not impact|no$|no effect|not at all|haven|it hasn|hasn't$|none|has not|no change|nope|no influen|same$|don't feel like|nil|not sign|nothing sign|not massiv|not subst|didn't.+impact|imposs|don't.+impact|hasn't made|hasn't impact|not had.+impact|^hasn't|about the same", ignore_case = TRUE)),
      1, 0
    ),
    # General positive impact
    positive = ifelse(str_detect(RELATIONSHIP_TECH,
                                 regex("great|good|helped|positive|improve|very useful|helpful", ignore_case = TRUE)),
      1, 0
    ),
    # Not application for whatever reason
    not_app = ifelse(str_detect(RELATIONSHIP_TECH,
                                regex("^not app|N/A|^na", ignore_case = TRUE)),
      1, 0
    ),
    # No use of technology, either in general or due to living together---less importance
    no_use = ifelse(str_detect(RELATIONSHIP_TECH,
                               regex("don't use|unnecessary|no need|negli|live together|together all|together more|home together|liv.+together|always together|same place|same build", ignore_case = TRUE)),
      1, 0
    ),
    # Technology enables the relationship to persist despite physical distance
    continuity = ifelse(str_detect(RELATIONSHIP_TECH,
                                   regex("keep.+alive|stay in|stay.+con|ke.+con|keep.+touch|talk to|contact|in touch|together yet|allow|see.+less|watch.+together|kept things|distanc|talk.+more|chat.+more|saver|see each|more open|call.+more|chat.+often|intimacy from afar", ignore_case = TRUE)),
      1, 0
    ),
    # Participants estimate the magnitude of the impact
    est_impact = ifelse(str_detect(RELATIONSHIP_TECH,
                                   regex("somewhat|little|a lot|very much", ignore_case = TRUE)),
      1, 0
    ),
    # Necessary and easy
    depend = ifelse(str_detect(RELATIONSHIP_TECH,
                               regex("depend|eas|on it all|rely.+on|only way|survive|without tech|together without", ignore_case = TRUE)),
      1, 0
    ),
    # Technology as a distraction or source of disagreement
    dist_disa = ifelse(str_detect(RELATIONSHIP_TECH,
                                 regex("distract|calls more often|paying att|fall outs|escape|on his phone|talk.+less|speak less|less time on soc|too much time|tensio|phones more than|excuse|not present", ignore_case = TRUE)),
      1, 0
    )
  )
p_qual_tech

# Create an other category
p_qual_tech_1 <- p_qual_tech %>%
  filter(
    none != 1 &
    positive != 1 &
    not_app != 1 &
    no_use != 1 &
    continuity != 1 &
    est_impact != 1 &
    depend != 1 &
    dist_disa != 1
  ) %>%
  select(RELATIONSHIP_TECH) %>%
  mutate(category = rep("other", nrow(.))) %>%
  count(category) %>%
  mutate(percent = (n / nrow(p_qual_tech)) * 100)
p_qual_tech_1

# Calculate frequency of categorys
p_qual_tech %>%
  select(-starts_with("RELATIO")) %>%
  gather(key = "category", value = "occurrence") %>%
  filter(occurrence != 0) %>%
  count(category) %>%
  mutate(percent = (n / nrow(p_qual_tech)) * 100) %>%
  rbind(p_qual_tech_1) %>%
  arrange(desc(n))

# Select example categorys
example <- p_qual_tech %>%
  filter(depend == 1)
View(example)

# NLP - RELATIONSHIP_QUAL -------------------------------------------------

# In your own words, please explain how the current COVID-19 pandemic and social lockdown 
# has affected your intimate relationship(s), both positively and negatively:
#######

# Number who provided a narrative
partner_qual %>%
  filter(!is.na(RELATIONSHIP_QUAL) & RELATIONSHIP_STATUS == "Serious Relationship") %>%
  select(RELATIONSHIP_QUAL) %>%
  nrow(.)

# Percent who provided a narrative
339 / sum(partner_qual$RELATIONSHIP_STATUS == "Serious Relationship", na.rm = TRUE)

# Generate categories
p_qual_relation <- partner_qual %>%
  # Remove missing values and participants who are single or casually dating
  filter(!is.na(RELATIONSHIP_QUAL) & RELATIONSHIP_STATUS == "Serious Relationship") %>%
  select(ID, RELATIONSHIP_QUAL) %>%
  # Create categories from keywords
  mutate(
    # No change
    none = ifelse(str_detect(RELATIONSHIP_QUAL,
                             regex("hasn.+change|not too much|no impact|not.+change|unaff|not effe|doesnt imp|dont.+change|everything.+same|no issu|don't think.+has|don't.+change|allways|same$|it hasn't|hasn't.+affect|hasn't.+effect|no affec|no effec|no change|isnt at the|much the same|remain.+same|hasn't im|same as it would|minimal imp|same as prev", ignore_case = TRUE)),
      1, 0
    ),
    # Less "intimate," which participants used to refer to sex
    less_intim = ifelse(str_detect(RELATIONSHIP_QUAL,
                                   regex("less.+intima|intimate less|less due to|haven't.+intim|less physic|no physic|not.+physica|lack.+physic|no intim|no.+sex|less sex|reduc.+intim|unable.+intim|not (?!really).+intimate|not intimate|sleeping in diff|crave that intim|lack of energy|limits.+sex", ignore_case = TRUE)),
      1, 0
    ),
    # More time together perceived as enhancing the affective dimension 
    deeper_bond = ifelse(str_detect(RELATIONSHIP_QUAL,
                                    regex("affection|more time.+together|closer|bond|more intim|happy|rely on.+other|energy for inti|enjoy|more together|intercourse every|more sex|team|talk more|intim more|strong we are|support|isolation togeth|spend.+time|spend a lot|talk to.+longer|connect more|intimate more|seeing my partner more|more time with.+partner|each other more|move in with|live together$|more time.+each other|depth|now we are|more avail|stronger|valuable time|spend together|more quality|time to spend|see each other|now living|see more of him|living with.+now|have eachother", ignore_case = TRUE)),
      1, 0
    ),
    # Problems with boundaries or work-life balance---together/working too much
    boundary = ifelse(str_detect(RELATIONSHIP_QUAL,
                                 regex("argu|boundar|work.+balanc|tens|strain|need.+break|fight|less excit|frustrat|lack.+space|no alone|not.+time.+myself|tired|more space than|less priva|not really quality time|little boy|clean more|living with pare|rowing|lack of energy", ignore_case = TRUE)),
      1, 0
    ),
    # Less contact with partner
    less_con = ifelse(str_detect(RELATIONSHIP_QUAL,
                                 regex("see.+less|not.+see|stagnate|long.+apart|see.+in person|unable.+see|home less|missing human|disinteres|cannot do act|haven't.+seen (?!much impact)|lonely|stay in touch|miss being with|miss each other|can't.+see each other|don't.+see each other|can't live toget|cannit|stopped me from|painful.+apart", ignore_case = TRUE)),
      1, 0
    ),
    # Greater negative arousal
    neg_arousal = ifelse(str_detect(RELATIONSHIP_QUAL,
                                    regex("anxi|(?<!de-)stress|depress|more distant|worr|feel.+down|mundane|angry|prolonged periods|irrita", ignore_case = TRUE)),
      1, 0
    ),
    # Generally positive and other category
    other = ifelse(str_detect(RELATIONSHIP_QUAL,
                                     regex("positively$|50|less with friends|key worker|keyworker|disinfect and take", ignore_case = TRUE)),
      1, 0
    ),
    # Recreational coping
    rec_cope = ifelse(str_detect(RELATIONSHIP_QUAL,
                                 regex("gaming|work on me|organis|do more things|consider what I want|my son|new hobb", ignore_case = TRUE)),
      1, 0
    )
  )
p_qual_relation

# Check categorization
p_qual_relation_1 <- p_qual_relation %>%
  filter(
    none != 1 &
    less_intim != 1 &
    other != 1 &
    deeper_bond != 1 &
    boundary != 1 &
    less_con != 1 &
    neg_arousal != 1 &
    rec_cope != 1
  )
p_qual_relation_1

# Calculate category frequencies
p_qual_relation %>%
  select(-c(ID, RELATIONSHIP_QUAL)) %>%
  gather(key = "category", value = "occurrence") %>%
  filter(occurrence != 0) %>%
  count(category) %>%
  mutate(percent = (n / nrow(p_qual_relation)) * 100) %>%
  arrange(desc(n))

# Select example categorys
example <- p_qual_relation %>%
  filter(other == 1)
View(example)

# Basic demographics of block quotes
survey %>%
  select(ID, AGE, ETHNIC, GENDER) %>%
  filter(ID %in% c(551, 16, 391, 228))

# Percent who mentioned being a key worker
sum(str_detect(p_qual_relation$RELATIONSHIP_QUAL, 
               regex("key worker|keyworker|disinfect", ignore_case = TRUE))) / nrow(p_qual_relation)
