library(tidyverse)
library(here)

ef = read.csv(here("data", "tidy", "l2_survey_tidy.csv"), na.strings = NA) %>% 
  filter(participant != "Prueba1") %>% 
  filter(participant != "Prueba2") %>% 
  select(-X) %>% 
  select(-she, -they)

ef_l1 = read.csv(here("data", "tidy", "l1_survey_tidy.csv"), na.strings = NA) %>% 
  filter(participant != "Prueba1") %>% 
  filter(participant != "Prueba2") %>% 
  select(-X) %>% 
  select(-she, -they)

ef[ef == ""] <- 0
ef[is.na(ef)] <- 0

ef_l1[ef_l1 == ""] <- 0
ef_l1[is.na(ef_l1)] <- 0

# replace spanish with english for consitency 
ef[ef == "un 10%"] <- "10%"
ef[ef == "un 25%"] <- "25%"
ef[ef == "un 50 %"] <- "50%"
ef[ef == "un 75%"] <- "75%"
ef[ef == "100 %"] <- "100%"

ef[ef == "entre dos y 3"] <- "between 2 or 3 hours"
ef[ef == "más de 4"] <- "over 4 hours"
ef[ef == "media hora"] <- "half an hour"
ef[ef == "entre 1 y dos horas"] <- "between 1 or 2 hours"
ef[ef == "nada"] <- "none"
ef[ef == "none"] <- "0"


ef_l1[ef_l1 == "un 10%"] <- "10%"
ef_l1[ef_l1 == "un 25%"] <- "25%"
ef_l1[ef_l1 == "un 50 %"] <- "50%"
ef_l1[ef_l1 == "un 75%"] <- "75%"
ef_l1[ef_l1 == "100 %"] <- "100%"

ef_l1[ef_l1 == "entre dos y 3"] <- "between 2 or 3 hours"
ef_l1[ef_l1 == "más de 4"] <- "over 4 hours"
ef_l1[ef_l1 == "media hora"] <- "half an hour"
ef_l1[ef_l1 == "entre 1 y dos horas"] <- "between 1 or 2 hours"
ef_l1[ef_l1 == "nada"] <- "none"
ef_l1[ef_l1 == "none"] <- "0"






  

ef_long = ef %>% 
  pivot_longer(cols = 4:12, names_to = "question", values_to = "value") %>% 
  mutate(exposure_score = case_when(
    value == "0" ~ 0,
    value == "10%" ~ .1,
    value == "25%" ~ .25,
    value == "50%" ~ .5,
    value == "75%" ~ .75,
    value == "100%" ~ 1,
    value == "half an hour" ~ .25, 
    value == "between 1 or 2 hours" ~ .5,
    value == "between 2 or 3 hours" ~ .75,
    value == "over 4 hours" ~ 1
  )) %>% group_by(participant, group) %>% summarise(composite_score = sum(exposure_score))


ef_long %>% 
  group_by(group) %>% 
  summarize(mean_c = mean(composite_score), sd_c = sd(composite_score))


ef_long_l1 %>% 
  group_by(group) %>% 
  summarize(mean_c = mean(composite_score), sd_c = sd(composite_score))

ef_long_l1 = ef_l1 %>% 
  select(-x24_do_you_make_content_videos_podcasts_tik_tok_etc_on_social_networks) %>% 
  pivot_longer(cols = 4:13, names_to = "question", values_to = "value") %>% 
  mutate(exposure_score = case_when(
    value == "0" ~ 0,
    value == "0%" ~ 0,
    value == "10%" ~ .1,
    value == "25%" ~ .25,
    value == "50 %" ~ .5,
    value == "75%" ~ .75,
    value == "100%" ~ 1,
    value == "around 30 minutes" ~ .25, 
    value == "between 1-2 hours" ~ .5,
    value == "between 2-3 hours" ~ .75,
    value == "more than 4 hours" ~ 1
  )) %>% group_by(participant, group) %>% summarise(composite_score = sum(exposure_score))

comp_score_df = rbind(ef_long_l1,ef_long)

comp_score_df %>% 
  write.csv(here("data", "tidy", "comp_exposure_scores.csv"))

## survey question models 

long_ef = ef %>% 
  pivot_longer(cols = 4:12, names_to = "question", values_to = "answer")

i = 9
qs = unique(long_ef$question)

df = long_ef %>% 
  filter(question == qs[3] | question == qs[9])

model = lm(effect ~ answer, data = df)

summary(model)

# half an hour .25
# between 1 or 2 hours .5
# between 2 or 3 hours .75
# over 4 hours 1

# 10 .1
# 25 .25
# 50 .5
# 75 .75
# 1 1


long_ef_l1 %>% 
  ggplot(aes(x = effect, y = answer)) + geom_boxplot() + 
  facet_wrap(~question)

long_ef_l1 = ef_l1 %>% 
  pivot_longer(cols = 4:14, names_to = "question", values_to = "answer")

it = 11
qs_l1 = unique(long_ef_l1$question)

df_l1 = long_ef_l1 %>% 
  filter(question == qs_l1[it])

model_l1 = lm(effect ~ answer, data = df_l1)

summary(model_l1)


ef %>% 
  ggplot(aes(x = effect, y = x24_wh)) + geom_boxplot()


ef %>% 
  ggplot(aes(x = effect, y = x14_if)) + geom_boxplot()

ef %>% 
  ggplot(aes(x = effect, y = x16_if)) + geom_boxplot()

ef %>% 
  ggplot(aes(x = effect, y = x18_if)) + geom_boxplot()

ef %>% 
  ggplot(aes(x = effect, y = x20_si)) + geom_boxplot()


ef %>% 
  ggplot(aes(x = effect, y = x22_if)) + geom_boxplot()


ef %>% 
  ggplot(aes(x = effect, y = group)) + geom_boxplot()

