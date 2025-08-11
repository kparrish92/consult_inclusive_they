library(ranger)
## Classification forest with default settings
reg2_dat = read.csv(here("data", "tidy", "reg2_data.csv"))

## 14, 16, 18, 20, 22, 23, 24, 25, 26, 29 and 32

eng_l1 = read.csv(here("data", "surveys", "l1_eng_survey.csv")) %>% 
  janitor::clean_names() %>% 
  select(x1_participant_s_code, 
         x14_if_the_previous_answer_is_yes_what_percentage_of_language_includes_gender_neutral_language_daily_content,
         x16_if_the_previous_answer_is_yes_what_percentage_of_language_includes_inclusive_gender_language_daily_content,
         x18_if_the_previous_answer_is_yes_what_percentage_of_that_content_happens_daily_contenido,
         x20_if_the_previous_answer_is_yes_what_percentage_of_that_content_happens_daily_contenido,
         x22_if_the_previous_answer_is_yes_what_percentage_of_that_content_happens_daily,
         x23_how_much_of_that_content_includes_neutral_gender_language_daily_when_you_listen_to_or_read_social_media_content,
         x24_do_you_make_content_videos_podcasts_tik_tok_etc_on_social_networks,
         x25_if_the_previous_answer_is_yes_what_percentage_of_that_content_happens_daily,
         x26_when_you_make_social_media_content_how_much_does_that_content_have_to_do_with_inclusive_gender_language_content,
         x29_if_your_answer_to_question_28_is_yes_what_percentage_of_that_content_happens_daily_content,
         x32_if_your_answer_to_question_30_is_yes_what_percentage_of_that_content_happens_daily_contenido)

eng_l2 = read.csv(here("data", "surveys", "l2_eng_survey.csv")) %>% 
  janitor::clean_names() %>% 
  select(x1_participant_s_code, 
         x14_if_you_answered_yes_to_the_previous_question_what_percentage_of_that_content_takes_place_on_a_daily_basis_content,
         x16_if_you_anwered_yes_to_the_previous_question_what_percentage_of_that_content_takes_place_on_a_daily_basis_content,
         x18_if_you_answered_yes_to_the_previous_questions_what_percentage_of_that_content_takes_place_on_a_daily_basis_content,
         x20_si_la_respuesta_a_la_pregunta_anterior_es_si_que_porcentaje_de_ese_contenido_sucede_diariamente_contenido,
         x22_if_you_answered_yes_to_the_previous_question_how_much_time_a_day_do_you_devote_to_that_activity,
         x23_when_you_listen_to_or_read_social_media_networks_in_english_how_much_of_that_daily_content_has_to_do_with_inclusive_language_in_english_content,
         x24_when_you_listen_to_or_read_social_media_networks_in_english_how_much_of_that_content_has_to_do_with_neutral_language_in_english_on_a_daily_basis_content,
         x25_if_you_answered_yes_to_the_previous_question_how_much_time_a_day_do_you_devote_to_that_activity,
         x26_when_you_create_social_media_content_in_english_how_much_of_that_content_has_to_do_with_inclusive_language_in_english_content) %>% 
  rename("participant" = x1_participant_s_code)

rf_df = reg2_dat %>% # negative means she was slower than they - the effect is the slow down gonig from she to they
  group_by(participant, gender,group) %>% 
  summarize(mean_lrt = mean(log_rt)) %>% 
  pivot_wider(names_from = gender, values_from = mean_lrt) %>% 
  mutate(effect = she - they) %>% 
  left_join(eng_l2, by = "participant") %>% 
  filter(group != "L1") %>% 
  write.csv(here("data", "tidy", "l2_survey_tidy.csv"))

reg2_dat %>% 
  ggplot(aes(x = log_rt, y = group, fill = gender)) + geom_boxplot() + theme_minimal()




ranger(effect ~ ., data = rf_df)

## Prediction
train.idx <- sample(nrow(rf_df), 2/3 * nrow(rf_df))
iris.train <- rf_df[train.idx, ]
iris.test <- rf_df[-train.idx, ]
rg.iris <- ranger(effect ~ ., data = iris.train)
pred.iris <- predict(rg.iris, data = iris.test)
table(iris.test$effect, pred.iris$predictions)

## Variable importance
rg.iris <- ranger(Species ~ ., data = iris, importance = "impurity")
rg.iris$variable.importance




ranger(effect ~ ., data = rf_df, importance = "impurity")









reg2_dat %>% # negative means she was slower than they - the effect is the slow down gonig from she to they
  group_by(participant, gender,group) %>% 
  summarize(mean_lrt = mean(log_rt)) %>% 
  pivot_wider(names_from = gender, values_from = mean_lrt) %>% 
  mutate(effect = she - they) %>% 
  ggplot(aes(y = participant, x = effect, color = group)) + geom_point()


reg2_dat %>% 
  group_by(participant, gender) %>% 
  summarise(n = n())


reg2_dat %>% 
  filter(participant == "ADV_EN_16")


