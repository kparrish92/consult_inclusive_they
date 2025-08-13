
ef = read.csv(here("data", "tidy", "l2_survey_tidy.csv"), na.strings = NA) %>% 
  filter(participant != "Prueba1") %>% 
  filter(participant != "Prueba2") %>% 
  select(-X) %>% 
  select(-she, -they, -participant)

ef[ef == ""] <- 0
ef[is.na(ef)] <- 0

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


glimpse(ef)

unique(ef$x14_if_you_answered_yes_to_the_previous_question_what_percentage_of_that_content_takes_place_on_a_daily_basis_content)
unique(ef$x16_if_you_anwered_yes_to_the_previous_question_what_percentage_of_that_content_takes_place_on_a_daily_basis_content)
unique(ef$x18_if_you_answered_yes_to_the_previous_questions_what_percentage_of_that_content_takes_place_on_a_daily_basis_content)
unique(ef$x20_si_la_respuesta_a_la_pregunta_anterior_es_si_que_porcentaje_de_ese_contenido_sucede_diariamente_contenido)
unique(ef$x22_if_you_answered_yes_to_the_previous_question_how_much_time_a_day_do_you_devote_to_that_activity)
unique(ef$x23_when_you_listen_to_or_read_social_media_networks_in_english_how_much_of_that_daily_content_has_to_do_with_inclusive_language_in_english_content)
unique(ef$x24_when_you_listen_to_or_read_social_media_networks_in_english_how_much_of_that_content_has_to_do_with_neutral_language_in_english_on_a_daily_basis_content)
unique(ef$x25_if_you_answered_yes_to_the_previous_question_how_much_time_a_day_do_you_devote_to_that_activity)
unique(ef$x26_when_you_create_social_media_content_in_english_how_much_of_that_content_has_to_do_with_inclusive_language_in_english_content)

colnames(ef) = substr(colnames(ef) , start = 1, stop = 6)
## Variable importance
rf_l2 <- ranger(effect ~ ., data = ef, importance = "impurity")

rf_l2$variable.importance

ef$x14_if

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

