library(tidyverse)
library(purrr)
library(fs)
library(here)

## function to move 


tidy_spr = function(data)
{  
  df = data %>% 
    filter(!is.na(gender)) %>% 
    filter(experiment == "inclusive_gender_they") %>% 
    select(participant, sentence, experiment, gender,
           word1.rt, word2.rt, word3.rt, word4.rt, word5.rt, word6.rt, word7.rt, word8.rt, 
           word9.rt) %>% 
    pivot_longer(cols = c(5:13), names_to = "word_position", values_to = "rt") %>% 
    mutate(rt = as.numeric(rt)) %>% 
    filter(rt > .2 & rt < 2) %>% 
    mutate(log_rt = log(rt)) %>% 
    mutate(region = case_when(
      word_position == "word1.rt" ~ "region_1",
      word_position == "word2.rt" ~ "region_1",
      word_position == "word3.rt" ~ "region_1",
      word_position == "word4.rt" ~ "region_1",
      word_position == "word5.rt" ~ "region_2",
      word_position == "word6.rt" ~ "region_2",
      word_position == "word7.rt" ~ "region_2",
      word_position == "word8.rt" ~ "region_2",
      word_position == "word9.rt" ~ "region_3"
    ))
  return(df)
}


## Tidy starts here 


l1_data = dir_ls(here("data", "spr_data_l1"), regexp = "\\.csv$") %>% 
  map_dfr(read_csv, .id = "source", col_types = cols(.default = "c")) %>% 
  tidy_spr() %>% 
  mutate(group = "L1")

l2_data_a = dir_ls(here("data", "spr_data_l2", "ADVANCED"), regexp = "\\.csv$") %>% 
  map_dfr(read_csv, .id = "source", col_types = cols(.default = "c")) %>% 
  mutate(proficiency = "advanced") %>% 
  tidy_spr() %>% 
  mutate(group = "L2_advanced")

l2_data_b = dir_ls(here("data", "spr_data_l2", "BEGINNERS"), regexp = "\\.csv$") %>% 
  map_dfr(read_csv, .id = "source", col_types = cols(.default = "c")) %>% 
  mutate(proficiency = "beginner") %>% 
  tidy_spr() %>% 
  mutate(group = "L2_beginner")

tidy_spr = rbind(l1_data,l2_data_a, l2_data_b)

l1_data %>% 
  ggplot(aes(x = rt, y = region, fill = gender)) + geom_boxplot()

l2_data_a %>% 
  ggplot(aes(x = rt, y = region, fill = gender)) + geom_boxplot()

l2_data_b %>% 
  ggplot(aes(x = rt, y = region, fill = gender)) + geom_boxplot()

tidy_spr = rbind(l1_data,l2_data_a, l2_data_b)

reg2_dat = tidy_spr %>% 
  filter(region == "region_2") %>% 
  write


bayes_mod = brms::brm(log_rt ~ gender*group + 
                        (gender | participant), data = reg2_dat,
                      file = here("models", "bayesmod.rds"))


brms::conditional_effects(bayes_mod)

summary(bayes_mod)

posterior<- as.matrix(bayes_mod)

plot_title <- ggtitle("Posterior distributions",
                      "with medians and 80% intervals")

library(bayesplot)
mcmc_areas(posterior,
           pars = c("b_genderthey", 
                    "b_groupL2_advanced",
                    "b_groupL2_beginner",
                    "b_genderthey:groupL2_advanced",
                    "b_genderthey:groupL2_beginner"),
           prob = 0.8) + plot_title + theme_minimal()
