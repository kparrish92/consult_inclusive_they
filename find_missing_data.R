### how much data was removed? 


tidy_spr_all_data = function(data)
{  
  df = data %>% 
    filter(!is.na(gender)) %>% 
    filter(experiment == "inclusive_gender_they") %>% 
    select(participant, sentence, experiment, gender,
           word1.rt, word2.rt, word3.rt, word4.rt, word5.rt, word6.rt, word7.rt, word8.rt, 
           word9.rt) %>% 
    pivot_longer(cols = c(5:13), names_to = "word_position", values_to = "rt") %>% 
    mutate(rt = as.numeric(rt)) %>% 
 #   filter(rt > .2 & rt < 2) %>% 
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

n2 = dir_ls(here("data", "spr_data_l1"), regexp = "\\.csv$") %>% 
  map_dfr(read_csv, .id = "source", col_types = cols(.default = "c")) %>% 
  tidy_spr_all_data() 

n1 = dir_ls(here("data", "spr_data_l2", "ADVANCED"), regexp = "\\.csv$") %>% 
  map_dfr(read_csv, .id = "source", col_types = cols(.default = "c")) %>% 
  tidy_spr_all_data() 

n3 = dir_ls(here("data", "spr_data_l2", "BEGINNERS"), regexp = "\\.csv$") %>% 
  map_dfr(read_csv, .id = "source", col_types = cols(.default = "c")) %>% 
  tidy_spr_all_data() 

nrow(n2) + nrow(n1) + nrow(n3)


check = n3 %>% 
  group_by(participant) %>% 
  summarise(n = n())
  
11808/144
  
tidy_spr %>% 
  group_by(region) %>% 
  summarize(n = n())
