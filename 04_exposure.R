## A script to create a continuous variable for exposure 

library(here)
library(tidyverse)

l2_grp = read.csv(here("data", "tidy", "l2_survey_tidy.csv"))
l1_grp = read.csv(here("data", "tidy", "l1_survey_tidy.csv"))
