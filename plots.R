
goodale_theme <- function() {
  theme(
    # add border 1)
    # color background 2)
    panel.background = element_rect(fill = "white"),
    
    
    #  panel.grid.major.x = element_line(colour = "black", linetype = 2, size = 0.2),
    #panel.grid.minor.x = element_blank(),
    #    panel.grid.major.y =  element_line(colour = "black", linetype = 2, size = 0.2),
    # panel.grid.minor.y = element_blank(),
    #
    # modify grid 3)
    # modify text, axis and colour 4) and 5)
    axis.text = element_text(colour = "black", family = "Times New Roman", size = 10),
    axis.title = element_text(colour = "black", family = "Times New Roman", size = 12),
    # legend at the bottom 6)
    legend.position = "bottom",
    legend.title = element_text(colour = "black", family = "Times New Roman", size = 12),
    legend.text = element_text(colour = "black", family = "Times New Roman", size = 12)
  )
} 




reg2_dat = read.csv(here("data", "tidy", "reg2_data.csv"))
word_only = read.csv(here("data", "tidy", "word_only_data.csv"))


## Figure 1 - distribution of all data 
reg2_dat %>% 
  ggplot(aes(x = log_rt, y = group, fill = gender)) + geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("seagreen3", "orange")) +
  theme_minimal() + goodale_theme() + 
  ylab("Group") + xlab("Log-transformed Reaction Times") +
  scale_y_discrete(labels=c("L2_beginner" = "L2 (beginner)", 
                            "L2_advanced" = "L2 (advanced)",
                            "L1" = "L1")) 
  
# Desc stats below figure 1
reg2_dat %>% 
  group_by(group, gender) %>% 
  summarize(mean_log = mean(log_rt), sd_log = sd(log_rt))

ggsave(filename = "fig1.png", path = here("plots"), dpi = 1200)
  
# figure 2 - same as 1 but word only 

word_only %>% 
  ggplot(aes(x = log_rt, y = group, fill = gender)) + geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("seagreen3", "orange")) +
  theme_minimal() + goodale_theme() + 
  ylab("Group") + xlab("Log-transformed Reaction Times") +
  scale_y_discrete(labels=c("L2_beginner" = "L2 (beginner)", 
                            "L2_advanced" = "L2 (advanced)",
                            "L1" = "L1")) 

# Desc stats below figure 1
word_only %>% 
  group_by(group, gender) %>% 
  summarize(mean_log = mean(log_rt), sd_log = sd(log_rt))

ggsave(filename = "fig2.png", path = here("plots"), dpi = 1200)


## Figure 3 - average differences as a function of group 

sd_all_data = sd(reg2_dat$log_rt)

reg2_dat %>% 
  group_by(participant, gender, group) %>% 
  summarise(mean_log_rt = mean(log_rt)) %>% 
  pivot_wider(names_from = gender, values_from = mean_log_rt) %>% 
  mutate(Effect = they-she) %>%
  mutate(Effect_hi = Effect + sd_all_data) %>% # positive is she is slower than they, neg she is faster than they
  mutate(Effect_lo = Effect - sd_all_data) %>% 
  mutate(Effect_direction = ifelse(Effect > 0, "She is slower", "She is faster")) %>% 
  ggplot(aes(y = reorder(participant, -Effect), x = Effect, xmin = Effect_lo, xmax = Effect_hi, color = Effect_direction)) + 
  geom_pointrange() +
  theme(
    axis.text = element_text(colour = "black", family = "Times New Roman", size = 10),
    axis.title = element_text(colour = "black", family = "Times New Roman", size = 12),
    # legend at the bottom 6)
    legend.position = "bottom",
    legend.title = element_text(colour = "black", family = "Times New Roman", size = 12),
    legend.text = element_text(colour = "black", family = "Times New Roman", size = 12)
  ) +
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank()) + xlab("Effect: Difference in log-rt") +
  ylab("Participant") + facet_wrap(~group)

ggsave(filename = "fig3.png", path = here("plots"), dpi = 1200)


## Figure 4 - average differences as a function of individual 

reg2_dat %>% 
  group_by(participant, gender, group) %>% 
  summarise(mean_log_rt = mean(log_rt)) %>% 
  pivot_wider(names_from = gender, values_from = mean_log_rt) %>% 
  mutate(Effect = they-she) %>%
  mutate(Effect_hi = Effect + sd_all_data) %>% # positive is she is slower than they, neg she is faster than they
  mutate(Effect_lo = Effect - sd_all_data) %>% 
  mutate(Effect_direction = ifelse(Effect > 0, "She is slower", "She is faster")) %>% 
  ggplot(aes(y = reorder(participant, -Effect), x = Effect, xmin = Effect_lo, xmax = Effect_hi, color = Effect_direction)) + 
  geom_pointrange() +
  theme(
    axis.text = element_text(colour = "black", family = "Times New Roman", size = 10),
    axis.title = element_text(colour = "black", family = "Times New Roman", size = 12),
    # legend at the bottom 6)
    legend.position = "bottom",
    legend.title = element_text(colour = "black", family = "Times New Roman", size = 12),
    legend.text = element_text(colour = "black", family = "Times New Roman", size = 12)
  ) +
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank()) + xlab("Effect: Difference in log-rt") +
  ylab("Participant") 

ggsave(filename = "fig4.png", path = here("plots"), dpi = 1200)

## Figure 4 - survey data vs effects

