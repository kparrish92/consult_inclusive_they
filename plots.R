
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

## Figure 1 - distribution of all data 
reg2_dat %>% 
  ggplot(aes(x = log_rt, y = group, fill = gender)) + geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("seagreen3", "orange")) +
  theme_minimal() + goodale_theme() + 
  ylab("Group") + xlab("Log-transformed Reaction Times") +
  scale_y_discrete(labels=c("L2_beginner" = "L2 (beginner)", 
                            "L2_advanced" = "L2 (advanced)",
                            "L1" = "L1")) 
  
  
ggsave(filename = "fig1.png", path = here("plots"), dpi = 1200)
  
## Figure 2 - average differences as a function of group 

## Figure 3 - average differences as a function of individual 