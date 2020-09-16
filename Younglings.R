kids <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-15/kids.csv')

setwd("C:/Users/Nutzer/Desktop/MSc/R/Projects/TidyTuesday")

library(tidyverse)
library(extrafont)
library(cowplot)

### US as a whole?
kids.total.clean <- kids %>% 
  select(-variable, -raw) %>% 
  group_by(year) %>%
  summarise(total_sum = sum(inf_adj))
  


### total US spending per state over the years - line
kids.states.clean <- kids %>% 
  select(-variable, -raw) %>% 
  group_by(year) %>%
  summarise(state = sum(inf_adj))



## total per per year 

ggplot(kids.total.clean, aes(x = year, y = total_sum)) +
  geom_line() +
  theme_minimal() +
  labs(title = "XXX",
       subtitle = "Total US Expenditure",
       caption = "@ABarroso_BA | Source: tidykids R Package | #TidyTuesday Week 38") +
  theme(plot.margin = margin(15,20,30,20),
        legend.position = "bottom",
        legend.margin = margin(20, 0, 10, 0),
        legend.title = element_blank(),
        axis.title.x = element_text("Year"),
        axis.title.y = element_text("Expenditure in USD"),
        plot.background = element_rect(fill = "#F1F8E0"),
        plot.title = element_text(size = 28, face ="bold", hjust = 0.50, margin = margin(20, 0, 10, 0)),
        plot.subtitle = element_text(size = 16, hjust = 0.5, margin = margin(0, 0, 30, 0)),
        plot.caption = element_text(size = 8, hjust = 0.5, vjust = -5))



## do pie charts to see which state makes up the most %??





