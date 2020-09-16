setwd("C:/Users/Nutzer/Desktop/MSc/R/Projects/TidyTuesday")

library(tidyverse)
library(extrafont)
library(cowplot)
library(scales)
library(ggtext)

kids <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-15/kids.csv')


kids %>%
  filter(variable == "parkrec") %>%
  group_by(state) %>%
  select(-raw, -inf_adj_perchild) %>%
  summarise(ppm = sum(inf_adj)/1000000) %>%
  ggplot(aes(x = ppm, y = state)) +
  geom_point(colour = "#663F46") +
  scale_x_continuous(labels = comma) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Our Future's Biggest Investors",
       subtitle = "Total Expenditure on children per American state, 1997-2016.",
       x ="Expenditure (per $1,000,000)", y = "State",
       caption = "@ABarroso_BA | Source: tidykids R Package | #TidyTuesday Week 38") +
  theme(plot.margin = margin(30,30,15,30),
        panel.grid.major = element_line(size = 0.3, colour = "grey87"),
        panel.grid.minor = element_line(size = 0.3, colour = "grey87"),
        axis.text.x = element_text(size = 9, angle = 70, vjust = 1, hjust=1),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 8, margin = margin(0, 20, 0, 0)),
        plot.background = element_rect(fill = "grey97"),
        plot.title = element_text(colour = "#663F46", size = 36, face ="bold", margin = margin(20, 0, 10, 0)),
        plot.subtitle = element_text(colour = "grey30", size = 10, face = "bold", margin = margin(0, 0, 30, 0)),
        plot.caption = element_text(family = "Open Sans", size = 8, hjust = 1, vjust = -6))

# scale_x_continuous(breaks = as.numeric(levels(df$Year))[c(TRUE, rep(FALSE, 19))])


# ggplot(col="") adds legend to plot --> manually change under theme?

## DO:
# fonts
# ytext spacing
# colour palette
# point col
# xy text back --> #663F46




