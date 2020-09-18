library(tidyverse)
library(extrafont)
library(cowplot)
library(scales)
library(ggtext)
library(here)

kids <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-15/kids.csv')


kids.total <- kids %>%
  filter(variable == "socsec") %>%
  group_by(state) %>%
  select(-raw, -inf_adj) %>%
  summarise(total = sum(inf_adj_perchild))


p <- ggplot(kids.total, aes(x = total, y = state)) +
  geom_point(colour = "#AC7B84", size = 2) +
  geom_vline(xintercept = 5.405788, colour = "grey55")  +
  geom_vline(xintercept = 6.427477, colour = "grey65", alpha = 0.5)  +
  geom_vline(xintercept = 4.384098, colour = "grey65", alpha = 0.5) +
  scale_x_continuous(labels=scales::dollar_format())+
  coord_flip(clip = "off") +
  theme_minimal() +
  geom_label(label="Average spending \n on children's \n social security \n is 1.23*x higher \n for southern states",
             x = 7.5, y = 55.65, label.padding = unit(0.3, "lines"),
             label.size = 0.05, color = "#AC7B84", fill = "#F8F6F1", size = 2.5) +
  labs(title = "State Spending on Social Security",
       subtitle = "Total expenditure per child by American state (1997-2016)",
       x = "Expenditure in USD", y = "State",
       caption = "*Figures have been rounded to two decimal points        \n @ABarroso_BA | Source: tidykids R Package | #TidyTuesday Week 38") +
  annotate("text", x = 7.84, y = 3.7, size = 2, colour = "#AC7B84", label = "Alabama: $7.84*") +
  annotate("text", x = 7.88, y = 28, size = 2, colour = "#AC7B84", label = "Mississippi: $7.88*") +
  annotate("text", x = 8, y = 45.6, size = 2, colour = "#AC7B84", label = "West Virginia: $7.88*") +
  theme(plot.margin = margin(30, 85, 0, 30),
        panel.grid.major = element_line(size = 0.3, colour = "#EFE6E8"),
        panel.grid.minor = element_line(size = 0.3, colour = "#EFE6E8"),
        axis.text.x = element_text(family = "AdobeFanHeitiStd", size = 8, angle = 70, vjust = 1, hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_text(family = "AdobeFanHeitiStd-Bold", colour = "grey35",
                                    face = "bold", size = 9, margin = margin(0, 20, 0, 0)),
        plot.background = element_rect(fill = "#F8F6F1"),
        plot.title = element_text(family = "Britannic Bold", colour = "#AC7B84",
                                  size = 31, hjust = -0.5, margin = margin(5, 20, 9, 0)),
        plot.subtitle = element_text(family = "Britannic Bold", colour = "grey35",
                                     size = 11, margin = margin(0, 0, 30, 0)),
        plot.caption = element_text(colour = "grey30", family = "AdobeFanHeitiStd-Bold",
                                    size = 8, hjust = 1.30, vjust = -1, margin = margin(10, 0, 10, 0)))


ggdraw() +
  draw_plot(p) +
  draw_label("Max. standard deviation", x = 0.925, y = 0.556, colour = "grey65", size = 6) +
  draw_label("Mean average spending", x = 0.925, y = 0.445, colour = "grey35", size = 6) +
  draw_label("Min. standard deviation", x = 0.925, y = 0.332, colour = "grey65", size = 6)


ggsave("Images/Week38_Kids.tiff", plot = last_plot(), dpi = 400, width = 220, height = 160, units = "mm")






# Mean spending of southern states: 6.6588874
# mean spending of all: 5.405788
# Southern states spend 1.2318069817018 times more


