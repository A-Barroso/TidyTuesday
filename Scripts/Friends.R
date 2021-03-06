library(tidyverse)
library(extrafont)
library(cowplot)
library(here)

friends_emotions <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends_emotions.csv')

friends.clean <- friends_emotions %>%
  select(-episode, -scene, -utterance) %>%
  mutate(
    season = case_when(
      season == "1" ~ "Season 1",
      season == "2" ~ "Season 2",
      season == "3" ~ "Season 3",
      season == "4" ~ "Season 4")
  ) %>%
  group_by(season) %>%
  count(emotion)



### IMDb Ratings

friends_info <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends_info.csv')

f_info.clean <- friends_info %>%
  select(season, imdb_rating) %>%
  group_by(season)

rate_s1 <- f_info.clean[1:24,]
rate_s2 <- f_info.clean[25:48,]
rate_s3 <- f_info.clean[49:73,]
rate_s4 <- f_info.clean[74:97,]

median(rate_s1$imdb_rating)
median(rate_s2$imdb_rating)
median(rate_s3$imdb_rating)
median(rate_s4$imdb_rating)



font_import(path = "C:/Users/Nutzer/AppData/Local/Microsoft/Windows/Fonts")
loadfonts(quiet=TRUE)


img <- "https://pbs.twimg.com/media/EhfwFiEWAAIVEsL?format=png&name=4096x4096"

friends.clean$emotion <- as.factor(friends.clean$emotion)
friends.clean$emotion <- factor(friends.clean$emotion, levels = c("Joyful",
                                                                    "Mad",
                                                                    "Peaceful",
                                                                    "Sad",
                                                                    "Powerful",
                                                                    "Scared",
                                                                    "Neutral"))

p <- ggplot(friends.clean, aes(emotion, n, fill = emotion)) +
  geom_col() +
  coord_polar(theta = "y", clip = "off") +
  theme_minimal() +
  facet_wrap(~ season, nrow = 1) +
  labs(title = "Friend : An Emotional Rollercoaster",
       subtitle = "Aggregated emotional values per season",
       caption = "@ABarroso_BA | Source: friends R Package | #TidyTuesday Week 37") +
  scale_fill_manual(values = c("#F74035", "#9787CD", "#3F9DD4", "#008F48", "#F6D400", "#DF4013", "#941205")) +
  guides(fill = guide_legend(nrow = 1, override.aes = list(size = 2))) +
  theme(text = element_text(family = "GabrielWeissFriendsFont"),
        plot.margin = margin(20,20,30,20),
        legend.position = "bottom",
        legend.margin = margin(25, 0, 15, 0),
        legend.title = element_blank(),
        legend.text = element_text(colour = "grey97"),
        strip.text = element_text(colour = 'grey97'),
        panel.grid.major = element_line(size = 0.3, colour = "grey10"),
        panel.grid.minor = element_line(size = 0.3, colour = "grey10"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.background = element_rect(fill = "#000000"),
        plot.title = element_text(family = "Gabriel Weiss' Friends Font", size = 28, face ="bold", colour = "grey97", hjust = 0.90, vjust = 2, margin = margin(20, 0, 10, 0)),
        plot.subtitle = element_text(size = 16, colour = "grey97", hjust = 0.5, margin = margin(0, 0, 30, 0)),
        plot.caption = element_text(size = 8, colour = "grey97", hjust = 0.5, vjust = 2))


ggdraw() +
  draw_plot(p) +
  draw_image(img, scale=0.30, y=0.325, x=-0.305) +
  draw_label("Mean IMDb Rating: 8.20", x= 0.145, y = 0.271, colour = "white", size = 10) +
  draw_label("Mean IMDb Rating: 8.45", x= 0.38, y = 0.271, colour = "white", size = 10) +
  draw_label("Mean IMDb Rating: 8.30", x= 0.62, y = 0.271, colour = "white", size = 10) +
  draw_label("Mean IMDb Rating: 8.50", x= 0.855, y = 0.271, colour = "white", size = 10)


ggsave("Images/Week37_Friends.tiff", plot = last_plot(), dpi = 400, height = 180, width = 280, units = "mm")








