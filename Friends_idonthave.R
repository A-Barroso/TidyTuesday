setwd("C:/Users/Nutzer/Desktop/MSc/R/Projects/TidyTuesday")

install.packages("friends")
install.packages("tidytuesdayR")

library(tidyverse)
library("ggplot2")

### alt way:
## tuesdata <- tidytuesdayR::tt_load(2020, week = 37)
## friends <- tuesdata$friends

friends <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends.csv')
friends_emotions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends_emotions.csv')
friends_info <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends_info.csv')

Emo_S1 <- friends_emotions[1:2892,]

S1E1_emo <- Emo_S1[1:101,]

## the below is fucking wrooooong need different type of plot
ggplot(data = S1E1_emo, mapping = aes(x = scene, y = emotion)) +
  geom_bin2d() +
    theme_light() +
    labs(x = "Scene", y = "Emotions", title = "Emotional mapping of scenes in Episode 1 (S1)", caption = "xana is a noob")
  









######## OK FUCK IT NEW PLAN ########## EDIT THIS SHIT BELOW SO IT'S FRIENDS

library(extrafont)
library(here)
library(tidytext)
library(magick)

install.packages(c("cluster.datasets"), dependencies = TRUE)
library(cluster.datasets)


emo_words <- data.frame(word = c("Joyful",
                                 "Peaceful",
                                 "Neutral",
                                 "Sad",
                                 "Mad",
                                 "Scared",
                                 "Powerful"))

friends.clean <- friends_emotions %>% 
  select(episode, season) %>%
  unnest_tokens(word,episode) %>%
  mutate(cut(season, breaks=c(-Inf, 10, 20, 30, 40), labels=c("Season 1", "Season 2", "Season 3", "Season 4"))) %>%
  count(word, category, sort = TRUE) %>%
  anti_join(friends_emotions) %>%
  anti_join(emo_words) %>%
  arrange(desc(n)) %>%
  group_by(category) %>%
  filter(row_number() <= 5) %>%
  ungroup()

logo_raw <- image_read("R/chopped.png")
logo <- as.raster(logo_raw)

ggplot(chopped.clean, aes(n, reorder(word, n))) +
  facet_wrap(~category, scales="free", nrow = 1) +
  geom_col(fill = "#F75821", width = 0.75) + 
  xlim(0, 40) +
  labs(title = "What Makes A Good Dessert?",
       subtitle = "The most common ingredients in meals in the Chopped TV Series", 
       caption = "@CSHoggard | Data from Kaggle | #TidyTuesday Week 35",
       x = "Number of dishes",
       y = "") +
  theme_minimal() +
  theme(plot.margin = unit(c(1, 1, 0.5, 1), "cm"),
        text = element_text(family = "Open Sans"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 24, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12, face = "bold", colour = "#F75821", margin = margin(3, 2, 7, 2)),
        plot.caption = element_text(size = 9, colour = "grey40"),
        strip.text.x = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 8, colour = "grey40"),
        axis.title.x = element_text(size = 9, colour = "grey50", margin = margin(10, 0, 5, 0)))
grid.raster(logo, x = 0.5, y = 0.5)
