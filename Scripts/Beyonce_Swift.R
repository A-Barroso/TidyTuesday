library(tidyverse)
library(tidytext)
library(stopwords)
library(ggwordcloud)
library(patchwork)
library(here)
library(extrafont)
library(cowplot)

beyonce_lyrics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/beyonce_lyrics.csv')
taylor_swift_lyrics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/taylor_swift_lyrics.csv')


t.words <- taylor_swift_lyrics %>%
  select(artist = Artist, lyrics = Lyrics) %>%
  unnest_tokens(word, lyrics) %>%
  filter(!(word %in% stopwords(source = "stopwords-iso")))

b.words <- beyonce_lyrics %>%
  select(artist = artist_name, lyrics = line) %>%
  unnest_tokens(word, lyrics) %>%
  filter(!(word %in% stopwords(source = "stopwords-iso")))

t.clean <- t.words[-grep("\\b\\d+\\b", t.words$word),]
b.clean <- b.words[-grep("\\b\\d+\\b", b.words$word),]


t1 <- t.clean %>%
  count(word, sort = TRUE) %>%
  top_n(100) %>%
  arrange(desc(n))

b1 <- b.clean %>%
  count(word, sort = TRUE) %>%
  top_n(100) %>%
  arrange(desc(n))


p1 <- ggplot(t1, aes(label = word, size = n, colour = n)) + 
  geom_text_wordcloud_area(shape = "star", eccentricity = 1) +
  scale_size_area(max_size = 20) +
  theme_minimal() +
  panel_border(remove = TRUE) +
  scale_color_gradient(low = "orange", high = "#870058") +
  labs(title = "Quantity o",
       subtitle = "Taylor Swift",
       caption = "@ABarroso_BA | Source: Rosie Baillie and Dr. Sara Stoudt | #TidyTuesday Week 40") +
  theme(plot.margin = margin(20, 20, 20, 20),
        panel.border = element_blank(),
        plot.background = element_rect(fill = "#FFFEEB"),
        plot.title = element_text(family = "Forte", colour = "#870058",
                                  size = 52, hjust = 1.1, margin = margin(0, 0, 20, 0)),
        plot.subtitle = element_text(family = "Forte", colour = "orange",
                                     size = 40, hjust = 0.5, margin = margin(0, 0, 20, 0)),
        plot.caption = element_text(colour = "orange", family = "Open Sans", face = "bold",
                                    size = 9, hjust = -0.15, vjust = -13, margin = margin(10, 0, 0, 0)))
        

p2 <- ggplot(b1, aes(label = word, size = n, colour = n)) + 
  geom_text_wordcloud_area(shape = "star", eccentricity = 1) +
  scale_size_area(max_size = 20) +
  theme_minimal() +
  panel_border(remove = TRUE) +
  scale_color_gradient(low = "orange", high = "#870058") +
  labs(title = "r Quality?",
       subtitle = "Beyoncé",
       caption = "Size proportional to frequency of use.\n Taylor Swift uses a larger array of words,\n while Beyonce uses the same words more\n often. Both share many in common.") +
  theme(plot.margin = margin(20, 20, 20, 20),
        panel.border = element_blank(),
        plot.background = element_rect(fill = "#FFFEEB"),
        plot.title = element_text(family = "Forte", colour = "#870058",
                                  size = 52, hjust = -0.1, margin = margin(0, 0, 20, 0)),
        plot.subtitle = element_text(family = "Forte", colour = "orange",
                                     size = 42, hjust = 0.5, margin = margin(0, 0, 20, 0)),
        plot.caption = element_text(colour = "#870058", family = "Open Sans", face = "bold",
                            size = 12, hjust = 0.5, vjust = 2.5, margin = margin(0, 0, 10, 0)))


(p1 + p2) +
  plot_layout(ncol = 2)

ggsave("Images/Week40_Singers.tiff", plot = last_plot(), dpi = 400, height = 200, width = 330, units = "mm")

