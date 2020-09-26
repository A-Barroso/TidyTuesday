library("tidyverse")
library("extrafont")
library("cowplot")
library("scales")
library("ggtext")
library("here")
library("reshape2")
library("ggplot2")

expeditions <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/expeditions.csv')

exp.clean <- expeditions %>%
  filter(year > 1918) %>%
  select(year, peak_name, member_deaths, hired_staff_deaths) %>%
  drop_na() %>%
  group_by(peak_name, year) %>%
  summarise_if(is.numeric, funs(sum))


###### 10 DEADLIEST PEAKS ##########

exp_toll <- exp.clean %>%
  select(peak_name, member_deaths, hired_staff_deaths) %>%
  group_by(peak_name) %>%
  summarise_if(is.numeric, funs(sum))

exp_toll$death_toll <- exp_toll$member_deaths + exp_toll$hired_staff_deaths

#####################################

df <- exp.clean %>%
  filter(peak_name == "Everest" | peak_name == "Dhaulagiri I" | peak_name == "Manaslu" |
           peak_name == "Annapurna I" | peak_name == "Cho Oyu" | peak_name == "Makalu" |
           peak_name == "Kangchenjunga" | peak_name == "Pumori" | peak_name == "Ama Dablam" |
           peak_name == "Lhotse") %>%
  group_by(peak_name, year) %>%
  summarise_if(is.numeric, funs(sum))

df$member_deaths[(df$member_deaths == "0")] <- NA
df$hired_staff_deaths[(df$hired_staff_deaths == "0")] <- NA  

df1 <- df %>% drop_na()  

df2 <- df1
df2$peak_name <- as.factor(df2$peak_name)
df2$peak_name <- factor(df2$peak_name, levels = c("Lhotse",
                                  "Ama Dablam",
                                  "Pumori",
                                  "Kangchenjunga",
                                  "Makalu",
                                  "Cho Oyu",
                                  "Annapurna I",
                                  "Manaslu",
                                  "Dhaulagiri I",
                                  "Everest"))

p <- df1 %>% ggplot(aes(year, peak_name)) +
  geom_point(aes(size = member_deaths), colour = "red", alpha = 0.5) +
  geom_point(aes(size = hired_staff_deaths), colour = "#660000", alpha = 0.5) +
  scale_x_continuous(limits = c(1920, 2020)) +
  coord_flip() +
  theme_minimal() +
  labs(title = "A Century of Tragedy",
       subtitle = "Yearly death tolls of sherpas and expedition members per peak",
       x = "Year", y = "Peak name",
       caption = "@ABarroso_BA | Source: The Himalayan Database | #TidyTuesday Week 39") +
  theme(plot.margin = margin(20, 40, 10, 20),
        panel.grid.major = element_line(size = 0.3, colour = "grey85"),
        panel.grid.minor = element_line(size = 0.3, colour = "grey85"),
        legend.title = element_blank(),
        legend.text = element_text(colour = "#004E64"),
        axis.text.x = element_text(family = "ChaparralPro-LightIt", colour = "#004E64", size = 8),
        axis.text.y = element_text(family = "ChaparralPro-LightIt", colour = "#004E64", size = 8),
        axis.title.y = element_text(family = "Bernard MT Condensed", colour = "grey95",
                                    size = 12, margin = margin(0, 20, 0, 0)),
        axis.title.x = element_text(family = "Bernard MT Condensed", colour = "grey95",
                                    size = 12, margin = margin(10, 0, 45, 0)),
        plot.background = element_rect(fill = "grey75"),
        plot.title = element_text(family = "Bernard MT Condensed", colour = "#004E64",
                                  size = 39, hjust = 0.5, margin = margin(5, 20, 9, 20)),
        plot.subtitle = element_text(family = "Bernard MT Condensed", colour = "grey95",
                                     size = 16, hjust = 0.5, margin = margin(0, 0, 30, 0)),
        plot.caption = element_text(colour = "white", family = "ChaparralPro-LightIt",
                                    size = 9, hjust = 0.48, vjust = -6, margin = margin(10, 0, 0, 0)))

ggdraw() +
  draw_plot(p) +
  draw_label("Death toll", x = 0.91, y = 0.57, colour = "white", size = 12) +
  draw_label("Over the last 100 years, 181 Sherpa guides have perished in expeditions to these ten \n peaks. In total, these have accounted for 263 deaths in the Himalayan mountain \n range, making up 23.89% of the region's cumulative mountaineering death toll.",
             x = 0.48, y = 0.10, colour = "#004E64", size = 10, alpha = 0.5) +
  draw_label("Sherpas", x = 0.91, y = 0.37, colour = "#660000", size = 11) +
  draw_label("Expedition\n members ", x = 0.91, y = 0.32, colour = "#E00000", size = 10)
  
ggsave("Images/Week39_Himal.tiff", plot = last_plot(), dpi = 400, width = 225, height = 160, units = "mm")
