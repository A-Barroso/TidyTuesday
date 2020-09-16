# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

tuesdata <- tidytuesdayR::tt_load(2020, week = 34)

plants <- tuesdata$plants

# Or read in the data manually

plants <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/plants.csv')
actions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/actions.csv')
threats <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/threats.csv')

# action type by country, compare to n of extinct?

library(tidyverse)
library()

country_action_db <- filter (actions) %>% select(country, group, red_list_category, action_type) 
casualties_db <- filter (threats) %>% select(country, group, red_list_category, threat_type)



## Do better - makes no sense - think: order, what, why, how
ggplot(country_action_db, aes(x = country, y = action_type, colour = group)) +
  geom_point() +
  labs(x = "Country", y = "Actions", title = "Preventions by plant type", caption = "Graph showing action types by country.") +
  theme_light()


ggplot(casualties_db, aes(x = country, y = threat_type, colour = group)) +
  geom_point() +
  labs(x = "Country", y = "Threats", title = "Threats by plant type", caption = "Graph showing xxx.") +
  theme_light()




### density is for how often it occurs, right? Well what do Ns mean then??
ggplot(data = casualties_db, aes(x = threat_type)) +
  geom_density(fill = "aquamarine") +
  labs(x = "Threats", y = "Prevalence", title = "Common Threats", caption = "Graph showing prevalence of threat types.") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  #  + coord_flip()  --> x = should be continuous not qualitative 




###########

ggplot(data = DartPoints) + geom_boxplot(mapping = aes(Length, colour = Name)) + labs(title = "Length of Points", caption = "boxplot showing length of points by named group.")

ggplot(data = DartPoints) + geom_density(mapping = aes(Length, colour = Name)) + theme_light() +  theme(legend.position = "bottom")


ggplot(data = V1_database) + geom_boxplot(mapping = aes(Length, colour = Name)) + labs(title = "Length of Points", caption = "boxplot showing length of points in Pedernales and Wells groups.")

ggplot(data = V1_database) + geom_density(mapping = aes(Length, colour = Name)) +  theme(legend.position = "bottom")


### ggplot (data = <DATA> ) + <GEOM_FUNCTION> (mapping = aes( <MAPPINGS> ), stat = <STAT> , position = <POSITION> ) + <COORDINATE_FUNCTION> + <FACET_FUNCTION> + <SCALE_FUNCTION> + <THEME_FUNCTION>







### /chakRa