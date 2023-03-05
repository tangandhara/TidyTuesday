# TidyTuesday Week 04 - Alone 

# Read in with tidytuesdayR package 

tuesdata <- tidytuesdayR::tt_load('2023-01-24')
tuesdata <- tidytuesdayR::tt_load(2023, week = 4)
alone <- tuesdata$alone

# Or read in the data manually

survivalists <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-24/survivalists.csv')
loadouts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-24/loadouts.csv')
episodes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-24/episodes.csv')
seasons <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-24/seasons.csv')

## For this week I decided to identify where most North American surivalists live. 

library(tidyverse)
library(ggmap)
library(broom)
library(geojsonio)
library(mapproj)

# Where are survivalists from? replace missing state with country name
home <- survivalists |> select(season, state, country) |> mutate(state = coalesce(state,country))

# Generate a tibble of countries and states and the number of survivalists
home2 <- home |> group_by(country, state) |> summarise(survivalists = n()) |> print(n=41)

# I found .geojson files online for Canada and US that I imported into R 
spdf_can <- geojson_read("/canada_provinces.geojson",  what = "sp") 
spdf_us <- geojson_read("/states.geojson",  what = "sp")

## To be able to use merge with the home table above they needed to be fortified 
spdf_us_fortified <- broom::tidy(spdf_us)
spdf_can_fortified <- broom::tidy(spdf_can)

## Upon fortifying I realised that the state & province names were missing so this appends the fortified spdf with state names. First create a temp df with the names
temp_df <- data.frame(spdf_can@data$PRENAME)
names(temp_df) <- c("PRENAME")
temp_df$id <- seq(1,nrow(temp_df))

## Merge the temp df to the fortified df
spdf_can_fortified <- merge(spdf_can_fortified, temp_df, by="id")

#rename the state name to match survivalists df
spdf_can_fortified <- rename(spdf_can_fortified, state = PRENAME)

## join everything 
home2 <- left_join(home2, spdf_us_fortified, by ="state")
home2 <- left_join(home2, spdf_can_fortified, by ="state")

## maps the plot the final result - updated to include state borders

ggplot(home2) +
  geom_polygon(spdf_can_fortified, mapping=aes( x = long, y = lat, group = group ),fill = "white", color="grey", size = 0.5)+
  geom_polygon(spdf_us_fortified, mapping=aes( x = long, y = lat, group = group),fill = "white", color="grey")+
  geom_polygon(aes( x = long.x, y = lat.x, group = group.x,fill=survivalists),color="grey",size = 0.03)+
  scale_fill_distiller(palette='RdYlGn', breaks = c(1,3,5,7,9,11))+
  theme_void()+
  coord_map()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.title = element_text(face="bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5))+
  theme(legend.position="right",
        legend.key.size = unit(0.3, 'cm'), 
        legend.key.height = unit(0.3, 'cm'), 
        legend.key.width = unit(0.3, 'cm'), 
        legend.title = element_text(size=8), 
        legend.text = element_text(size=8))+
  labs(title="ALONE: The Survivalists",
       subtitle = "Where do American & Canadian surivalists live?",
       fill="Number of\nsurvivalists",
       caption = "\n#TidyTuesday 2023 - Week 04 | Viz: @tangandhara")

### Save plot
ggsave("Alone.jpg", width = 10, height = 10, units = "in", dpi = 300)
