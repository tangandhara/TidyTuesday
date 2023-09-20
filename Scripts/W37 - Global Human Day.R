## TidyTuesday week 37 - Global Human Day
library(tidytuesdayR)
library(tidyverse)
library(showtext)
library(extrafont)
library(ggtext)

font_add_google("Cabin", "cabin")
font_add_google("Enriqueta", "enriqueta")

showtext_auto()

tuesdata <- tidytuesdayR::tt_load('2023-09-12')
all_countries <- tuesdata$all_countries
country_regions <- tuesdata$country_regions
global_human_day <- tuesdata$global_human_day
global_economic_activity <- tuesdata$global_economic_activity

## filter countries by time spent for Food prep and group by global regions

all_countries2 <- all_countries |> filter(Subcategory == "Food growth & collection") |> 
  inner_join(country_regions) |> 
  group_by(region_name) |>
  summarise(avg = round(mean(hoursPerDayCombined)*60,2)) |> 
  mutate(color_group = ifelse(grepl("Africa", region_name), "Africa", 
                        ifelse(grepl("Europe", region_name), "Europe",
                               ifelse(grepl("Asia", region_name), "Asia",
                                      ifelse(grepl("America", region_name), "America","Rest of the world"))))) ##categorises based on string in variable


all_countries2$color_group <- factor(all_countries2$color_group, levels = c("Africa", "Asia", "America", "Europe", "Rest of the world"))


countries <- all_countries2[order(all_countries2[[3]], decreasing = TRUE), ][1:20, 1:3] ## do not use


## Plot

food <- ggplot(countries, aes(y = avg, x = reorder(region_name, avg), fill=color_group))+
  geom_col(aes(alpha = avg))+
  labs(x = "Sub-region", y = "Minutes",
       title = "Food growth around the world",
       subtitle = "Average amount of time per day spent on food growth & collection in different regions of the world",
       caption = "Data source: The Human Chronome Project | #TidyTuesday - week 37 | Viz by @tangandhara")+
  scale_fill_manual(values = c("Africa" = "#fb8500", 
                               "Asia" = "#FFB703",
                               "America" = "#023047",
                               "Europe" = "#219EBC",
                               "Rest of the world" = "#8ECAE6"))+
  scale_alpha(range = c(.2, 1))+
  theme_minimal(base_size = 12, base_family = "space") +
  theme(text = element_text(colour = "#323A30", family = "cabin"),
        plot.title = ggtext::element_textbox_simple(face = "bold", colour = "#0C1509", size = rel(1.5), family = "enriqueta",
                                  lineheight = 1.3,
                                  margin = margin(0.5, 0, 1, 0, "lines")),
        plot.subtitle = ggtext::element_textbox_simple(hjust = 0, size = rel(1.1), lineheight = 1.3,
                                     margin = margin(0, 0, 1, 0, "lines")),
        legend.position = "none",
        strip.text = ggtext::element_textbox_simple(family = "enriqueta",
                                  colour = "#323A30", 
                                  size = rel(1.1), face = "bold",
                                  margin = margin(2, 0, 0.5, 0, "lines")),
        axis.text = element_text(colour = "#323A30"))+
  facet_wrap(~color_group, nrow = 1, scales = "free_y" )+coord_flip()

## scale_fill_brewer(palette = "Paired")

food
ggsave("myplot.jpg", width = 8, height = 5, units = "in", dpi = 300)
