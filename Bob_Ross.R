library(tidyverse)
library(tidytuesdayR)
library(packcircles)
library(ggpubr)
library(jpeg)
library(magick)
library(sysfonts)
font_add_google(name = "Oswald", family = "Oswald")
font <- "Oswald"

tuesdata <- tidytuesdayR::tt_load(2023, week = 8)
bob_ross <- tuesdata$bob_ross
view(bob_ross)

## Filter table into colours & counts of TRUE value
bob_ross2 <- bob_ross |> select(c(10:27)) |> 
  pivot_longer(
    cols = everything(),
    names_to = "colours",
    values_to = "count"
  ) |> filter(count == TRUE) |> 
  group_by(colours) |>  
  summarise(
    count = n()
  )

colour_palette <- c("Alizarin_Crimson" = "#4E1500",
                    "Black_Gesso" = "#000000",
                    "Bright_Red" = "#DB0000",
                    "Burnt_Umber" = "#8A3324",
                    "Cadmium_Yellow" = "#FFEC00",
                    "Dark_Sienna" = "#5F2E1F",
                    "Indian_Red" = "#CD5C5C",
                    "Indian_Yellow" = "#FFB800",
                    "Liquid_Black" = "#000000",
                    "Liquid_Clear" = "#FFFFFF",
                    "Midnight_Black" = "#000000",
                    "Phthalo_Blue" = "#0C0040",
                    "Phthalo_Green" = "#102E3C",
                    "Prussian_Blue" = "#021E44",
                    "Sap_Green" = "#0A3410",
                    "Titanium_White" ="#FFFFFF" ,
                    "Van_Dyke_Brown" = "#221B15",
                    "Yellow_Ochre" = "#C79B00")

bob_ross3 <- cbind(bob_ross2, colour_palette)
bob_ross3$colours <- sub("_", " ", bob_ross3$colours)

## PLOT ##


## bar chart

ggplot(bob_ross3, aes(x = reorder(colours, -count), y = count, fill = colour_palette))+
  geom_bar(position='stack', stat='identity', colour="#D0CFCF")+
  scale_color_manual(values = c("Alizarin_Crimson" = "#4E1500",
                                "Black_Gesso" = "#000000",
                                "Bright_Red" = "#DB0000",
                                "Burnt_Umber" = "#8A3324",
                                "Cadmium_Yellow" = "#FFEC00",
                                "Dark_Sienna" = "#5F2E1F",
                                "Indian_Red" = "#CD5C5C",
                                "Indian_Yellow" = "#FFB800",
                                "Liquid_Black" = "#000000",
                                "Liquid_Clear" = "#FFFFFF",
                                "Midnight_Black" = "#000000",
                                "Phthalo_Blue" = "#0C0040",
                                "Phthalo_Green" = "#102E3C",
                                "Prussian_Blue" = "#021E44",
                                "Sap_Green" = "#0A3410",
                                "Titanium_White" ="#FFFFFF" ,
                                "Van_Dyke_Brown" = "#221B15",
                                "Yellow_Ochre" = "#C79B00")) +
  scale_fill_identity()+
  coord_flip()+
  ylab("Number of times colour was used")+
  xlab("Bob's colours")+
  labs(title = "The Joy of Bob Ross' Palette",
       subtitle = "The number of times each colour was used in Bob Ross' paintings",
       caption = "\n#TidyTuesday 2023 - Week 07 | Viz: @tangandhara")+
  theme_bw()+
  theme(plot.background = element_rect(fill = NA, color = NA),
        plot.margin = margin(10, 20, 10, 20),
        plot.title = element_text(
          size = 20, 
          face = "bold", 
          vjust = 0, 
          color = "grey25"),
        plot.subtitle =  element_text(
          size = 12),
        plot.caption = element_text(size = 8),
        axis.title = element_text(),
        axis.text.y=element_text(color = "grey40"),
        axis.ticks.y=element_blank(),
        axis.text = element_text(color = "grey40"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())

ggsave("myplot.jpg", width = 8, height = 5, units = "in", dpi = 300)

## Add logo to plot using {magick}
pub_plot <- image_read("myplot.jpg") # import exported plot
logo <- image_read("bob-ross.jpg") %>% 
  image_resize(600) # import logo
pub_plot %>% 
  image_composite(logo, offset = "+1722+250") # merge plot

