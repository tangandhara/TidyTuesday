library(tidyverse)
library(readr)
library(tidyr)
library(hrbrthemes)
library(forcats)

groundhogs <- read_csv("data/2024/2024-01-30/groundhogs.csv")
predictions <- read_csv("data/2024/2024-01-30/predictions.csv")

new_predictions_data <- predictions |>
  mutate(pred = case_when(shadow == TRUE ~ "Shadow",
                             shadow == FALSE ~ "No shadow",
                             is.na(shadow) == TRUE ~ "No record")) |> 
  left_join(groundhogs) |> 
  select(id, year, pred, region) |> 
  group_by(pred, region) |> 
  count(pred, region) |> 
  arrange(region) |> 
  print(n = 85)


new_predictions <- new_predictions_data |> 
  group_by(region) |> 
  mutate(total_count = sum(n)) |> 
  ungroup() |> 
  mutate(proportion = round((n / total_count)*100, 1)) |> 
  select(-n, -total_count) |> 
  arrange(region)


ggplot(new_predictions,
       aes(x = pred, 
           y = fct_reorder(region, desc(region)),
           colour = pred,
           size = proportion)) +
  geom_point(alpha = 0.6) +
  geom_text(aes(label = proportion), 
            colour = "black", 
            size = 3) +
  scale_x_discrete(position = "top") +
  scale_y_discrete(expand = expansion(add = c(2,2.5)))+
  scale_radius(range = c(9, 22)) + # Adjust as required.
  scale_color_brewer(palette = "Set1") +
  labs(title = "Groundhog Day Predictions",
       subtitle = str_wrap("Since 1886, Groundhog Day is celebrated annually across North America on February 2nd
                           when ‘prognosticating’ animals predict the onset of spring. 
                           If the groundhog ‘sees its shadow’, it means six more weeks of winter but 
                           if the groundhog doesn’t see its shadow, spring will come early. 
                           The chart below shows the percentage of all predictions made in the region listed. \n", width = 150),
       x = NULL,
       y = NULL,
       caption = "\n#TidyTuesday 2024 - Week 05 | Viz: @tantheidiot.bsky.social") +
  theme_ipsum()+
  theme(
        legend.position = "none",
        panel.background = element_blank(),
        panel.grid = element_line(color = "black",
                                  size = 0.6,
                                  linetype = 4),
        plot.title = element_text(size = 20,
                                  colour = "Black",
                                  face = "bold",
                                  margin = unit(c(0, 0, 0.5, 0), "cm")),
        plot.subtitle = element_text(size = 10,
                                         lineheight = 1.5,
                                         hjust = 0,
                                         colour = "Black",
                                         margin = unit(c(0, 0, 0.5, 0), "cm")),
        axis.text.y = element_text(margin = unit(c(0, -2, 0, 1), "cm"), vjust = -.1),
        axis.text.x = element_text( margin = unit(c(1, 0, 0, 0), "cm")),
        axis.text = element_text(size = 10, face = "bold", colour = "Black"),
        plot.caption = element_text(size = 8,
                                        lineheight = 0.4,
                                        vjust = -2.9,
                                        face = "bold",
                                        colour = "gray",
                                        margin = unit(c(0, 0, 1, 1), "cm")))
#
ggsave("myplot_wk5.jpg", width = 30, height = 20, units = "cm", dpi = 300)
