## TidyTuesday week 39 - Roy Kent F**k count
library(tidytuesdayR)
library(tidyverse)
library(showtext)
library(extrafont)
library(ggtext)

richmondway <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-26/richmondway.csv')


font_add_google("Cabin", "cabin")
font_add_google("Enriqueta", "enriqueta")

showtext_auto()


ggplot(richmondway, aes(x = as.factor(Season), y = F_perc, size = richmondway$F_count_RK))+
  geom_point(colour = richmondway$Season, alpha = 0.4, position = "jitter")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  scale_x_discrete(limits = c("1", "2", "3"), labels = c("Season 1", "Season 2", "Season 3"))+
  theme_minimal(base_size = 12, base_family = "space") + 
  labs(title = "Dropping The F-Bomb: Roy Kent's F-Word Count In Ted Lasso By Season",
       subtitle = str_wrap("In season 1 of Ted Lasso, Roy Kent dropped the f-word sparingly but by season 2 he had a higher proportion of all f-bombs per episode. However, in season 3 he had a lower proportion of overall F-bombs but dropped them at a more consistently.", width = 110),
       y = "F-score percentage",
       caption = "Roy Kent F-word count data from Deepsha Menghani's {richmondway} | #TidyTuesday - week 39 | Viz by @tantheidiot")+
  theme(axis.title.x = element_blank(),
        legend.position = "right",
        legend.title = element_text(face = "bold",family = "enriqueta"),
        text = element_text(colour = "#323A30", family = "cabin"),
        plot.title = element_text(face = "bold", colour = "#0C1509", size = rel(1.5), family = "enriqueta", lineheight = 1.3,margin = margin(0.5, 0, 1, 0, "lines")),
        plot.subtitle = ggtext::element_textbox_simple(hjust = 0, size = rel(1.1), lineheight = 1.3,
                                                       margin = margin(0, 0, 1, 0, "lines")),
        plot.caption = element_text(face = "bold", margin = margin(t = 20, r = 0, b = 10, l = 0)),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "#DAD7CD", linewidth = .1))+
    scale_size_continuous(breaks = c(2, 5, 10, 20, 23), "Size by count of F-bombs")
