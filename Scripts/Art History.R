##

font_add_google(name = "Oswald", family = "Oswald")
font <- "Oswald"

showtext_auto()
showtext_opts(dpe=320)
artists <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-17/artists.csv')

df2 <- artists |> 
  select(edition_number, book, artist_name, space_ratio_per_page_total) |> 
  group_by(edition_number, book) |> 
  mutate(avg = mean(space_ratio_per_page_total, na.rm = TRUE)) |> 
  filter(artist_name == "Vincent Van Gogh") |> 
  mutate(delta = space_ratio_per_page_total - avg) |> 
  pivot_longer(cols = c(space_ratio_per_page_total, avg)) |> 
  mutate(name = recode(name, space_ratio_per_page_total = "Van Gogh", avg = "Average")) |> 
  filter(book == "Janson")

align <- ifelse(df2$name == "Avgerage", 0, 1)

df2 |> 
  ggplot(aes(y = as.character(edition_number), x = value)) + 
  geom_line(aes(group = edition_number), size = 5, color = "#F1C40F",lineend = "round") +
  geom_text(aes(label = name), size = 8, color = "#FF5B33", hjust = align, family = font) + 
  scale_x_continuous() + 
  theme_minimal() +
  theme(plot.title = element_text(family = font, size = 32, hjust = 0.5, colour = "#888888"),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = font, size = 20, hjust = 0.5, colour = "#2E86C1"),
        plot.caption.position = "plot",
        plot.caption = element_text(family = font, size = 20, hjust = 0.5, colour = "#2E86C1"),
        legend.position = "none",
        axis.title = element_text(family = font, size = 20, hjust = 0.5, colour = "#888888"),
        axis.text = element_text(family = font, size = 20, hjust = 0.5, colour = "#888888"),
        panel.grid = element_line(linewidth = 0.5, linetype = "dotted", colour = "#888888"),
        plot.margin = unit(c(1.0, 1.0, 1.0, 1.0), "cm"),
        plot.background = element_rect(colour = "#FFFFFF", fill = "#FFFFFF")) + 
  labs(title = "Vincent Van Gogh",
       subtitle = str_wrap("The space allocated to the artist Vincent Van Gogh in every edition of Janson's art history textbook from 1963 until 2011 relative to the average for all artists\n", width = 90),
       y = "Book Editions\n",
       x = "\nSpace per Page Ratio",
       caption = "\n#TidyTuesday 2023 - Week 03 | Viz: @tangandhara (based on code by @ryanahart)"
  )