## Set fonts
library(tidyverse)
library(sysfonts)
font_add_google(name = "Oswald", family = "Oswald")
font <- "Oswald"

## Get data

big_tech_stock_prices <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-07/big_tech_stock_prices.csv')
big_tech_companies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-07/big_tech_companies.csv')

## Replace symbol with name
df <- df %>% 
  left_join(big_tech_companies, by = 'stock_symbol') |> 
  select(-stock_symbol) |> 
  select(company, date:volume)

## Tidy up names
df$company = str_split(df$company, "\\,", simplify = TRUE)[,1]
df$company = str_remove_all(df$company, "\\Inc.|Corporation|Platforms")
df$company = str_squish(df$company)

## Remove columns not required
df$stock_symbol = NULL
df$high         = NULL
df$low          = NULL
df$volume       = NULL

## New DF 
high = df
high$company2 = high$company
high$company = NULL
high = filter(high, date >= "2020-01-01")

## Filter data to FAANG stocks after 2020
df2 <- filter(df, company %in% c("Apple" , "Amazon.com", "Alphabet", "Meta Platforms", "Netflix"), date >= "2020-01-01")

## Plot

plot <- ggplot(df2, aes(date,adj_close))+
  geom_line(data = high, 
            aes(x = date, y = adj_close, group = company2), 
            linewidth = .3, color = "#9a8f97"
  ) +
  geom_line(color="#f24c00", linewidth = .7)+
  facet_wrap(~ company, nrow = 5, labeller = labeller(facet_category = label_wrap_gen(width = 26)))+
  labs(title = "Performance of FAANG stocks since 2020",
       subtitle = "Closing price after adjustments when compared to other big tech prices",
       caption = "\n#TidyTuesday 2023 - Week 06 | Viz: @tangandhara")+
  theme_bw()+
  theme(plot.background = element_rect(fill = NA, color = NA),
        plot.margin = margin(20, 30, 20, 30),
        plot.title = element_text(
          size = 12, 
          face = "bold", 
          vjust = 0, 
          color = "grey25"),
        plot.subtitle =  element_text(
          size = 8),
        plot.caption = element_text(size = 8),
        axis.title = element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text = element_text(color = "grey40"),
        strip.text = element_text(size = 7, face = "bold", color = "#000000"),
        strip.background = element_blank(),
        panel.border = element_rect(colour = NA, fill = NA)
  )+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave("myplot.jpg", width = 10, height = 10, units = "in", dpi = 300)