library(tidyverse)
library(tidytuesdayR)
library(showtext)
library(sysfonts)
library(patchwork)
font_add_google(name = "DM Serif Display", family = "DM Serif")
showtext_auto()
showtext_opts(dpi = 300)

tuesdata <- tidytuesdayR::tt_load('2023-02-28')
afrisenti <- tuesdata$afrisenti
afrisenti <- inner_join(afrisenti, tuesdata$languages, by = 'language_iso_code') ##append full name to df
afrisenti <- afrisenti[c(-1, -2,-4)] ## drop columns
afrisenti <- afrisenti[c(2,1)] ## re-order columns
language_fam <- data.frame(language = tuesdata$languages$language,
                          family = c('Afro-Asiatic','Afro-Asiatic','Afro-Asiatic','Afro-Asiatic','Niger-Congo','Niger-Congo', 'Afro-Asiatic', 'English Creole', 'Indo-European', 'Niger-Congo', 'Afro-Asiatic', 'Niger-Congo', 'Niger-Congo', 'Niger-Congo'))
afrisenti <- inner_join(afrisenti,language_fam, by = 'language') ## append language family
afrisenti2 <- afrisenti |> group_by(language, family, label) |> 
  tally() ## summarise dataset
#### not used  #### afrisenti2 <- afrisenti2 |> pivot_wider(names_from = label, values_from = n)

###PLOT

ggplot(afrisenti2, aes(fill=label, y=reorder(language, desc(language)), x=n)) + 
  geom_bar(position="fill", stat="identity")+
  scale_fill_manual("legend", values = c("negative" = "#E76F51",
                                         "neutral" = "#E9C46A",
                                         "positive" = "#2A9D8F"),
                    name = "Sentiment", labels = c("Positive", "Neutral", "Negative"))+
  labs(title = "Sentiment analysis of tweets in 14 African languages",
       subtitle = "A visualisation of sentiment in the AfriSenti dataset\n",
       caption = "\n#TidyTuesday 2023 - Week 09 | Viz: @tangandhara", family = "DM Serif")+
  theme_void()+
  theme(plot.background = element_rect(fill = "#F1FAEE",
                                       colour = "#F1FAEE"),
        plot.margin = margin(10, 20, 10, 20),
        plot.title = element_text(
          size = 20, 
          face = "bold", 
          vjust = 0,
          color = "grey25"),
        plot.subtitle =  element_text(
          size = 12,
          color = "grey25"),
        plot.caption = element_text(size = 8, color = "grey40"),
        axis.title = element_text(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y=element_text(color = "grey40",hjust=1),
        axis.ticks.y=element_blank(),
        axis.text = element_text(color = "grey40"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        text = element_text(family = "DM Serif"),
        plot.title.position = "plot",
        legend.title = element_text(colour="grey40"),
        legend.text = element_text(colour="grey40"))+
  guides(fill = guide_legend(reverse = TRUE))+
  scale_x_continuous(labels = scales::percent_format())

#####################

ggsave("myplot.jpg", width = 10, height = 5, units = "in", dpi = 300)

