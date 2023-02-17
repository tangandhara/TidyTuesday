library(tidyverse)
library(here)
library(janitor)
library(patchwork)
library(showtext)
library(ggtext)
library(glue)
library(patchwork)

age_gaps <- read_csv(
  "http://hollywoodagegap.com/movies.csv",
) |> 
  clean_names()

glimpse(age_gaps)

# Quickly check that the columns make sense.
length(vctrs::vec_cast(age_gaps$release_year, integer())) == nrow(age_gaps)
length(vctrs::vec_cast(age_gaps$age_difference, integer())) == nrow(age_gaps)
unique(age_gaps$actor_1_gender)
!any(is.na(as.Date(age_gaps$actor_1_birthdate)))
length(vctrs::vec_cast(age_gaps$actor_1_age, integer())) == nrow(age_gaps)
unique(age_gaps$actor_2_gender)
!any(is.na(as.Date(age_gaps$actor_2_birthdate)))
length(vctrs::vec_cast(age_gaps$actor_2_age, integer())) == nrow(age_gaps)

# Formally set the dates to dates.
age_gaps <- age_gaps |> 
  mutate(
    across(
      ends_with("birthdate"),
      as.Date
    )
  )

# Try to get a better understanding of the "gender" columns.
count(age_gaps, actor_1_gender)
count(age_gaps, actor_2_gender)

# The order of the characters doesn't seem to be consistent
age_gaps |> 
  summarize(
    p_1_older = mean(actor_1_age > actor_2_age),
    p_1_male = mean(actor_1_gender == "man"),
    p_1_female_2_male = mean(actor_1_gender == "woman" & actor_2_gender == "man"),
    p_1_first_alpha = mean(actor_1_name < actor_2_name)
  )

# For the most part, they put the man first if there's a man in the couple. It
# doesn't look like there's a strict rule, though. But beware: Some movies have
# more than 1 couple! Let's use all that to rebuild the data, always putting the
# older character first.
age_gaps <- age_gaps |> 
  mutate(
    couple_number = row_number(),
    .by = "movie_name"
  ) |> 
  pivot_longer(
    cols = starts_with(c("actor_1_", "actor_2_")),
    names_to = c(NA, NA, ".value"),
    names_sep = "_"
  ) |> 
  # Put the older actor first.
  arrange(desc(age_difference), movie_name, birthdate) |> 
  # While we have it pivoted, correct Elliot Page's name. I don't know if other
  # actors are similarly deadnamed, but at least we can fix this one. Note that
  # the *characters* played by Elliot in these particular films were women, so
  # I'll leave the gender as-is.
  mutate(
    name = case_match(
      name,
      "Ellen Page" ~ "Elliot Page",
      .default = name
    )
  ) |>
  mutate(
    position = row_number(),
    .by = c("movie_name", "couple_number")
  ) |> 
  pivot_wider(
    names_from = "position",
    names_glue = "actor_{position}_{.value}",
    values_from = c("name", "gender", "birthdate", "age")
  )

# The gender isn't really the actor so much as it is the character. Let's
# correct that.
age_gaps <- age_gaps |> 
  rename(
    "character_1_gender" = "actor_1_gender",
    "character_2_gender" = "actor_2_gender"
  )

glimpse(age_gaps)

# Save the data.
write_csv(
  age_gaps,
  here::here(
    "data", "2023", "2023-02-14",
    "age_gaps.csv"
  )
)


## Filter dataset to Leonardo DiCaprio
ldc <- age_gaps |> filter(actor_1_name =="Leonardo DiCaprio" | actor_2_name =="Leonardo DiCaprio")

## Plot age_difference overall vs age_difference for LDC
ggplot(ldc, aes(release_year,age_difference))+
  geom_jitter(data = age_gaps, mapping = aes(x = release_year, y=age_difference, size = age_difference), colour = alpha("#B1A7A6", 0.8))+
  geom_smooth(data= age_gaps, colour = "#161A1D", se = FALSE) +
  geom_jitter(aes(color="#660708", size = age_difference))+
  geom_smooth(colour = "#E5383B", se = FALSE)


## Advanced plot!
col_label = str_wrap("Age gaps in Hollywood films have been falling over the years but how do films starring <span style='color:#f24c00;'>Leonardo DiCaprio</span> compare?", 10)

p1 <- ggplot(ldc, aes(release_year,age_difference))+
  geom_jitter(data = age_gaps, mapping = aes(x = release_year, y=age_difference, size = age_difference), colour = alpha("#B1A7A6", 0.8))+
  geom_smooth(data= age_gaps, method=lm, colour = "#161A1D", se = FALSE) +
  geom_jitter(aes(color=alpha("#E5383B", 1), size = age_difference))+
  theme(legend.position = "none")+
  labs(y= "Age gap in years",
       subtitle = col_label)


p2 <- ggplot(ldc, aes(release_year,age_difference))+
  geom_jitter(aes(color = character_1_gender, size = age_difference))+
  scale_colour_manual(values = c("#E5383B", "#B1A7A6"),
                      name = "Character gender",
                      labels = c("Man", "Woman"))+
  theme(legend.position = "right",
        legend.title = element_text(size=24),
        legend.text = element_text(size=24),
        legend.key = element_rect(fill = "#F0F5F5"),
        legend.box.background = element_rect(fill="#F0F5F5"))+
  guides(size = FALSE)+
  labs(y= "Age gap in years",
       subtitle = "The age gap between male and female characters in Leonardo DiCaprio's films has been rising")

## Join plots together with patchwork
(p1 + p2 +plot_layout(widths = c(4,3)))+
  plot_annotation(title = "Hollywood Age Gaps",
                  caption = "\n#TidyTuesday 2023 - Week 07 | Viz: @tangandhara")&
  theme(text = element_text(family = "Commissioner",
                            colour = "#001845"),
        plot.subtitle = element_markdown(size = 26,
                                         lineheight = 0.4,
                                         hjust = 0,
                                         colour = "#001845",
                                         margin = unit(c(0, 0, -0.5, 0), "cm")),
        plot.tag.position = c(0.01, 0.72),
        plot.margin = margin(0, 10, 10, 10),
        plot.title = element_text(family = "Tourney",
                                  size = 60,
                                  colour = "#E5383B",
                                  margin = unit(c(0.5, 0, 0.5, 0), "cm")),
        plot.caption = element_markdown(size = 26,
                                         lineheight = 0.4,
                                         hjust = 0,
                                         colour = "#E5383B",
                                         margin = unit(c(0, 0, -0.5, 0), "cm")),
        plot.title.position = "plot",
        axis.text = element_text(size = 24, vjust = 2, colour = "#001845"),
        axis.title.y = element_text(size = 24, vjust = 2, colour = "#001845"),
        strip.text = element_text(size = 24, lineheight = 0.4, colour = "#001845"),
        axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        plot.background = element_rect(fill = "#F0F5F5", colour = "#F0F5F5"),
        panel.background = element_rect(fill = "#F0F5F5", colour = "#F0F5F5"))

## Save plt

ggsave("myplot.jpg", width = 30, height = 22.5, units = "cm", dpi = 300)
