library(tidyverse)
library(showtext)
library(patchwork)
library(ggtext)
library(tidytuesdayR)

population <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-22/population.csv')


font_add_google("Roboto", "roboto")
font_add_google("Roboto Slab", "robotoslab")
showtext_auto()

country <- "United Kingdom of Great Britain and Northern Ireland"

## Calculate uk totas
uk <- population |> 
  filter(coa_name == country, year ==2022) |> 
  select(year, refugees, asylum_seekers, stateless, returned_refugees) |> 
  group_by(year) |> 
  summarise(across(everything(), sum)) |> 
  mutate(total = (refugees + asylum_seekers + stateless) - returned_refugees, .after = 1) 

## calculate global totals
all <- population |>
  filter(year ==2022) |>
  select(year, refugees, asylum_seekers, stateless, returned_refugees) |> 
  group_by(year) |> 
  summarise(across(everything(), sum)) |> 
  mutate(total = (refugees + asylum_seekers + stateless) - returned_refugees, .after = 1) 

## calculate proprtion of total
proportion <- uk |> 
  mutate(
    uk_prop = round((uk$total / all$total)*100, 2),
    all_prop = 100-uk_prop,
    .after = 1
    ) |> 
  select(year, uk_prop, all_prop) |> 
  pivot_longer(cols = c(uk_prop, all_prop), names_to="Variable", values_to="Value")


## total to get top 10
total <- population |>
  filter(year ==2022) |>
  select(year, refugees, asylum_seekers, stateless, returned_refugees, coa_name) |> 
  group_by(coa_name) |> 
  summarise(across(everything(), sum)) |> 
  mutate(total = (refugees + asylum_seekers + stateless) - returned_refugees, .after = 1) 

top_10_values <- total[order(total[[2]], decreasing = TRUE), ][1:10, 1:2]

comparison_value <- total |> select(coa_name, total) |> filter(coa_name == country)

top_10_values <- rbind(top_10_values, comparison_value)


# Compute the proportions for top 11 relative to the total sum
# and multiply by 100 to get percentage for waffle chart representation
top_10_values$proportion <- round((top_10_values$total / all$total) * 100,2)

#finds difference between proportions and create new entry to ad to df
# Calculate the sum of top 11 values
sum_top_11 <- sum(top_10_values$total)

# Calculate the sum of the rest of the values (outside the top 11)
rest_sum = all$total- sum_top_11

# Calculate proportion for this 'rest' category
rest_proportion <- round((rest_sum / all$total) * 100,2)

# Add this proportion as a new entry in the dataframe
new_entry <- data.frame(coa_name = "Others", total = sum(all$total-sum_top_11), proportion = rest_proportion)
top_10_values <- rbind(top_10_values, new_entry)

# create a vector of names and proportions
v1 <- setNames(top_10_values$proportion, top_10_values$coa_name)

# Create the waffle chart with the proportions
library(waffle)
waffle(v1, rows = 8, 
       colors = c("#03045E", "#023E8A", "#0077B6", "#0096c7", "#00Bfd8", "#48CAE4", "#90E0EF", "#ADE8F4", "#CAF0F8", "#7BDFF2", "#BD3A50", "#8F8073"), 
       xlab = "Data source: UNCHR Refugee Data Finder | #TidyTuesday - week 34 | Viz by @tangandhara",
       size = 2.5)+labs(title = "Refugees & asylum-seekers in 2022", subtitle = "\nHow does the proportion of forcibly displaced people seeking asylum in the UK compare with top 10 countries?\n\nOnly 1.3% of the world's displaced sought refuge in the UK in 2022 compared to 9.8% in Turkey, the largest host of refugees and asylum seekers in the world")+
  theme(plot.title = element_text(face = "bold",family = "Roboto"),
        plot.subtitle = element_text(hjust = 0))

ggsave("myplot.jpg", width = 8, height = 5, units = "in", dpi = 300)


