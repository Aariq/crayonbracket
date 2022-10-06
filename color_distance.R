
# Load packages and data --------------------------------------------------
library(colorspace)
library(tidyverse)
library(ggrepel)

full_data <- read_rds("tweet_data_2022-10-06.rds")


# Plot colors? ------------------------------------------------------------
# full_data |> 
#   group_by(match) |> 
#   group_split() |> 
#   map(~hclplot(.x$colors))


# Calculate distance ------------------------------------------------------
# Is Euclidean distance a good idea?

#Create matrix column of HSV coordinates for each color
dist_diff <- 
  full_data |>
  mutate(coords = colors |> hex2RGB() |> as("LUV") |> coords()) |> 
  #For each match ...
  group_by(match) |> 
  #Use the matrix to calculate euclidean distance
  summarise(distance = dist(coords, method = "euclidean"),
            poll_margin = abs(diff(votes)),
            division = unique(division),
            match_name = glue::glue_collapse(color_name, sep = " vs. "))

division_color <- 
  c(Blue = "blue", Grayscale = "grey", Green = "green", `Orange/Brown` = "brown", Pink = "pink", Purple = "purple", Red = "red", Yellow = "goldenrod")

ggplot(dist_diff, aes(x = distance, y = poll_margin, color = division)) + 
  geom_point() +
  geom_label_repel(aes(label = match), show.legend = FALSE) +
  # geom_label(aes(label = match)) +
  scale_color_manual(values = division_color) +
  labs(title = "Are #crayonbracket win margins related to color similarity?",
       subtitle = "labels = match #",
       x = "Color difference (distance in LUV space)",
       y = "Win margin (votes)") +
  theme_dark() +
  theme(text = element_text(size = 12))

ggsave("color_diff_vs_win_margin.png", width = 1200*2, height = 675*2, unit = "px")

