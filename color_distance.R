
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
dist_diff <- full_data |>
  mutate(hsv = colors |> hex2RGB() |> as("HSV") |> coords()) |> 
  #For each match ...
  group_by(match) |> 
  #Use the matrix to calculate euclidean distance
  summarise(distance = dist(hsv, method = "euclidean"),
            poll_margin = abs(diff(votes)))

ggplot(dist_diff, aes(x = distance, y = poll_margin)) + 
  geom_point() +
  geom_label_repel(aes(label = match)) +
  labs(title = "Are #crayonbracket win margins related to color similarity?",
       x = "Euclidean distance in HSV color space",
       y = "Win margin (|color 1 - color 2|)") +
  theme_bw()

ggsave("prelim-data.png")
