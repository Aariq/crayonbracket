
# Load packages -----------------------------------------------------------

library(tidyverse)
library(httr)
library(rtweet)


# Find tweets --------------------------------------------------------------

crayonbracket <- search_tweets("#crayonbracket Match#", include_rts = FALSE)
crayonbracket
View(crayonbracket)


# Wrangle data ------------------------------------------------------------

# I need tweet ID, match number, whether the tweet is the colors or the polls, the colors

bracket_2 <- crayonbracket |> 
  select(id_str, full_text) |> 
  mutate(match = str_extract(full_text, "(?<=Match #)\\d+") |> as.numeric()) |> 
  mutate(poll = str_detect(full_text, "photo in previous tweet")) |> 
  select(match, poll, id_str, full_text) |> 
  arrange(match)

bracket_2
#now extract colors??

colors <-
  bracket_2 |>
  filter(!poll) |>
  mutate(colors = str_extract_all(full_text, "#[A-Z0-9]{6}")) |>
  select(-full_text, - poll)

#extract poll IDs

polls <- bracket_2 |> filter(poll) |> select(match, poll_id = id_str)

colors_polls <- full_join(colors, polls)


# Get poll results --------------------------------------------------------

#functionality not in `rtweet` (yet), but can use the API still


headers <- c(
  'Authorization' = 'Bearer AAAAAAAAAAAAAAAAAAAAAMvffwEAAAAA4Y2LHaw%2FQ%2Fv1vEBJCg8az5bNgnw%3DExZpZkaqIr6n4IEveUeIdaTTW9pmbbdD9UHEsVEOxlbGJ06Oda'
)
