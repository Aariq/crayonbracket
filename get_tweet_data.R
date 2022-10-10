
# Load packages -----------------------------------------------------------

library(tidyverse)
library(httr)
library(rtweet)


# Find tweets --------------------------------------------------------------
#only finds tweets from the most recent 10 days, so might need to combine data from multiple runs of this code.  Getting most recent 200 tweets to hopefully capture all 64 matches (2 tweets per match).
tweets <-
  search_tweets("#crayonbracket Match#", n = 200, include_rts = FALSE)
tweets
# View(tweets)


# Wrangle data ------------------------------------------------------------

# I need tweet ID, match number, whether the tweet is the colors or the polls, the colors

bracket <- tweets |> 
  select(id_str, full_text) |> 
  mutate(match = str_extract(full_text, "(?<=Match #)\\d+") |> as.numeric()) |> 
  mutate(poll = str_detect(full_text, "photo in previous tweet")) |> 
  select(match, poll, id_str, full_text) |> 
  filter(!is.na(match)) |> 
  arrange(match)

tweet_ids <- bracket |> select(match, id_str)

write_csv(bracket, paste0("data/crayonbracket_raw_", Sys.Date(), ".csv"))

