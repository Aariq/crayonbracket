
# Load packages -----------------------------------------------------------

library(tidyverse)
library(httr)
library(rtweet)


# Find tweets --------------------------------------------------------------

crayonbracket <- search_tweets("#crayonbracket Match#", include_rts = FALSE)
crayonbracket
# View(crayonbracket)


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

colors_polls <- 
  full_join(colors, polls) |>
  #assuming that first color is always the left/first option and second color is
  #always the right/second option
  unnest("colors") |>
  group_by(match) |> 
  mutate(position = 1:2)

# Get poll results --------------------------------------------------------

#functionality not in `rtweet` (yet), but can use the API still

bearer_token <- Sys.getenv("twitter_bearer_token")
headers <- c(
  'Authorization' = paste0('Bearer ', bearer_token)
)

url <- "https://api.twitter.com/"
base_path <- c("2", "tweets")


get_poll_result <- function(poll_id) {
  poll_result <-
    httr::GET(
      url = url,
      path = c(base_path, poll_id),
      query = list(expansions = "attachments.poll_ids"),
      add_headers(headers)
    ) |>
    content()
  
  map_df(1:2, ~poll_result$includes$polls[[1]]$options[[.x]] |> as_tibble() |> add_column(poll_id = poll_id))
}

poll_results <- map_df(unique(colors_polls$poll_id), get_poll_result)


# join with the rest of the data ------------------------------------------

full_data <- full_join(colors_polls, poll_results, by = c("poll_id", "position"))

write_rds(full_data, paste0("tweet_data_", Sys.Date()))
