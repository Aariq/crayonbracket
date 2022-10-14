library(tidyverse)
library(rtweet)
library(httr)

# Read data, extract tweet IDs --------------------------------------------
tweet_data <- read_csv("data/manual_entry.csv")
tweet_data <- tweet_data |> 
  mutate(id = str_extract(url, "(?<=https://twitter.com/sbarolo/status/)\\d+"))

# Lookup tweets -----------------------------------------------------------
tweets <- lookup_tweets(tweet_data$id)


# Wrangle data ------------------------------------------------------------
bracket <- tweets |> 
  select(id_str, full_text) |> 
  mutate(match = str_extract(full_text, "(?<=Match #)\\s?\\d+") |> as.numeric()) |> 
  mutate(poll = str_detect(full_text, "photo in previous tweet")) |> 
  select(match, poll, id_str, full_text) |> 
  filter(!is.na(match)) |> 
  arrange(match)
bracket
#any missing matches?
bracket$match[!1:max(bracket$match) %in% unique(bracket$match)]

#any matches missing one tweet?
bracket |> count(match) |> filter(n != 2) 


colors <-
  bracket |>
  filter(!poll) |>
  mutate(colors = str_extract_all(full_text, "#[A-Z0-9]{6}")) |>
  select(-full_text, - poll)

#extract poll IDs

polls <- bracket |> filter(poll) |> select(match, poll_id = id_str)

colors_polls <- 
  full_join(colors, polls) |>
  #assuming that first color is always the left/first option and second color is
  #always the right/second option
  unnest("colors") |>
  group_by(match) |> 
  mutate(position = 1:2)

# Get poll results --------------------------------------------------------

#functionality not in `rtweet` (yet), but can use the API still
#To run this, you need a twitter API bearer token.  Save it in a .Renviron file to use this code.
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
  
  map_df(1:2, ~poll_result$includes$polls[[1]]$options[[.x]] |>
           as_tibble() |>
           add_column(poll_id = poll_id))
}

poll_results <- map_df(unique(colors_polls$poll_id), get_poll_result)


# join with the rest of the data ------------------------------------------

full_data <- full_join(colors_polls, poll_results, by = c("poll_id", "position"))

# Clean data ----------------------------------------------------------

full_data <-
  full_data |> 
  mutate(color_name = str_remove(label, " \\(.+\\)")) |> 
  #add division
  mutate(division = case_when(
    match %in% c(1:4, 31, 32, 48) ~ "Pink",
    match %in% c(5, 6, 33) ~ "Red",
    match %in% c(7:14, 34:37, 49, 50, 55) ~ "Orange/Brown",
    match %in% c(15, 16, 38) ~ "Yellow",
    match %in% c(17:21, 39:40, 51) ~ "Purple",
    match %in% c(21:24, 41:43, 52, 56) ~ "Blue",
    match %in% c(25:28, 44:45, 53) ~ "Green",
    match %in% c(29:30, 46:47, 54) ~ "Grayscale",
    TRUE ~ NA_character_
  ))

write_csv(full_data, "data/crayonbracket.csv")
