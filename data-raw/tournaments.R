
library(tidyverse)
library(rvest)
tourney_url <- 'https://liquipedia.net/halo/S-Tier_Tournaments'
page <- tourney_url %>% read_html()
tourney_nodes <- page %>% html_elements('.divTable.table-full-width.tournament-card')
tourney_nodes
name_nodes <- tourney_nodes %>% html_elements('.divCell.Tournament.Header') %>% html_elements('b')
names <- name_nodes %>% html_text2()
links <- name_nodes %>% html_elements('a') %>% html_attr('href')
links
names
extract_text <- function(x) {
  tourney_nodes %>% html_elements(sprintf('.divCell.%s', x)) %>% html_text2() %>% str_trim()
}
dates <- extract_text('EventDetails-Left-55.Header')
prizes <- extract_text('EventDetails-Right-45.Header')
locations <- extract_text('EventDetails-Left-60.Header')
participants <- extract_text('EventDetails-Right-40.Header')
first_places <- extract_text('Placement.FirstPlace')
second_places <- extract_text('Placement.SecondPlace')
tibble(
  name = names,
  url = sprintf('https://liquipedia.net%s', links),
  date = dates,
  prize = prizes,
  location = locations,
  n_participants = participants %>% str_remove('\\s+teams') %>% ,
  first_place = first_places,
  second_place = second_places
)
tourney_nodes %>% html_elements('.divCell.Placement.FirstPlace') %>% html_elements('.team-template-text > a') %>% html_attr('title')
