
library(tidyverse)
library(lubridate)
library(rvest)

scrape_time <- now()

scrape_tournament <- function(url) {
  cat(
    sprintf('Scraping %s.', basename(url)),
    sep = '\n'
  )
  page <- url %>% read_html()
  tourney_nodes <- page %>% html_elements('.divTable.table-full-width.tournament-card')
  name_nodes <- tourney_nodes %>% html_elements('.divCell.Tournament.Header') %>% html_elements('b')
  names <- name_nodes %>% html_text2()
  links <- name_nodes %>% html_elements('a') %>% html_attr('href')
  extract_text <- function(x) {
    tourney_nodes %>% html_elements(sprintf('.divCell.%s', x)) %>% html_text2() %>% str_trim()
  }
  dates <- extract_text('EventDetails-Left-55.Header')
  prizes <- extract_text('EventDetails-Right-45.Header')
  locations <- extract_text('EventDetails-Left-60.Header')
  participants <- extract_text('EventDetails-Right-40.Header')
  first_place_abbrvs <- extract_text('Placement.FirstPlace')
  second_place_abbrvs <- extract_text('Placement.SecondPlace')

  extract_title <- function(x) {
    els <- tourney_nodes %>% 
      html_elements(sprintf('.divCell.Placement.%sPlace', x))
    
    els %>% 
      map_chr(
        function(.x) {
          el <- .x %>% html_elements('.team-template-text > a')
          if(length(el) == 0) {
            return(NA_character_)
          }
          el %>%
            html_attr('title') %>% 
            str_remove(' \\(page does not exist\\)')
        }
      )
  }
  first_place_names <- extract_title('First')
  second_place_names <- extract_title('Second')
  
  tourneys <-
    tibble(
      name = names,
      url = sprintf('https://liquipedia.net%s', links),
      date = dates,
      prize = prizes,
      location = locations,
      n_participants = participants %>% str_remove('\\s+teams') %>% as.integer(),
      first_place_abbrv = first_place_abbrvs,
      second_place_abbrv = second_place_abbrvs,
      first_place_name = first_place_names,
      second_place_name = second_place_names
    )
  tourneys
}

tourney_urls <- tibble(
  tier = c('S', 'A', 'B', 'C')
) %>% 
  mutate(
    url = sprintf('https://liquipedia.net/halo/%s-Tier_Tournaments', tier)
  )

possibly_scrape_tournament <- possibly(scrape_tournament, otherwise = tibble(), quiet = FALSE)
tourneys <- tourney_urls %>% 
  mutate(tourney = map(url, possibly_scrape_tournament))

all_tourneys <- tourneys %>%
  select(tier, tourney) %>% 
  unnest(tourney)

all_tourneys$scrape_time <- scrape_time
usethis::use_data(all_tourneys, internal = FALSE, overwrite = TRUE)  
