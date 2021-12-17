
library(tidyverse)
library(lubridate)
library(rvest)
library(usethis)

scrape_time <- now()

.coalesce_tourney_date <- function(date) {
  date %>% 
    strptime('%b %d, %Y', tz = 'UTC') %>% 
    ymd()
}

.str_replace_tourney_date <- function(date, which = c('start', 'end')) {
  
  has_dash <- str_detect(date, '[-]')
  if(!has_dash) {
    return(date)
  }
  
  match.arg(which)
  replacement <- switch(
    which,
    'start' = '\\1 \\2, \\5',
    'end' = '\\3 \\4, \\5'
  )
  
  # date <- "Apr 29 - May 1, 2022"
  res <- str_replace(date, '^([A-Z][a-z]+)\\s([0-9]+)\\s[-]\\s([A-Z][a-z]+)\\s([0-9]+)[,]\\s(20[012][0-9]$)', replacement)
  if(!is.na(res) & res != date) {
    return(res)
  }
  
  replacement <- switch(
    which,
    'start' = '\\1 \\2, \\4',
    'end' = '\\1 \\3, \\4'
  )
  
  # Something like Sep 25 - 27, 2022
  # date <- "Sep 23 - 25, 2022"
  res <- str_replace(date, '^([A-Z][a-z]+)\\s([0-9]+)\\s[-]\\s([0-9]+)[,]\\s(20[012][0-9]$)', replacement)
  if(!is.na(res) & res != date) {
    return(res)
  }

  if(is.na(res)) {
    rlang::warn(
      sprintf('Couldn\'t coalesce `date = %s` with `which = %s`.', date, which)
    )
  }
  res
}

.clean_abbrv <- function(x) {
  y <- str_remove(x, '^TBD$')
  ifelse(y == '', NA_character_, y)
}

scrape_tournament <- function(url) {
  cat(
    sprintf('Scraping %s.', basename(url)),
    sep = '\n'
  )
  
  ## scrape ----
  page <- url %>% read_html()
  tourney_elements <- page %>% html_elements('.divTable.table-full-width.tournament-card')
  header_elements <- tourney_elements %>% html_elements('.divCell.Tournament.Header')
  games <- header_elements %>% map_chr(~.x %>% html_element('a') %>% html_attr('title'))
  name_elements <- header_elements %>% html_elements('b')
  names <- name_elements %>% html_text2()
  links <- name_elements %>% html_elements('a') %>% html_attr('href')
  extract_text <- function(x) {
    tourney_elements %>% html_elements(sprintf('.divCell.%s', x)) %>% html_text2() %>% str_trim()
  }
  dates <- extract_text('EventDetails-Left-55.Header')
  prizes <- extract_text('EventDetails-Right-45.Header')
  locations <- extract_text('EventDetails-Left-60.Header')
  participants <- extract_text('EventDetails-Right-40.Header')
  first_place_abbrvs <- extract_text('Placement.FirstPlace')
  second_place_abbrvs <- extract_text('Placement.SecondPlace')
  
  extract_title <- function(x) {
    els <- tourney_elements %>% 
      html_elements(sprintf('.divCell.Placement.%sPlace', x))
    
    els %>% 
      map_chr(
        function(.x) {
          el <- .x %>% html_elements('.team-template-text > a')
          if(length(el) == 0) {
            return(NA_character_)
          }
          el %>%
            html_attr('title')
        }
      )
  }
  first_place_names <- extract_title('First')
  second_place_names <- extract_title('Second')
  
  regions <- tourney_elements %>% 
    html_elements('.divCell.EventDetails-Left-60.Header') %>% 
    html_elements('.flag > a') %>% 
    html_attr('title')
  
  ## clean ----
  start_dates <- dates %>% map_chr(~.str_replace_tourney_date(.x, 'start')) %>% .coalesce_tourney_date()
  end_dates <- dates %>% map_chr(~.str_replace_tourney_date(.x, 'end')) %>% .coalesce_tourney_date()
  is_multi_day <- dates %>% str_detect('[-]')
  
  prizes <- prizes %>% str_remove_all('[$,]') %>% as.integer()
  n_participant <- participants %>% str_remove('\\s+teams') %>% as.integer()

  rgx_no_page <- ' \\(page does not exist\\)'
  first_place_has_page <- first_place_names %>% str_detect(rgx_no_page)
  second_place_has_page <- second_place_names %>% str_detect(rgx_no_page)
  first_place_names <- first_place_names %>% str_remove(rgx_no_page)
  second_place_names <- second_place_names %>% str_remove(rgx_no_page)

  first_place_abbrvs <- first_place_abbrvs %>% .clean_abbrv()
  second_place_abbrvs <- second_place_abbrvs %>% .clean_abbrv()

  tourneys <-
    tibble(
      name = names,
      game = games,
      url = sprintf('https://liquipedia.net%s', links),
      start_date = start_dates,
      end_date = end_dates,
      is_multi_day = is_multi_day,
      prize = prizes,
      region = regions,
      location = locations,
      n_participants = n_participant,
      first_place = first_place_names,
      second_place = second_place_names,
      first_place_abbrv = first_place_abbrvs,
      second_place_abbrv = second_place_abbrvs,
      first_place_has_page = first_place_has_page,
      second_place_has_page = second_place_has_page
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
tourneys

all_tourneys <- tourneys %>%
  select(tier, tourney) %>% 
  unnest(tourney)

all_tourneys$scrape_time <- scrape_time
use_data(all_tourneys, internal = FALSE, overwrite = TRUE)  
