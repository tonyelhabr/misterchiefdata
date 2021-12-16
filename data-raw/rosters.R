
library(dplyr)
library(purrr)
library(stringr)
library(rvest)
library(janitor)
library(usethis)
library(rlang)
library(tidyr)
library(lubridate)
library(usethis)

scrape_time <- now()
data('all_rosters')
old_all_rosters <- all_rosters
rm('all_rosters')

get_team_url <- function(team_name) {
  team_name <- case_when(
    team_name == 'KC Pioneers' ~ 'Kansas_City_Pioneers',
    team_name == 'Spacestation' ~ 'Spacestation_Gaming',
    TRUE ~ str_replace_all(team_name, ' ', '_')
  )
  sprintf(
    'https://liquipedia.net/halo/%s', team_name
  )
}

data('all_teams', package = 'hcs')

team_urls <- all_teams %>% 
  mutate(team_url = get_team_url(team))

scrape_roster <- function(team_url) {
  team <- team_url %>% str_remove_all('^.*\\/') %>% str_replace_all('_', ' ')
  cat(
    sprintf('Scraping roster for %s.', team),
    sep = '\n'
  )

  team_url_exists <- team_url %>% url_exists()
  if(!team_url_exists) {
    return(tibble())
  }
  page <- team_url %>% read_html()
  table_elements <- page %>% html_elements('.wikitable.wikitable-striped.roster-card')
  n_tables <- length(table_elements)
  scrape_method <- 'roster_card_wikitable'
  
  if(n_tables == 0) {
    # it may be in an "old" format
    # there could be more than just the table(s) for rosters
    table_elements <- page %>% html_elements('.wikitable.wikitable-striped.sortable')
    n_tables <- length(table_elements)
    scrape_method <- 'generic_wikitable'
    stopifnot(
      "Number of tables must be greater than 1" = n_tables >= 1
    )
  } else {
    
    # if(!(n_tables >= 1 & n_tables <= 2)) {
    #   abort(
    #     sprintf("Number of tables must be between 1 and 2, not %s", n_tables)
    #   )
    # }
  }
  
  pluck_table <- function(i) {
    table_init <- table_elements[i] %>% 
      html_table() %>% 
      pluck(1) %>% 
      remove_empty(which = 'cols')
    old_names <- table_init %>% names()
    first_row <- table_init[1, ] %>% c() %>% unname() %>% unlist()
    tb <- table_init %>% 
      set_names(first_row) %>% 
      slice(c(2:n())) %>% 
      clean_names() %>% 
      mutate(
        table_name = old_names[1]
      )
  }
  
  
  res <- 1:pmin(n_tables, 2) %>% 
    map_dfr(pluck_table) %>% 
    mutate(scrape_method = !!scrape_method)

  res
}

possibly_scrape_roster <- possibly(scrape_roster, otherwise = tibble())
rosters <- team_urls %>% 
  mutate(
    roster = map(team_url, possibly_scrape_roster)
  )
rosters

clean_date <- function(x, which) {
  str_remove_all(x, sprintf('^%s\\sDate[:]\\s|\\[[0-9]+\\]$', str_to_title(which))) %>% ymd()
}

all_rosters <- rosters %>% 
  select(team, roster) %>% 
  unnest(roster) %>% 
  transmute(
    team,
    id,
    status = table_name %>% str_replace_all('(^.*)(\\s.*$)', '\\1') %>% tolower(),
    across(name, ~str_remove_all(.x, '^\\(|\\)')),
    position,
    across(join_date, ~clean_date(.x, 'join')), # warnings with g2 are fine
    across(leave_date, ~clean_date(.x, 'leave'))
  )

all_rosters$scrape_time <- scrape_time
usethis::use_data(all_rosters, internal = FALSE, overwrite = TRUE)  
