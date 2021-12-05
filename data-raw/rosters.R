

library(tidyverse)
library(rvest)

# pluck_element <- function(players, .css) {
#   players %>% html_elements(.css) %>% html_text2()
# }
# pluck_date_element <- function(players) {
#   players %>% 
#     html_elements('.Date') %>% 
#     html_elements('div.Date') %>% 
#     html_text2()
# }

scrape_roster <- function(team_url) {
  page <- team_urls[2] %>% read_html()
  table_elements <- page %>% html_elements('.wikitable.wikitable-striped.roster-card')
  n_tables <- length(table_elements)
  scrape_method <- 'roster_card_wikitable'
  if(n_tables == 0) {
    # it may be in an "old" format
    # there could be more than just the table(s) for rosters
    table_elements <- page %>% html_elements('.wikitable.wikitable-striped.sortable')
    n_tables <- length(table_elements)
    scrape_method <- 'generic_wikitable'
  } else {
    
    ## one for active and possibly one for former
    stopifnot(n_tables >= 1 & n_tables <= 2)
    
    # pluck_table_element <- function(i) {
    #   table_elements[i] %>% html_elements('.Player')
    # }
    # 
    # active_player_elements <- pluck_table_element(1)
    # 
    # players <- tibble(
    #   id = active_player_elements %>% pluck_element('.ID'),
    #   name = active_player_elements %>% pluck_element('.Name'),
    #   join_date = active_player_elements %>% pluck_date_element()
    # ) %>% 
    #   mutate(
    #     status = 'active',
    #     leave_date = NA_character_
    #   )
    # 
    # if(n_tables == 2) {
    #   former_player_elements <- pluck_table_element(2)
    #   dates <- former_player_elements %>% pluck_date_element()
    #   n_dates <- length(dates)
    #   idx_join <- seq(1, n_dates, by = 2)
    #   idx_leave <- seq(2, n_dates, by = 2)
    #   former_players <- tibble(
    #     id = former_player_elements %>% pluck_element('.ID'),
    #     name = former_player_elements %>% pluck_element('.Name'),
    #     join_date = dates[idx_join],
    #     leave_date = dates[idx_leave]
    #   ) %>% 
    #     mutate(status = 'former')
    #   
    #   players <- bind_rows(
    #     players,
    #     former_players
    #   )
    # }
    # players
  }
  
  pluck_table <- function(i) {
    table_init <- table_elements[i] %>% html_table() %>% pluck(1) %>% janitor::remove_empty(which = 'cols')
    first_row <- table_init[1, ] %>% c() %>% unname() %>% unlist()
    tb <- table_init %>% 
      set_names(first_row) %>% 
      slice(c(2:n())) %>% 
      janitor::clean_names()
  }
  
  1:pmin(n_tables, 2) %>% 
    map_dfr(pluck_table)
}

get_team_url <- function(team_name) {
  sprintf(
    'https://liquipedia.net/halo/%s', str_replace_all(team_name, ' ', '_')
  )
}

team_urls <- 
  tibble(
    team = 
      c(
        'EUnited',
        'Kansas City Pioneers',
        'OpTic Gaming',
        'Status Quo'
      )
  ) %>% 
  mutate(team_url = get_team_url(team))
possibly_scrape_roster <- possibly(scrape_roster, otherwise = tibble())
rosters <- team_urls %>% 
  map_dfr(possibly_scrape_roster)
