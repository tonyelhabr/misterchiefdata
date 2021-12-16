
# mosesfps lan wins sheet: https://docs.google.com/spreadsheets/d/150bwdlGKysFJHEwYOv3f3JgGCx4gOEiFa-Jf3lSdkyg/edit#gid=1475647446
library(dpyr)
library(purrr)
library(stringr)
library(janitor)
library(rlang)
library(rvest)
library(usethis)

scrape_time <- now()
# players <- c(
#   # Sen
#   'Frosty',
#   'LethuL',
#   'Royal2',
#   'SnakeBite',
#   'Royal1'
#   # Optic
#   'aPG',
#   'iGotUrPistola',
#   'Lucid',
#   'TriPPPey',
#   # Incocneivable
#   'Snipedown',
#   # c9
#   'Eco',
#   'Penguin',
#   'Renegade',
#   'StelluR',
#   # eUnited
#   'Spartan',
#   'RyaNoob',
#   'KingNick',
#   'Rayne',
#   'Chig',
#   # g2
#   'Artic',
#   'Gilk3y',
#   'Sabinater',
#   'Str8 Sick'
#   
#   
#   'Elamite'
#   'Lunchbox'
#   'Ninja',
#   'Walshy',
#   'StrongSide',
#   'Tsquared'
# )

data('all_rosters')
all_ids <- all_rosters %>% 
  # we may miss out on some old players who have turned into something other than coaches in the future,
  # but this logic works as of 2021-12-10
  filter((is.na(position) | position == 'Coach')) %>% 
  distinct(id) %>% 
  arrange(id)

scrape_player <- function(id) {
  player_url <- sprintf('https://liquipedia.net/halo/%s/Results', str_replace_all(id, '\\s', '_'))
  inform(
    sprintf('Scraping resuts for %s.', id)
  )
  
  player_url_exists <- player_url %>% url_exists(quiet = TRUE)
  if(!player_url_exists) {
    warn(
      sprintf('No page for for %s.', id)
    )
    return(tibble())
  }
  page <- player_url %>% read_html()
  tb <- page %>% 
    html_table() %>%
    pluck(1)
  
  if(length(tb) == 0) {
    warn(
      sprintf('No results for %s.', id)
    )
    return(tibble())
  }
    clean_names()
  
  clean_tb <- tb %>% 
    clean_names() %>% 
    filter(date != place) %>% 
    rename(score = result) %>% 
    select(-matches('_2$')) %>% 
    mutate(
      across(
        date,
        ymd
      ),
      across(
        place,
        ~str_remove_all(.x, '[A-Za-z]|\\s')
      ),
      across(
        tier,
        ~str_replace_all(.x, '(^.*)([A-Z][-]Tier|Weekly)', '\\2')
      ),
      across(
        score,
        list(
          team_score = ~str_remove_all(.x, '\\s+?[:].*$') %>% as.integer(),
          opponent_score = ~str_remove_all(.x, '^.*\\s[:]\\s+?') %>% as.integer()
        ),
        .names = '{.fn}'
      ),
      across(prize, ~str_remove_all(.x, '[$,]') %>% as.integer())
    ) %>% 
    select(-score)
  
  opponent_nodes <- page %>% html_elements('td.results-team-icon')
  opponent_teams <- opponent_nodes %>% 
    map_chr(~{
      el <- html_elements(.x, 'span.team-template-team-icon')
      if(length(el) == 0) {
        return(NA_character_)
      }
      team <- el %>% html_attr('data-highlightingclass')
    }
    )
  
  n_opponents <- length(opponent_teams)
  n_results <- nrow(clean_tb)
  if(n_opponents != n_results) {
    warn(
      sprintf(
        'Mismatch in opponent cells for %s (%d != %d). Returning results without opponent name',
        id, 
        n_opponents, 
        n_results
      )
    )
    return(clean_tb)
  }
  final_tb <- clean_tb %>% 
    bind_cols(tibble(opponent = opponent_teams)) %>% 
    select(
      date,
      place,
      tier,
      tournament,
      team,
      opponent,
      team_score,
      opponent_score,
      prize
    )
  final_tb
}

possibly_scrape_player <- possibly(scrape_player, otherwise = tibble(), quiet = FALSE)
players <- all_ids %>% 
  mutate(
    results = map(id, possibly_scrape_player)
  )

all_players <- players %>% 
  mutate(has_results = map_int(results, nrow) > 0) %>% 
  filter(has_results) %>% 
  select(-has_results)
all_players$scrape_time <- scrape_time
use_data(all_players, internal = FALSE, overwrite = FALSE)
