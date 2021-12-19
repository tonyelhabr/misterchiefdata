
# mosesfps lan wins sheet: https://docs.google.com/spreadsheets/d/150bwdlGKysFJHEwYOv3f3JgGCx4gOEiFa-Jf3lSdkyg/edit#gid=1475647446

suppressPackageStartupMessages(suppressWarnings({
  library(dplyr)
  library(stringr)
  library(cli)
  library(tibble)
  library(rvest)
  library(purrr)
  library(janitor)
  library(lubridate)
  library(arrow)
}))

scrape_player <- function(id) {
  player_url <- sprintf(
    'https://liquipedia.net/halo/%s/Results', 
    stringr::str_replace_all(id, '\\s', '_')
  )
  cli::cli_alert_info(
    sprintf('Scraping resuts for %s.', id)
  )
  
  player_url_exists <- player_url %>% url_exists(quiet = TRUE)
  if(!player_url_exists) {
    cli::cli_alert_warning(
      sprintf('No page for for %s.', id)
    )
    return(tibble::tibble())
  }
  page <- player_url %>% rvest::read_html()
  tb <- page %>% 
    rvest::html_table() %>%
    purrr::pluck(1)
  
  if(length(tb) == 0) {
    cli::cli_alert_warning(
      sprintf('No results for %s.', id)
    )
    return(tibble::tibble())
  }
  
  suppressWarnings(
    clean_tb <- tb %>% 
      janitor::clean_names() %>% 
      dplyr::filter(.data$date != .data$place) %>% 
      dplyr::rename(score = .data$result) %>% 
      dplyr::select(-matches('_2$')) %>% 
      dplyr::mutate(
        dplyr::across(
          .data$date,
          lubridate::ymd
        ),
        dplyr::across(
          place,
          ~stringr::str_remove_all(.x, '[A-Za-z]|\\s')
        ),
        dplyr::across(
          tier,
          ~stringr::str_replace_all(.x, '(^.*)([A-Z][-]Tier|Weekly)', '\\2')
        ),
        dplyr::across(
          score,
          list(
            team_score = ~stringr::str_remove_all(.x, '\\s+?[:].*$'),
            opponent_score = ~stringr::str_remove_all(.x, '^.*\\s[:]\\s+?')
          ),
          .names = '{.fn}'
        ),
        dplyr::across(.data$prize, ~stringr::str_remove_all(.x, '[$,]')),
        ## warnings here
        dplyr::across(
          c(.data$team_score, .data$opponent_score, .data$prize), 
          as.integer
        )
      ) %>% 
      dplyr::select(-.data$score)
  )
  opponent_elements <- page %>% rvest::html_elements('td.results-team-icon')
  
  ## could scrape team urls here, but won't end up using them since i've been using team names for ids
  opponent_teams <- opponent_elements %>% 
    purrr::map_chr(~{
      el <- rvest::html_elements(.x, 'span.team-template-team-icon')
      if(length(el) == 0) {
        return(NA_character_)
      }
      team <- el %>% rvest::html_attr('data-highlightingclass')
    }
    )
  
  n_opponents <- length(opponent_teams)
  n_results <- nrow(clean_tb)
  if(n_opponents != n_results) {
    cli::cli_alert_warning(
      sprintf(
        'Mismatch in opponent cells for %s (%d != %d). Returning results without opponent name.',
        id, 
        n_opponents, 
        n_results
      )
    )
    return(clean_tb)
  }
  final_tb <- clean_tb %>% 
    dplyr::bind_cols(tibble(opponent = opponent_teams)) %>% 
    dplyr::select(
      .data$date,
      .data$place,
      .data$tier,
      .data$tournament,
      .data$team,
      .data$opponent,
      .data$team_score,
      .data$opponent_score,
      .data$prize
    )
  final_tb
}

possibly_scrape_player <- purrr::possibly(
  scrape_player, 
  otherwise = tibble::tibble(), 
  quiet = FALSE
)


scrape_roster_players <- function(rosters) {
  ids <- rosters %>% 
    # we may miss out on some old players who have turned into something other than coaches in the future,
    # but this logic works as of 2021-12-10
    dplyr::distinct(id) %>% 
    dplyr::arrange(id)
  
  players <- ids$id %>% 
    setNames(., .) %>% 
    purrr::map_dfr(possibly_scrape_player, .id = 'id')
  players
}

do_scrape_players <- function(rosters, scrape_time, overwrite = FALSE) {
  
  cli::cli_alert_info('Scraping players.')
  
  players_exist <- file.exists(path_players)
  
  if(!players_exist) {
    cli::cli_alert_info(
      sprintf('%s does not exist! Must scrape all players.', path_players)
    )
  }
  
  if(!players_exist | overwrite) {
    
    cli::cli_alert_info(
      'Scraping all players.'
    )
    
    players <- rosters %>% scrape_roster_players()
    players$scrape_time <- scrape_time
    
  } else {
    existing_players <- arrow::read_parquet(path_players)
    new_rosters <- rosters %>% dplyr::filter(.data$scrape_time == !!scrape_time)
    
    if(nrow(new_rosters) == 0) {
      cli::cli_alert_success(
        'No new players to scrape.'
      )
      return(existing_players)
    }
    
    new_players <- new_rosters %>% scrape_roster_players()
    new_players$scrape_time <- scrape_time
    players <- dplyr::bind_rows(
      new_players,
      existing_players %>% dplyr::filter(!(.data$id %in% new_players$id))
    )
  }
  
  arrow::write_parquet(players, path_players)
  cli::cli_alert_success('Done scraping players.')
  players
  
}

