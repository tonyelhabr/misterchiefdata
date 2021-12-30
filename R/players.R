
# mosesfps lan wins sheet: https://docs.google.com/spreadsheets/d/150bwdlGKysFJHEwYOv3f3JgGCx4gOEiFa-Jf3lSdkyg/edit#gid=1475647446
scrape_player <- function(id) {
  url <- sprintf(
    'https://liquipedia.net/halo/%s/Results', 
    stringr::str_replace_all(id, '\\s', '_')
  )
  cli::cli_alert_info(
    sprintf('Scraping resuts for %s.', id)
  )
  
  url_exists <- url %>% url_exists(quiet = TRUE)
  if(!url_exists) {
    cli::cli_alert_warning(
      sprintf('No page exists for %s.', id)
    )
    return(tibble::tibble())
  }
  page <- url %>% rvest::read_html()
  
  trs <- page %>% rvest::html_elements('tbody > tr')
  tds <- trs %>% purrr::map(~rvest::html_elements(.x, 'td')) %>% purrr::keep(~length(.x) == 9)
  if(length(tds) == 0) {
    cli::cli_alert_warning(
      sprintf('Page exists for %s, but there is no data.', id)
    )
    return(tibble::tibble())
  }
  
  .parse_row <- function(i) {
    date <- tds[[i]][1] %>% rvest::html_text2()
    place <- tds[[i]][2] %>% rvest::html_element('.placement-text') %>% rvest::html_text2()
    tier <- tds[[i]][3] %>% rvest::html_element('a') %>% rvest::html_text2()
    game <- tds[[i]][4] %>% rvest::html_element('a') %>% rvest::html_attr('title')
    tournament_element <- tds[[i]][5] %>% rvest::html_element('a')
    tournament_name <- tournament_element %>% rvest::html_attr('title')
    tournament_link <- tournament_element %>% rvest::html_attr('href')
    tournament_url <- ifelse(
      !is.na(tournament_link),
      sprintf('https://liquipedia.net%s', tournament_link),
      NA_character_
    )
    team <- tds[[i]][6] %>% rvest::html_text2()
    result <- tds[[i]][7] %>% rvest::html_text2()
    opponent <- tds[[i]][8] %>% 
      rvest::html_element('span.team-template-team-icon') %>% 
      rvest::html_attr('data-highlightingclass')
    prize <- tds[[i]][9] %>% rvest::html_text2()
    tibble::tibble(
      url = tournament_url,
      tournament = tournament_name,
      date = date,
      place = place,
      tier = tier,
      game = game,
      team = team,
      opponent = opponent,
      score = result,
      prize = prize
    )
  }
  rows <- seq_along(tds) %>% purrr::map_dfr(.parse_row)
  
  tb <- rows %>% 
    dplyr::mutate(
      dplyr::across(
        .data$date,
        lubridate::ymd
      ),
      dplyr::across(
        .data$place,
        ~stringr::str_remove_all(.x, '[A-Za-z]|\\s')
      ),
      dplyr::across(
        .data$tier,
        ~stringr::str_replace_all(.x, '(^.*)([A-Z][-]Tier|Weekly)', '\\2') %>% 
          stringr::str_remove('[-]Tier$')
      ),
      dplyr::across(
        .data$score,
        list(
          team_score = ~stringr::str_remove_all(.x, '\\s+?[:].*$'),
          opponent_score = ~stringr::str_remove_all(.x, '^.*\\s[:]\\s+?')
        ),
        .names = '{.fn}'
      ),
      dplyr::across(.data$prize, ~stringr::str_remove_all(.x, '[$,]'))
    )
  
  suppressWarnings(
    tb <- tb %>% 
      dplyr::mutate(
        ## warnings here
        dplyr::across(
          c(.data$team_score, .data$opponent_score, .data$prize), 
          as.integer
        )
      )
  )
  
  tb <- tb %>% 
    dplyr::select(
      .data$url,
      .data$tournament,
      .data$date,
      .data$place,
      .data$tier,
      .data$game,
      .data$team,
      .data$opponent,
      .data$team_score,
      .data$opponent_score,
      .data$prize
    )
  tb
}

possibly_scrape_player <- purrr::possibly(
  scrape_player, 
  otherwise = tibble::tibble(), 
  quiet = FALSE
)


scrape_new_players <- function(rosters, scrape_time) {
  ids <- rosters %>% 
    # we may miss out on some old players who have turned into something other than coaches in the future,
    # but this logic works as of 2021-12-10
    dplyr::distinct(.data$id) %>% 
    dplyr::arrange(.data$id)
  
  players <- ids %>% 
    dplyr::pull(.data$id) %>% 
    stats::setNames(., .) %>% 
    purrr::map_dfr(possibly_scrape_player, .id = 'id')
  players
  
  res <- ids %>% 
    dplyr::left_join(players, by = 'id')
  
  res$scrape_time <- scrape_time
  res
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
    
    players <- rosters %>% scrape_new_players(scrape_time)
    
  } else {
    existing_players <- import_csv(path_players)
    
    existing_player_ids <- existing_players %>% 
      dplyr::distinct(.data$id)
    
    roster_player_ids <- rosters %>% 
      dplyr::distinct(.data$id)
    
    new_player_ids <- roster_player_ids %>% 
      dplyr::anti_join(
        existing_player_ids,
        by = 'id'
      )
    
    has_new_players <- nrow(new_player_ids) > 0
    if(!has_new_players) {
      cli::cli_alert_info(
        'No players to update based on rosters provided.'
      )
    }
    
    new_rosters <- rosters %>% 
      dplyr::filter(.data$scrape_time == !!scrape_time)
    
    has_new_roster_players <- nrow(new_rosters) > 0
    if(!has_new_roster_players) {
      cli::cli_alert_info(
        'No players to update based on roster scrape times.'
      )
    }
    
    if(!has_new_players & !has_new_roster_players) {
      return(existing_players)
    }
    
    new_players <- dplyr::bind_rows(
      new_rosters %>% dplyr::distinct(.data$id), ## must have >0 rows
      new_player_ids %>% dplyr::distinct(.data$id) ## doesn't matter if this has 0 rows
    ) %>% 
      scrape_new_players(scrape_time)
    
    players <- dplyr::bind_rows(
      new_players,
      existing_players %>% dplyr::filter(!(.data$id %in% new_players$id))
    )
  }
  
  export_csv(players, path_players)
  cli::cli_alert_success('Done scraping players.')
  players
  
}

