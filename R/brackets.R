
.get_team_names <- function(series_element) {
  names <- series_element %>% rvest::html_elements('.name')
  idx_team_names <- names %>% 
    rvest::html_attr('style') %>% 
    stringr::str_which('overflow:hidden;text-overflow:ellipsis;white-space:pre')
  names[idx_team_names] %>% rvest::html_text2()
}

.parse_bracket_series_result <- function(bracket_element) {
  team_names <- .get_team_names(bracket_element)

  series_matches_won <- bracket_element %>% 
    rvest::html_elements('.brkts-opponent-score-inner') %>% 
    rvest::html_text2()
  
  tibble::tibble(
    side = c('home', 'away'),
    team = team_names,
    w = series_matches_won
  ) %>% 
    tidyr::pivot_wider(
      names_from = .data$side,
      values_from = c(.data$team, .data$w),
      names_glue = '{side}_{.value}'
    ) %>% 
    dplyr::mutate(
      series_result = 'bracket'
    ) %>% 
    dplyr::relocate(series_result)
}

.parse_bracket_series_matches <- function(bracket_element) {
  team_names <- .get_team_names(bracket_element)
  
  popup <- bracket_element %>% rvest::html_elements('.brkts-popup.brkts-match-info-popup')
  popup_matches <- popup %>% rvest::html_elements('.brkts-popup-body-element.brkts-popup-body-game')
  popup_divs <- popup_matches %>% rvest::html_elements('div > div')
  match_text <- popup_divs %>% 
    rvest::html_text() %>% 
    stringr::str_trim()
  match_modes <- popup_divs %>%
    rvest::html_element('a') %>%
    rvest::html_attr('title') %>% 
    purrr::discard(is.na)
  
  n_matches <- length(match_text) / 3
  matches_wide <- tibble::tibble(
    match = rep(1:n_matches, each = 3),
    home_team = team_names[1],
    away_team = team_names[2],
    text = match_text,
    name = rep(c('home_score', 'map', 'away_score'), n_matches)
  ) %>% 
    tidyr::pivot_wider(
      names_from = .data$name,
      values_from = .data$text
    )
  
  # will get warnings if results are just "W" and "L"
  suppressWarnings(
    matches_wide <- matches_wide %>% 
      dplyr::mutate(
        is_integerish = stringr::str_detect(.data$home_score, '[0-9]'),
        winner = dplyr::case_when(
          .data$home_score == 'W' ~ .data$home_team,
          .data$home_score == 'L' ~ .data$away_team,
          .data$home_score == '' ~ NA_character_,
          as.integer(.data$home_score) > as.integer(.data$away_score) ~ .data$home_team,
          as.integer(.data$home_score) < as.integer(.data$away_score) ~ .data$away_team,
          TRUE ~ NA_character_
        ),
        dplyr::across(c(.data$home_score, .data$away_score), as.integer)
      )
  )
  matches_wide <- matches_wide %>%
    dplyr::select(
      .data$match,
      .data$map,
      .data$home_team,
      .data$away_team,
      .data$winner,
      .data$home_score,
      .data$away_score
    )
  
  first_match_w_result <- matches_wide %>% 
    dplyr::filter(!is.na(.data$winner)) %>% 
    dplyr::slice_min(.data$match, n = 1, with_ties = FALSE) %>% 
    dplyr::pull(.data$match)
  
  matches_wide %>% 
    dplyr::mutate(
      ## there are some cases when a game 1 or 2 is missing, and a later result in the series is not missing
      ## a series always has a row for all possible matches, so we can infer if a game should have a result
      ## by first checking if the match index is less than the total possible number of matches divided by 2
      ## and rounded up, or by checking if a later game has a result
      missing_result = dplyr::case_when(
        .data$match < ceiling(n_matches / 2) & is.na(.data$winner) ~ TRUE,
        !is.na(!!first_match_w_result) & .data$match < !!first_match_w_result ~ TRUE,
        TRUE ~ FALSE
      )
    ) %>% 
    dplyr::bind_cols(tibble::tibble(mode = match_modes)) %>% 
    # now drop unplayed matches (because series is already clinched)
    dplyr::filter(!(is.na(.data$winner) & !.data$missing_result))
}


.parse_pool_series_matches <- function(pool_elements) {
  pool_elements %>% .parse_bracket_series_matches()
  
}

.parse_pool_play_series_results <- function(pool_elements) {

  teams <- pool_elements %>% 
    rvest::html_elements(
      '.brkts-matchlist-opponent'
    ) %>% 
    rvest::html_attr('aria-label')
  
  scores <- pool_elements %>% 
    rvest::html_elements(
      '.brkts-matchlist-score'
    ) %>% 
    rvest::html_text2()
  
  n_teams <- length(teams)
  
  if((n_teams %% 2) == 1) { 
    n_teams <- n_teams - 1
    teams <- teams[1:(length(n_teams) - 1)]
    scores <- scores[1:(length(n_teams) - 1)]
  }
  
  idx_winner <- seq(1, n_teams, by = 2)
  idx_loser <- seq(2, n_teams, by = 2)
  tibble::tibble(
    series_type = 'pool',
    home_team = teams[idx_winner],
    away_team = teams[idx_loser],
    home_w = scores[idx_winner],
    away_w = scores[idx_loser]
  )
}

.parse_infobox <- function(page) {
  infobox_element <- page %>% rvest::html_elements('.fo-nttax-infobox-wrapper.infobox-halo')
  title <- infobox_element %>% 
    rvest::html_elements('.infobox-header') %>%
    purrr::pluck(1) %>% 
    rvest::html_text2() %>% 
    ## remove "[e][h]" before "HCS..."
    stringr::str_remove_all('\\[[eh]\\]')
  labels_and_values <- infobox_element %>% 
    rvest::html_elements('.infobox-cell-2') %>% 
    rvest::html_text2()
  n_text <- length(labels_and_values)
  
  ## if there is an odd number for some reason, take off the last value (e.g. https://liquipedia.net/halo/Halo_5_Pro_Series/Season_1/North_America/Championship)
  if((n_text %% 2) == 1) { 
    n_text <- n_text - 1
    labels_and_values <- labels_and_values[1:(length(n_text) - 1)]
  }
  
  idx_labels <- seq(1, n_text, by = 2)
  idx_values <- seq(2, n_text, by = 2)
  
  info <- tibble::tibble(
    label = labels_and_values[idx_labels] %>% 
      stringr::str_remove('[:]$'),
    value = labels_and_values[idx_values] %>% 
      stringr::str_trim() %>% 
      stringr::str_replace_all('\\n', ', ')
  ) %>% 
    ## resolves case found with https://liquipedia.net/halo/Halo_5_Pro_Series/Season_4/North_America/Championship
    dplyr::filter(.data$label != '') %>% 
    tidyr::pivot_wider(
      names_from = .data$label,
      values_from = .data$value
    ) %>% 
    janitor::clean_names()
  
  dplyr::bind_cols(
    tibble::tibble(event_name = title),
    info = info
  )
}

.parse_teams <- function(page) {
  team_elements <- page %>% 
    rvest::html_elements('div.teamcard') %>% 
    rvest::html_elements('center > b > a')
  
  urls <- team_elements %>% 
    rvest::html_attr('href') %>% 
    sprintf('https://liquipedia.net%s', .)
  tibble::tibble(
    team = team_elements %>% 
      rvest::html_text2(),
    # team_title = team_elements %>% 
    #   rvest::html_attr('title'),
    url = ifelse(stringr::str_detect(urls, "index[.]php?"), NA_character_, urls)
  )
}


.do_possibly_map_dfr_tourney_elements <- function(elements, f) {
  possibly_f <- purrr::possibly(f, otherwise = tibble::tibble(), quiet = TRUE)
  elements %>% 
    purrr::map_dfr(possibly_f, .id = 'series_index') %>% 
    dplyr::mutate(
      dplyr::across(.data$series_index, as.integer)
    )
}

scrape_bracket <- function(url) {
  
  cli::cli_alert_info(
    sprintf('Scraping bracket at %s.', url)
  )
  
  page <- url %>% rvest::read_html()
  infobox <- .parse_infobox(page)
  teams <- .parse_teams(page)
  
  ## this is a special case (confirmed no past tournaments have this)
  ## might need to use `url_exists` going forward.
  if(url == 'https://liquipedia.net/halo/Halo_Championship_Series/2021/Kickoff_Major') {
    pool_page <- sprintf('%s/Pool_Play', url) %>% rvest::read_html()
  } else {
    pool_page <- page
  }
  
  pool_elements <- pool_page %>% 
    rvest::html_elements(
      '.brkts-matchlist-match.brkts-match-has-details.brkts-match-popup-wrapper'
    )
  
  bracket_elements <- page %>% rvest::html_elements('.brkts-match.brkts-match-popup-wrapper')
  
  tourney <- infobox %>% 
    dplyr::mutate(
      teams = list(teams)
    )

  has_pool_play <- length(pool_elements) >= 0
  if(!has_pool_play) {
    
    cli::cli_alert_info(
      sprintf('No pool play matches to scrape at %s.', url)
    )
  } else {
    # do_pool <- purrr::partial(
    #   .do_possibly_map_dfr_tourney_elements,
    #   pool_elements,
    #   ... = 
    # )
    # pool_series_matches <- do_bracket(.parse_bracket_series_matches)
    # pool_series_results <- do_bracket(.parse_bracket_series_result)
    pool_series_matches <- .parse_pool_series_matches(
      pool_elements
    )
    pool_series_results <- .parse_pool_play_series_results(
      pool_elements
    )
  }
  
  
  if(length(bracket_elements) == 0) {
    cli::cli_alert_warning(
      sprintf('No series or matchups to scrape at %s.', url)
    )
    return(
      tourney
    )
  }
  
  do_bracket <- purrr::partial(
    .do_possibly_map_dfr_tourney_elements,
    bracket_elements,
    ... = 
  )
  
  bracket_series_matches <- do_bracket(.parse_bracket_series_matches)
  bracket_series_results <- do_bracket(.parse_bracket_series_result)
  tourney %>% 
    dplyr::mutate(
      series_results = list(series_results),
      match_results = list(series_matches)
    )
}

possibly_scrape_bracket <- purrr::possibly(
  scrape_bracket,
  otherwise = tibble::tibble(), 
  quiet = TRUE
)

do_scrape_brackets <- function(tournaments, scrape_time, overwrite = FALSE) {
  
  cli::cli_alert_info('Scraping brackets.')
  
  brackets_exist <- file.exists(path_brackets)
  
  if(!brackets_exist) {
    cli::cli_alert_info(
      sprintf('%s does not exist! Must scrape all brackets.', path_brackets)
    )
  }
  
  if(!brackets_exist | overwrite) {
    
    cli::cli_alert_info(
      'Scraping all brackets.'
    )
    
    bracket_urls <- tournaments %>%
      dplyr::filter(.data$region == 'United States' | .data$region == 'North America') %>%
      dplyr::filter(!(is.na(.data$first_place) & is.na(.data$second_place))) %>% 
      dplyr::arrange(dplyr::desc(.data$start_date))
    
    brackets <- bracket_urls$url %>% 
      stats::setNames(., .) %>% 
      purrr::map_dfr(possibly_scrape_bracket, .id = 'url')
    brackets$scrape_time <- scrape_time
  } else {
    existing_brackets <- readr::read_rds(path_brackets)
    new_urls <- tournaments %>% dplyr::filter(.data$scrape_time == !!scrape_time)
    
    if(nrow(new_urls) == 0) {
      cli::cli_alert_success(
        'No new brackets to scrape.'
      )
      return(existing_brackets)
    }
    
    new_brackets <- new_urls$url %>% 
      stats::setNames(., .) %>% 
      purrr::map_dfr(possibly_scrape_bracket, .id = 'url')
    
    new_brackets$scrape_time <- scrape_time
    brackets <- dplyr::bind_rows(
      new_brackets,
      existing_brackets %>% 
        dplyr::filter(!(.data$url %in% new_urls$url))
    )
  }
  
  readr::write_rds(brackets, path_brackets)
  cli::cli_alert_success('Done scraping brackets.')
  brackets
  
}
