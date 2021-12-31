
.get_teams_from_series_element <- function(series_element) {
  names <- series_element %>% rvest::html_elements('.name')
  idx_teams <- names %>% 
    rvest::html_attr('style') %>% 
    stringr::str_which('overflow:hidden;text-overflow:ellipsis;white-space:pre')
  names[idx_teams] %>% rvest::html_text2()
}

.parse_bracket_series_result <- function(bracket_element) {
  teams <- .get_teams_from_series_element(bracket_element)
  
  scores <- bracket_element %>% 
    rvest::html_elements('.brkts-opponent-score-inner') %>% 
    rvest::html_text2()
  
  
  n_teams <- length(teams)
  
  if((n_teams %% 2) == 1) { 
    n_teams <- n_teams - 1
    teams <- teams[1:(length(n_teams) - 1)]
    scores <- scores[1:(length(n_teams) - 1)]
  }
  
  idx_home <- seq(1, n_teams, by = 2)
  idx_away <- seq(2, n_teams, by = 2)
  tibble::tibble(
    series_type = 'bracket',
    home_team = teams[idx_home],
    away_team = teams[idx_away],
    home_w = scores[idx_home],
    away_w = scores[idx_away]
  )
}

.parse_bracket_series_matches <- function(bracket_element) {
  teams <- .get_teams_from_series_element(bracket_element)
  
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
    home_team = teams[1],
    away_team = teams[2],
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


.parse_pool_series_matches <- function(pool_element) {
  pool_element %>% .parse_bracket_series_matches()
  
}

.parse_pool_play_series_result <- function(pool_element) {
  
  teams <- pool_element %>% 
    rvest::html_elements(
      '.brkts-matchlist-opponent'
    ) %>% 
    rvest::html_attr('aria-label')
  
  scores <- pool_element %>% 
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
  
  nms <- names(info)
  if(any(nms == 'date') & !all(c('start_date', 'end_date') %in% nms)) {
    info <- info %>% 
      dplyr::mutate(
        start_date = .data$date,
        end_date = .data$date
      ) %>% 
      dplyr::select(-.data$date)
  }
  
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
  
  has_pool_play <- length(pool_elements) > 0
  if(!has_pool_play) {
    
    cli::cli_alert_info(
      sprintf('No pool play matches to scrape at %s.', url)
    )
    pool_series_matches <- tibble::tibble()
    pool_series_results <- tibble::tibble()
  } else {
    do_pool <- purrr::partial(
      .do_possibly_map_dfr_tourney_elements,
      pool_elements,
      ... =
    )
    pool_series_matches <- do_pool(.parse_bracket_series_matches)
    pool_series_results <- do_pool(.parse_bracket_series_result)
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
  bracket <- tourney %>% 
    dplyr::mutate(
      pool_series_results = list(pool_series_results),
      pool_match_results = list(pool_series_matches),
      bracket_series_results = list(bracket_series_results),
      bracket_match_results = list(bracket_series_matches)
    )
  bracket
}

possibly_scrape_bracket <- purrr::possibly(
  scrape_bracket,
  otherwise = tibble::tibble(), 
  quiet = TRUE
)

clean_brackets <- function(raw_brackets) {
  raw_brackets %>%
    dplyr::transmute(
      .data$url,
      .data$event_name,
      .data$series,
      .data$organizers,
      .data$game_version,
      .data$type,
      .data$location,
      .data$venue,
      .data$prize_pool,
      dplyr::across(.data$start_date, lubridate::ymd),
      dplyr::across(.data$end_date, lubridate::ymd),
      dplyr::across(.data$number_of_teams, as.integer),
      tier = .data$liquipedia_tier %>% .strip_tier(),
      .data$teams,
      .data$pool_series_results,
      .data$pool_match_results,
      .data$bracket_series_results,
      .data$bracket_match_results,
      .data$scrape_time
    )
}

scrape_new_brackets <- function(bracket_urls, scrape_time) {
  res <- bracket_urls %>% 
    dplyr::pull(.data$url) %>% 
    stats::setNames(., .) %>% 
    purrr::map_dfr(possibly_scrape_bracket, .id = 'url')
  res$scrape_time <- scrape_time
  res
}

## general flow for tournaments, brackets, rosters, and players:
## 1. start message
## 2. check if file exists.
## 3a. if not (2), scrape everything
## 3b. use `scrape_new_*()` function that takes a df with urls and scrape time
## 4a. if (2), importing existing
## 4b. check if there are no urls in passed in df vs. loaded df
## 4c. check if there is a difference in scrape time in `scrape_time` vs. loaded df
## 4d. scrape anything new
## 5. write results
## 6. final message

do_scrape_brackets <- function(tournaments, scrape_time, overwrite = FALSE) {
  
  cli::cli_alert_info('Scraping brackets.')
  
  brackets_exist <- file.exists(path_raw_brackets) & file.exists(path_brackets)
  
  if(!brackets_exist) {
    cli::cli_alert_info(
      sprintf('%s or %s does not exist!', path_raw_brackets, path_brackets)
    )
  }
  
  bracket_urls <- tournaments %>%
    # dplyr::filter(.data$region == 'United States' | .data$region == 'North America') %>%
    dplyr::filter(!(is.na(.data$first_place) & is.na(.data$second_place))) %>% 
    dplyr::arrange(dplyr::desc(.data$start_date))
  
  
  if(!brackets_exist | overwrite) {
    
    cli::cli_alert_info(
      'Scraping all brackets.'
    )

    raw_brackets <- bracket_urls %>% scrape_new_brackets(scrape_time)

  } else {
    existing_raw_brackets <- readr::read_rds(path_raw_brackets)
    
    ## todo: use bracket_urls here?!?
    
    new_urls <- tournaments %>% dplyr::filter(.data$scrape_time == !!scrape_time)
    
    if(nrow(new_urls) == 0) {
      cli::cli_alert_success(
        'No new brackets to scrape based on scrape time.'
      )
      return(existing_raw_brackets)
    }
    
    cli::cli_alert_info(
      'At least one new bracket to scrape:\n',
    )
    purrr::walk(
      new_urls$url,
      cli::cli_li
    )
    
    new_brackets <- new_urls %>% scrape_new_brackets(scrape_time)

    raw_brackets <- dplyr::bind_rows(
      new_brackets,
      existing_raw_brackets %>% 
        dplyr::filter(!(.data$url %in% new_urls$url))
    )
  }
  
  readr::write_rds(raw_brackets, path_raw_brackets)
  brackets <- raw_brackets %>% clean_brackets()
  readr::write_rds(brackets, path_brackets)
  cli::cli_alert_success('Done scraping brackets.')
  brackets
  
}
