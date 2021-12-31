
.coalesce_tourney_date <- function(date) {
  date %>% 
    strptime('%b %d, %Y', tz = 'UTC') %>% 
    lubridate::ymd()
}

.str_replace_tourney_date <- function(date, which = c('start', 'end')) {
  
  has_dash <- stringr::str_detect(date, '[-]')
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
  res <- stringr::str_replace(
    date, 
    '^([A-Z][a-z]+)\\s([0-9]+)\\s[-]\\s([A-Z][a-z]+)\\s([0-9]+)[,]\\s(20[012][0-9]$)', 
    replacement
  )
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
  res <-
    stringr::str_replace(
      date,
      '^([A-Z][a-z]+)\\s([0-9]+)\\s[-]\\s([0-9]+)[,]\\s(20[012][0-9]$)',
      replacement
    )
  if (!is.na(res) & res != date) {
    return(res)
  }
  
  if(is.na(res)) {
    cli::cli_alert_warning(
      sprintf('Couldn\'t coalesce `date = %s` with `which = %s`.', date, which)
    )
  }
  res
}

.clean_abbrv <- function(x) {
  y <- stringr::str_remove(x, '^TBD$')
  ifelse(y == '', NA_character_, y)
}

.get_valid_tournament_tiers <- function() {
  c('Recent', 'S', 'A', 'B', 'C')
}

.validate_tournament_tier <- function(x = .get_valid_tournament_tiers(), ...) {
  match.arg(x, ...)
}

.strip_tier <- function(x) {
  ## make it just S instead of S-Tier or S-Tier Tournaments
  x %>%
    stringr::str_remove('[-]Tier') %>% 
    stringr::str_remove(' Tournaments')
}

scrape_tournament <- function(tier = .get_valid_tournament_tiers()) {
  
  .validate_tournament_tier(tier)
  cli::cli_alert_info(
    sprintf('Scraping %s tournaments.', ifelse(tier == 'Recent', tier, sprintf('%s-tier', tier))),
  )
  
  url <- ifelse(
    tier == 'Recent', 
    'https://liquipedia.net/halo/Recent_Tournament_Results', 
    sprintf('https://liquipedia.net/halo/%s-Tier_Tournaments', tier)
  )
  
  ## scrape
  page <- url %>% rvest::read_html()
  hn <- ifelse(tier == 'Recent', '2', '3')
  titles_init <- page %>% rvest::html_elements(sprintf('h%s .mw-headline', hn)) %>% rvest::html_text2()
  if(tier == 'Recent') {
    titles_init <- titles_init %>% .strip_tier()
  }
  tourney_elements <- page %>% rvest::html_elements('.divTable.table-full-width.tournament-card')
  
  lengths <- tourney_elements %>% 
    purrr::map_int(
      ~rvest::html_elements(.x, '.divCell.Tournament.Header') %>% 
        length()
    )
  
  titles <- purrr::map2(titles_init, lengths, rep) %>% unlist()
  
  header_elements <- tourney_elements %>% rvest::html_elements('.divCell.Tournament.Header')
  
  games <- header_elements %>% 
    purrr::map_chr(
      ~.x %>% 
        rvest::html_element('a') %>% 
        rvest::html_attr('title')
    )
  name_elements <- header_elements %>% rvest::html_elements('b')
  names <- name_elements %>% rvest::html_text2()
  links <- name_elements %>% rvest::html_elements('a') %>% rvest::html_attr('href')
  extract_text <- function(x) {
    tourney_elements %>% 
      rvest::html_elements(sprintf('.divCell.%s', x)) %>% 
      rvest::html_text2() %>% 
      stringr::str_trim()
  }
  dates <- extract_text('EventDetails-Left-55.Header')
  prizes <- extract_text('EventDetails-Right-45.Header')
  locations <- extract_text('EventDetails-Left-60.Header')
  participants <- extract_text('EventDetails-Right-40.Header')
  first_place_abbrvs <- extract_text('Placement.FirstPlace')
  second_place_abbrvs <- extract_text('Placement.SecondPlace')
  
  extract_title <- function(x) {
    els <- tourney_elements %>% 
      rvest::html_elements(sprintf('.divCell.Placement.%sPlace', x))
    
    els %>% 
      purrr::map_chr(
        function(.x) {
          el <- .x %>% rvest::html_elements('.team-template-text > a')
          if(length(el) == 0) {
            return(NA_character_)
          }
          el %>%
            rvest::html_attr('title')
        }
      )
  }
  first_place_names <- extract_title('First')
  second_place_names <- extract_title('Second')
  
  regions <- tourney_elements %>% 
    rvest::html_elements('.divCell.EventDetails-Left-60.Header') %>% 
    rvest::html_elements('.flag > a') %>% 
    rvest::html_attr('title')
  
  ## clean
  start_dates <- dates %>%
    purrr::map_chr(
      ~.str_replace_tourney_date(.x, 'start')
    ) %>% 
    .coalesce_tourney_date()
  end_dates <- dates %>% 
    purrr::map_chr(
      ~.str_replace_tourney_date(.x, 'end')
    ) %>% 
    .coalesce_tourney_date()
  is_multi_day <- dates %>% stringr::str_detect('[-]')
  
  prizes <- prizes %>% 
    stringr::str_remove_all('[$,]') %>% 
    as.integer()
  n_participant <- participants %>% 
    stringr::str_remove('\\s+teams') %>% 
    as.integer()
  
  rgx_no_page <- ' \\(page does not exist\\)'
  first_place_has_page <- first_place_names %>% stringr::str_detect(rgx_no_page)
  second_place_has_page <- second_place_names %>% stringr::str_detect(rgx_no_page)
  first_place_names <- first_place_names %>% stringr::str_remove(rgx_no_page)
  second_place_names <- second_place_names %>% stringr::str_remove(rgx_no_page)
  
  first_place_abbrvs <- first_place_abbrvs %>% .clean_abbrv()
  second_place_abbrvs <- second_place_abbrvs %>% .clean_abbrv()
  
  tournaments <-
    tibble::tibble(
      title = titles,
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
  tournaments
}

possibly_scrape_tournament <- purrr::possibly(
  scrape_tournament, 
  otherwise = tibble::tibble(), 
  quiet = FALSE
)

scrape_new_tournaments <- function(tournament_urls, scrape_time) {
  res <- tournament_urls %>% 
    dplyr::distinct(.data$tier) %>% 
    dplyr::pull(.data$tier) %>% 
    stats::setNames(., .) %>% 
    purrr::map_dfr(possibly_scrape_tournament, .id = 'tier')
  res$scrape_time <- scrape_time
  res
}

do_scrape_tournaments <- function(scrape_time, overwrite = FALSE) {
  
  cli::cli_alert_info('Scraping tournaments.')
  
  letter_tiers <- c('S', 'A', 'B', 'C')
  tournaments_exist <- file.exists(path_tournaments)
  
  if(!tournaments_exist) {
    cli::cli_alert_info(
      sprintf('%s does not exist! Must scrape all tournament urls.', path_tournaments)
    )
  }
  
  if(!tournaments_exist | overwrite) {
    
    cli::cli_alert_info(
      'Scraping all tournament urls.'
    )
    
    tournaments <- tibble::tibble(tier = !!letter_tiers) %>%
      scrape_new_tournaments(scrape_time)

    
  } else {
    existing_tournaments <- import_csv(path_tournaments)
    
    finished_tournaments <- existing_tournaments %>% 
      dplyr::filter(!(is.na(.data$first_place) | is.na(.data$second_place)))
    
    future_tournaments <- existing_tournaments %>% 
      dplyr::filter(.data$end_date > !!scrape_time)
    
    open_brackets <- existing_tournaments %>% 
      dplyr::filter(
        basename(.data$url) == 'Open_Bracket'
      )
    
    other_tournaments <- existing_tournaments %>% 
      dplyr::anti_join(
        dplyr::bind_rows(
          finished_tournaments %>% dplyr::distinct(.data$url),
          future_tournaments %>% dplyr::distinct(.data$url),
          open_brackets %>% dplyr::distinct(.data$url)
        ),
        by = 'url'
      )
    
    if(nrow(other_tournaments) > 0) {
      cli::cli_alert_warning(
        'At least one tournament cannot be classified as finished or in the future:'
      )
      purrr::walk(
        other_tournaments$url,
        cli::cli_li
      )
    }    
    
    recent_tournaments <- possibly_scrape_tournament('Recent')
    new_recent_tournaments <- recent_tournaments %>% 
      dplyr::filter(.data$title %in% !!letter_tiers) %>% 
      dplyr::distinct(tier = .data$title, .data$url) %>% 
      dplyr::anti_join(
        import_bad_urls('tournament'),
        by = 'url'
      ) %>%
      dplyr::anti_join(
        finished_tournaments %>% 
          dplyr::distinct(.data$tier, .data$url),
        by = 'url'
      )
    
    if(nrow(new_recent_tournaments) == 0) {
      cli::cli_alert_info(
        'No new tournaments to scrape.'
      )
      return(existing_tournaments)
    }
    
    cli::cli_alert_info(
      'At least one new tournament to scrape:\n',
    )
    purrr::walk(
      new_recent_tournaments$url,
      cli::cli_li
    )

    new_tournaments <- new_recent_tournaments %>% 
      scrape_new_tournaments(scrape_time) %>% 
      dplyr::filter(.data$url %in% new_recent_tournaments$url)

    tournaments <- dplyr::bind_rows(
      new_tournaments,
      existing_tournaments %>% 
        ## do need this filter because scraping by tiers re-scrapes some tournaments
        dplyr::filter(!(.data$url %in% new_recent_tournaments$url))
    )

  }
  
  export_csv(tournaments, path_tournaments)
  cli::cli_alert_success('Done scraping tournaments.')
  tournaments
}

