
.clean_roster_date <- function(x, which) {
  x %>% 
    stringr::str_remove_all(sprintf('^%s\\sDate[:]\\s|\\[[0-9]+\\]$', stringr::str_to_title(which))) %>% 
    lubridate::ymd()
}

scrape_roster <- function(url) {
  team <- url %>% 
    stringr::str_remove_all('^.*\\/') %>% 
    stringr::str_replace_all('_', ' ')
  
  cli::cli_alert_info(
    sprintf('Scraping roster for %s.', team)
  )
  
  page <- url %>% rvest::read_html()
  table_elements <- page %>% rvest::html_elements('.wikitable.wikitable-striped.roster-card')
  n_tables <- length(table_elements)
  scrape_method <- 'roster_card_wikitable'
  
  if(n_tables == 0) {
    # it may be in an "old" format
    # there could be more than just the table(s) for rosters
    table_elements <- page %>% rvest::html_elements('.wikitable.wikitable-striped.sortable')
    n_tables <- length(table_elements)
    scrape_method <- 'generic_wikitable'
    if(n_tables == 0) {
      cli::cli_abort(
        'Number of tables must be greater than 1.'
      )
    }
  }
  
  pluck_table <- function(i) {
    table_init <- table_elements[i] %>% 
      rvest::html_table() %>% 
      purrr::pluck(1) %>% 
      janitor::remove_empty(which = 'cols')
    
    old_names <- table_init %>% names()
    
    if(!any(old_names[1] == c('Active Squad', 'Former Players'))) {
      return(tibble::tibble())
    }
    
    if(nrow(table_init) == 1) {
      is_id <- table_init[1, 1] == 'ID'
      if(!is_id) {
        cli::cli_alert_warning(
          'Encountered something unexpected with a 1-row table.'
        )
      }
      cli::cli_alert_info(
        'Skipping an empty table.'
      )
      return(tibble::tibble())
    }
    
    first_row <- table_init[1, ] %>% c() %>% unname() %>% unlist()
    tb <- table_init %>% 
      stats::setNames(first_row) %>% 
      dplyr::slice(c(2:dplyr::n())) %>% 
      janitor::clean_names() %>% 
      dplyr::mutate(
        table_name = old_names[1]
      )
  }
  
  roster_init <- 1:n_tables %>% 
    purrr::map_dfr(pluck_table)
  
  # example: https://liquipedia.net/halo/Wrath 
  if(nrow(roster_init) == 0) {
    cli::cli_alert_info(
      sprintf('No tables for %s.', team)
    )
    return(tibble::tibble())
  }
  
  roster <- 1:n_tables %>% 
    purrr::map_dfr(pluck_table) %>% 
    dplyr::mutate(scrape_method = !!scrape_method) %>%  
    dplyr::transmute(
      dplyr::across(.data$id, ~tolower(.x) %>% stringr::str_replace_all(' ', '_')),
      status = .data$table_name %>% stringr::str_replace_all('(^.*)(\\s.*$)', '\\1') %>% tolower(),
      dplyr::across(.data$name, ~stringr::str_remove_all(.x, '^\\(|\\)')),
      dplyr::across(.data$join_date, ~.clean_roster_date(.x, 'join')), # warnings with g2 are fine
      dplyr::across(.data$leave_date, ~.clean_roster_date(.x, 'leave')), # seen an issue with the site where a date is missing the leading "2" for "20[12]x"
      .data$scrape_method
    )
  roster
}

possibly_scrape_roster <- purrr::possibly(
  scrape_roster,
  otherwise = tibble::tibble(),
  quiet = FALSE
)

.scrape_team_template_element <- function(element) {
  rvest::html_elements(element, 'span.team-template-team-icon')
}

.scrape_team_info <- function(element) {
  el <- .scrape_team_template_element(element)
  if(length(el) == 0) {
    return(tibble::tibble(team = NA_character_, url = NA_character_))
  }
  team <- el %>% rvest::html_attr('data-highlightingclass')
  ## turns out that these urls are bogus, so don't rely on them (some of them may work, but not all of them)
  link <- el %>% rvest::html_element('a') %>% rvest::html_attr('href')
  if(!is.na(link)) {
    url <- sprintf('https://liquipedia.net%s', link)
  }
  url <- sprintf('https://liquipedia.net%s', link)
  tibble::tibble(
    team = team,
    url = url
  )
}

scrape_latest_transfers <- function() {
  t <- lubridate::today()
  y <- lubridate::year(t)
  m <- lubridate::month(t)
  
  q <- dplyr::case_when(
    m <= 3 ~ 1,
    m <= 6 ~ 2,
    m <= 9 ~ 3,
    m <= 12 ~ 4
  )
  url <- sprintf(
    'https://liquipedia.net/halo/Player_Transfers/%s/%s_Quarter',
    y,
    scales::ordinal(q)
  )
  page <- url %>% rvest::read_html()
  
  .extract_row_elements <- function(suffix) {
    row_elements <- page %>% 
      rvest::html_elements(sprintf('.divRow.mainpage-transfer-%s', suffix))
    
    dates <- row_elements %>% rvest::html_element('div.divCell.Date') %>% rvest::html_text2()
    names <- row_elements %>% rvest::html_element('div.divCell.Name') %>% rvest::html_text2()
    
    .do_scrape_team <- function(which = c('Old', 'New')) {
      team_elements <- row_elements %>% 
        rvest::html_element(sprintf('div.divCell.%sTeam', which))
      team_elements %>% purrr::map_dfr(.scrape_team_info)
    }
    
    old_teams <- .do_scrape_team('Old')
    new_teams <- .do_scrape_team('New')
    
    dplyr::bind_cols(
      tibble::tibble(
        transfer_type = stringr::str_remove(suffix, '[-].*$'),
        date = lubridate::ymd(dates),
        players = names,
      ),
      old_teams %>% dplyr::rename(old_team = .data$team, old_team_url = .data$url),
      new_teams %>% dplyr::rename(new_team = .data$team, new_team_url = .data$url)
    )
  }

  transfers <- c(
    'neutral',
    'to-team',
    'from-team'
  ) %>% 
    purrr::map_dfr(.extract_row_elements)
  
}

do_scrape_rosters <- function(teams, scrape_time, overwrite = TRUE) {
  
  cli::cli_alert_info('Scraping rosters.')
  
  rosters_exist <- file.exists(path_rosters)
  
  if(!rosters_exist) {
    cli::cli_alert_info(
      sprintf('%s does not exist! Must scrape all rosters.', path_rosters)
    )
  }
  
  if(!rosters_exist | overwrite) {
    
    rosters <- teams %>% 
      dplyr::filter(!is.na(.data$team_url)) %>% 
      dplyr::pull(.data$team_url) %>% 
      stats::setNames(., .) %>% 
      purrr::map_dfr(possibly_scrape_roster, .id = 'team_url')
    rosters$scrape_time <- scrape_time
  } else {
    existing_rosters <- arrow::read_parquet(path_rosters)
    last_scrape_time <- existing_rosters %>% 
      dplyr::slice_max(.data$scrape_time, n = 1, with_ties = FALSE) %>% 
      dplyr::ungroup()
    transfers <- scrape_latest_transfers()

    new_transfers <- transfers %>% 
      dplyr::filter(.data$date > scrape_time)
    
    if(nrow(new_transfers) == 0) {
      cli::cli_alert_info(
        'No rosters to update since no transfers.'
      )
      return(existing_rosters) 
    }
    
    teams_to_update <- dplyr::bind_rows(
      new_transfers %>%
        dplyr::distinct(team = .data$old_team),
      new_transfers %>%
        dplyr::distinct(team = .data$new_team)
    ) %>%
      dplyr::distinct(.data$team) %>% 
      dplyr::arrange(.data$team) %>% 
      dplyr::left_join(
        teams %>% dplyr::select(.data$team, .data$team_url),
        by = 'team'
      )
    
    teams_to_update_w_urls <- teams_to_update %>% 
      dplyr::filter(!is.na(.data$team_url))
    
    if(nrow(teams_to_update_w_urls) == 0) {
      cli::cli_alert_success(
        'No to update since there are no new transfers.'
      )
      return(existing_rosters) 
    } else {
      
      new_rosters <- teams_to_update_w_urls %>% 
        dplyr::pull(.data$team_url) %>% 
        stats::setNames(., .) %>% 
        purrr::map_dfr(possibly_scrape_roster, .id = 'team_url')
      
      new_rosters$scrape_time <- scrape_time
      
      rosters <- dplyr::bind_rows(
        new_rosters,
        existing_rosters %>% dplyr::filter(!(.data$team_url %in% teams_to_update_w_urls$team_url))
      )
    }
    
  }
  
  arrow::write_parquet(rosters, path_rosters)
  cli::cli_alert_success('Done scraping rosters.')
  rosters
}
