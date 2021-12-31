
.clean_roster_date <- function(x, which) {
  x %>% 
    stringr::str_remove_all(sprintf('^%s\\sDate[:]\\s|\\[[0-9]+\\]$', stringr::str_to_title(which))) %>% 
    lubridate::ymd()
}

.clean_player_id <- function(x) {
  x # %>% tolower() %>% stringr::str_replace_all(' ', '_')
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
  
  # new_pluck_table <- function(i) {
  #   # i <- 2
  #   trs <- table_elements[i] %>% rvest::html_elements('tr')
  #   h <- trs %>% 
  #     purrr::map_chr(
  #       ~rvest::html_element(.x, 'th') %>% 
  #         rvest::html_text2()
  #     ) %>% 
  #     purrr::pluck(1)
  #   if(!str_detect(h, 'Active|Players')) {
  #     return(tibble::tibble())
  #   }
  # 
  #   status <- h %>% stringr::str_replace_all('(^.*)(\\s.*$)', '\\1') %>% tolower()
  #   tds <- trs %>% purrr::map(~rvest::html_elements(.x, 'td'))
  #   
  #   .parse_row <- function(j) {
  #     
  #     td <- tds[[j]]
  #     if(length(td) == 0) {
  #       return(tibble::tibble())
  #     }
  #     td1 <- td[1]
  #     id <- td1 %>% rvest::html_text2() %>% stringr::str_trim()
  #     link <- td1 %>% 
  #       rvest::html_elements('a') %>% 
  #       rvest::html_attr('href') %>% 
  #       purrr::pluck(2)
  #     
  #     player_url <- ifelse(
  #       !is.na(link),
  #       sprintf('https://liquipedia.net%s', link),
  #       NA_character_
  #     )
  #     name <- td[2] %>% rvest::html_text2()
  #     join_date <- td[3] %>% rvest::html_text2()
  #     
  #     player <- tibble::tibble(
  #       id = id,
  #       name = name,
  #       player_url = as.character(player_url),
  #       join_date = join_date
  #     )
  #     
  #     if(length(td) > 3) {
  #       player <- player %>% 
  #         dplyr::mutate(
  #           leave_date = td[4] %>% rvest::html_text2()
  #         )
  #     } 
  #     
  #     suppressWarnings(
  #       player <- player %>% 
  #         dplyr::mutate(
  #           dplyr::across(.data$id, ~tolower(.x) %>% stringr::str_replace_all(' ', '_')),
  #           status = !!status,
  #           dplyr::across(.data$name, ~stringr::str_remove_all(.x, '^\\(|\\)|\\n') %>% stringr::str_trim()),
  #           # seen an issue with the site where a date is missing the leading "2" for "20[12]x"
  #           join_date2 = .data$join_date %>% paste0('2', .) %>% .clean_roster_date('join'),
  #           dplyr::across(.data$join_date, ~.clean_roster_date(.x, 'join')),
  #           dplyr::across(.data$join_date, ~dplyr::coalesce(.x, .data$join_date2))
  #         )
  #     )
  #     
  #     if(!any(names(player) == 'leave_date')) {
  #       player <- player %>%
  #         dplyr::mutate(
  #           leave_date = lubridate::NA_Date_
  #         )
  #     }
  #     
  #     suppressWarnings(
  #       player <- player %>% 
  #         dplyr::mutate(
  #           leave_date2 = paste0('2', .data$leave_date) %>% .clean_roster_date('leave'),
  #           dplyr::across(.data$leave_date, ~.clean_roster_date(.x, 'leave')),
  #           dplyr::across(.data$leave_date, ~dplyr::coalesce(.x, .data$leave_date))
  #         )
  #     )
  #     
  #     player %>% 
  #       dplyr::select(
  #         .data$id,
  #         .data$name,
  #         .data$join_date,
  #         .data$leave_date
  #       )
  #   }
  #   
  #   rows <- seq_along(tds) %>% 
  #     purrr::map_dfr(.parse_row)
  # }
  # 
  # tbs <- seq_along(table_elements) %>% 
  #   purrr::map_dfr(new_pluck_table)
  # tbs
  
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

  suppressWarnings(
    roster <- 1:n_tables %>%
      purrr::map_dfr(pluck_table) %>%
      dplyr::mutate(scrape_method = !!scrape_method) %>%
      dplyr::mutate(
        dplyr::across(.data$id, .clean_player_id),
        status = .data$table_name %>% stringr::str_replace_all('(^.*)(\\s.*$)', '\\1') %>% tolower(),
        dplyr::across(.data$name, ~stringr::str_remove_all(.x, '^\\(|\\)')),
        # seen an issue with the site where a date is missing the leading "2" for "20[12]x"
        join_date2 = .data$join_date %>% paste0('2', .) %>% .clean_roster_date('join'),
        dplyr::across(.data$join_date, ~.clean_roster_date(.x, 'join')),
        dplyr::across(.data$join_date, ~dplyr::coalesce(.x, .data$join_date2))
      )
  )

  if(!any(names(roster) == 'leave_date')) {
    roster <- roster %>%
      dplyr::mutate(
        leave_date = lubridate::NA_Date_
      )
  }

  suppressWarnings(
    roster <- roster %>%
      dplyr::mutate(
        leave_date2 = paste0('2', .data$leave_date) %>% .clean_roster_date('leave'),
        dplyr::across(.data$leave_date, ~.clean_roster_date(.x, 'leave')),
        dplyr::across(.data$leave_date, ~dplyr::coalesce(.x, .data$leave_date))
      )
  )

  roster %>%
    dplyr::select(
      .data$id,
      .data$status,
      .data$name,
      .data$join_date,
      .data$leave_date
    )
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
  transfers
}

scrape_new_rosters <- function(teams, scrape_time) {
  res <- teams %>% 
    dplyr::pull(.data$team_url) %>% 
    stats::setNames(., .) %>% 
    purrr::map_dfr(possibly_scrape_roster, .id = 'team_url')
  res$scrape_time <- scrape_time
  res
}

do_scrape_rosters <- function(teams, scrape_time, overwrite = TRUE) {
  
  cli::cli_alert_info('Scraping rosters.')
  
  rosters_exist <- file.exists(path_rosters)
  
  if(!rosters_exist) {
    cli::cli_alert_info(
      sprintf('%s does not exist! Must scrape all rosters.', path_rosters)
    )
  }
  
  existing_teams <- teams %>% 
    dplyr::filter(!is.na(.data$team_url)) %>% 
    dplyr::anti_join(
      import_bad_urls('roster') %>% dplyr::rename(team_url = .data$url),
      by = 'team_url'
    )
  
  if(!rosters_exist | overwrite) {
    
    rosters <- existing_teams %>% scrape_new_rosters(scrape_time)
    
  } else {
    existing_rosters <- import_csv(path_rosters)
    existing_roster_urls <- existing_rosters %>% 
      dplyr::distinct(.data$team_url)
    
    new_teams <- existing_teams %>% 
      dplyr::anti_join(
        existing_roster_urls,
        by = 'team_url'
      )
    
    has_new_teams <- nrow(new_teams) > 0
    if(!has_new_teams) {
      cli::cli_alert_info(
        'No rosters to update based on teams provided.'
      )
    } else {
      cli::cli_alert_info(
        'At least one roster to update.'
      )
      purrr::walk(
        new_teams$team_url,
        cli::cli_li
      )
    }
    
    last_scrape_time <- existing_rosters %>% 
      dplyr::slice_max(.data$scrape_time, n = 1, with_ties = FALSE) %>% 
      dplyr::ungroup()
    transfers <- scrape_latest_transfers()
    
    new_transfers <- transfers %>% 
      dplyr::filter(.data$date > scrape_time)
    
    has_new_transfers <- nrow(new_transfers) > 0
    if(!has_new_teams) {
      cli::cli_alert_info(
        'No rosters to update based on transfers.'
      ) 
    } else {
      cli::cli_alert_info(
        'At least one roster based on transfers:'
      )
      purrr::walk(
        new_transfers$team_url,
        cli::cli_li
      )
    }
    
    if(!has_new_teams & !has_new_transfers) {
      return(existing_rosters) 
    }
    
    teams_to_update <- dplyr::bind_rows(
      new_transfers %>%
        dplyr::distinct(team_url = .data$old_team_url),
      new_transfers %>%
        dplyr::distinct(team_url = .data$new_team_url)
    ) %>%
      dplyr::distinct(.data$team_url) %>% 
      dplyr::arrange(.data$team_url) %>% 
      dplyr::left_join(
        teams %>% 
          dplyr::select(.data$team_url),
        by = 'team_url'
      )
    
    teams_to_update_w_urls <- teams_to_update %>% 
      dplyr::filter(!is.na(.data$team_url))
    
    has_teams_to_update <- nrow(teams_to_update_w_urls) > 0
    if(!has_teams_to_update & has_new_transfers) {
      cli::cli_alert_success(
        'There are new transfers, but they do not correspond to teams with known urls.'
      ) 
    }
    
    if(!has_new_teams & !has_teams_to_update) {
      return(existing_rosters) 
    }
    
    new_rosters <- dplyr::bind_rows(
      teams_to_update_w_urls %>% dplyr::select(.data$team_url), ## does matter if this has rows (so we stop early if it doesn't)
      new_teams %>% dplyr::select(.data$team_url) ## doesn't matter if this has 0 rows
    ) %>% 
      scrape_new_rosters(scrape_time)
    
    rosters <- dplyr::bind_rows(
      new_rosters,
      existing_rosters %>% dplyr::filter(!(.data$team_url %in% teams_to_update_w_urls$team_url))
    )
    
  }
  
  export_csv(rosters, path_rosters)
  cli::cli_alert_success('Done scraping rosters.')
  rosters
}
