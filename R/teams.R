
.distinctly_select_teams <- function(data) {
  n_tournaments <- dplyr::bind_rows(
    data %>%
      dplyr::distinct(team = .data$home_team, .data$url),
    data %>%
      dplyr::distinct(team = .data$away_team, .data$url)
  ) %>%
    dplyr::distinct(.data$team, .data$url) %>% 
    dplyr::count(.data$team, name = 'n_tournaments', sort = TRUE)
  
  n <- dplyr::bind_rows(
    data %>%
      dplyr::count(team = .data$home_team),
    data %>%
      dplyr::count(team = .data$away_team)
  ) %>%
    dplyr::group_by(.data$team) %>%
    dplyr::summarize(
      dplyr::across(.data$n, sum)
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::arrange(dplyr::desc(.data$n)) %>% 
    dplyr::select(.data$team, .data$n)
  
  dplyr::full_join(
    n_tournaments,
    n,
    by = 'team'
  )
}

do_update_teams <- function(brackets, update_time) {
  
  cli::cli_alert_info('Updating teams.')
  
  teams_exist <- file.exists(path_teams)
  
  if(teams_exist) {
    existing_teams <- import_csv(path_teams)
  }

  .select_unnest <- function(col) {
    col <- rlang::enquo(col)
    brackets %>% 
      dplyr::select(.data$url, !!col) %>% 
      tidyr::unnest(!!col)
  }
  
  series_results <- dplyr::bind_rows(
    .select_unnest('bracket_series_results'),
    .select_unnest('pool_series_results')
  )
  match_results <- dplyr::bind_rows(
    .select_unnest('bracket_match_results'),
    .select_unnest('pool_match_results')
  )
  
  teams_init <- brackets %>% 
    dplyr::select(tourney_url = .data$url, .data$teams) %>% 
    dplyr::mutate(
      teams = purrr::map(.data$teams, ~dplyr::mutate(.x, dplyr::across(dplyr::everything(), as.character)))
    ) %>% 
    tidyr::unnest(cols = .data$teams) %>% 
    dplyr::rename(team_url = .data$url, url = .data$tourney_url) %>% 
    dplyr::count(.data$team, .data$team_url, name = 'n_tournaments', sort = TRUE)
  
  
  series_teams <- series_results %>% 
    .distinctly_select_teams() %>% 
    dplyr::rename(
      n_series = .data$n, 
      n_tournaments_w_series = .data$n_tournaments
    )
  
  matches_teams <- match_results %>% 
    .distinctly_select_teams() %>% 
    dplyr::select(
      .data$team,
      n_matches = .data$n
    )
  
  teams <- teams_init %>% 
    dplyr::full_join(
      series_teams,
      by = 'team'
    ) %>% 
    dplyr::full_join(
      matches_teams,
      by = 'team'
    ) %>% 
    dplyr::select(
      .data$team,
      .data$team_url,
      .data$n_series,
      .data$n_matches,
      .data$n_tournaments,
      .data$n_tournaments_w_series
    )
  
  teams$update_time <- update_time
  readr::write_csv(teams, path_teams, na = '')
  
  if(teams_exist) {
    teams_join <- teams %>% 
      dplyr::anti_join(
        teams %>% dplyr::select(-.data$update_time),
        existing_teams %>% dplyr::select(-.data$update_time), 
        by = c('team', 'team_url', 'n_series', 'n_matches', 'n_tournaments', 'n_tournaments_w_series')
      )
    
    if(nrow(teams_join) > 0) {
      cli::cli_alert_info(
        'At least one difference detected between updated teams df and old teams df.'
      )
    } else {
      cli::cli_alert_info(
        'No teams updated.'
      )
    }
  }
  
  cli::cli_alert_success('Done updating teams.')
  teams
}
