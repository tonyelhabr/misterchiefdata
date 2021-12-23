

request_match_list <- function(gamertag = 'SnakeBiteFPS') {
  
  token <- 'tok_dev_7ehcK94MVSBZJJNyL7d17sKD9HrME17Vh462kS7FzsFmfZVGhUErKBJt2qQx5B53'
  
  req <- httr2::request(
    'https://infinite.halo.autocode.gg/stats/matches/list/'
  ) %>% 
    # req_auth_bearer_token(token) %>% 
    httr2::req_headers('Authorization' = paste('Bearer', token)) %>% 
    httr2::req_headers('Content-Type' = 'application/json') %>% 
    httr2::req_body_json(
      data = list(
        'gamertag' = gamertag,
        'mode' = 'custom',
        'extended' = FALSE
      )
    ) %>%
    httr2::req_options(
      'ssl_verifypeer' = 0, 
      'ssl_verifyhost' = 0
    )
  
  resp <- req %>% httr2::req_perform()
  
  j <- resp %>% 
    httr2::resp_body_string() %>% 
    jsonlite::fromJSON()
  
  match_list <- j[['data']] %>% dplyr::as_tibble()
  match_list
}

.pluck_tibble <- function(data, col) {
  data %>% 
    dplyr::select(!!rlang::enquo(col)) %>% 
    purrr::pluck(1) %>% 
    tibble::as_tibble()
}

butcher_match_list <- function(match_list) {

  details <- match_list %>% .pluck_tibble(.data$details)
  player <- match_list %>% .pluck_tibble(.data$player)
  duration <- match_list %>% .pluck_tibble(.data$duration)
  
  details_map <- details %>% .pluck_tibble(.data$map)
  details_category <- details %>% .pluck_tibble(.data$category)

  player_stats <- player %>% .pluck_tibble(.data$stats)
  player_stats_core <- player_stats %>% .pluck_tibble(.data$core)
  player_stats_core_summary <- player_stats_core %>% .pluck_tibble(.data$summary)
  player_stats_core_damage <- player_stats_core %>% .pluck_tibble(.data$damage)
  player_stats_core_kdr <- player_stats_core %>% .pluck_tibble(.data$kdr)
  
  
  
  dplyr::bind_cols(
    match_list %>% 
      dplyr::select(
        match_id = .data$id,
        mode = .data$experience,
        .data$played_at
      ),
    details_map %>% dplyr::select(map = .data$name),
    details_category %>% dplyr::select(category = .data$name),
    player_stats_core_summary %>% 
      dplyr::select(.data$kills, .data$deaths, .data$assists),
    player_stats_core_kdr %>% 
      dplyr::select(kdr = .data$value),
    player_stats_core_damage %>% 
      dplyr::select(damage_taken = .data$taken, damage_dealt = .data$dealt),
    duration %>% 
      dplyr::select(gametime_duration = .data$seconds, actual_duration = .data$human)
  )
  
}
