

request_match_list <- function(gamertag = 'SnakeBiteFPS') {
  token <- 'tok_dev_7ehcK94MVSBZJJNyL7d17sKD9HrME17Vh462kS7FzsFmfZVGhUErKBJt2qQx5B53'
  req <- httr2::request(
    'https://infinite.halo.autocode.gg/stats/matches/list/'
  ) %>% 
    # req_auth_bearer_token(token) %>% 
    httr2::req_headers('Authorization' = paste('Bearer', token)) %>% 
    httr2::req_headers('Content-Type' = 'application/json') %>% 
    # req_headers('gamertag' = 'SnakeBiteFPS') %>% 
    # req_headers('mode' = 'custom') %>% 
    # req_headers('extended' = FALSE) %>% 
    httr2::req_body_json(
      data = list(
        'gamertag' = gamertag,
        'mode' = 'custom',
        'extended' = FALSE
      )
    ) %>%
    httr2::req_options(ssl_verifypeer = 0) %>% 
    httr2::req_options(ssl_verifyhost = 0) %>% 
    httr2::req_verbose()
  req %>% httr2::req_dry_run()
  
  resp <- req %>% 
    httr2::req_perform()
  j <- resp %>% httr2::resp_body_string() %>% jsonlite::fromJSON()
  
  d <- j[['data']] %>% dplyr::as_tibble()
  d %>% names()
  d %>% 
    # select(where(is.list)) %>% 
    dplyr::select(details) %>% 
    tidyr::hoist(details, 'category' = list('category', 1))
  tidyr::unnest(details)
  
  tb <- j %>% tibble::enframe()
  tb %>% 
    dplyr::filter(name == 'data') %>% 
    dplyr::select(value) %>% 
    tidyr::unnest(value)
}
