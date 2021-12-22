

scrape_gamertags <- function(page = 0, sleep = 1) {

  cli::cli_alert_info(
    sprintf('Scraping page %d of gamertags from halodatahive.', page + 1)
  )
  
  resp <- httr2::request(
    'https://halodatahive.com/Player'
  ) %>%
    httr2::req_method('POST') %>%
    httr2::req_body_form(
      ## despite this form, it's returning only 15 rows.
      data = list(
        'SearchCriteria.Page' = page,
        'SearchCritera.Descending' = FALSE,
        'SearchCriteria.Orderby' = 0,
        'SearchCritera.PageSize' = 100,
        'SearchCriteria.Range' = 4,
        'SearchCriteria.Filter' = '',
        'SearchCriteria.OrganisationId' = '',
        'PageSize' = 100
      )
    ) %>%
    # httr2::req_verbose() %>% 
    httr2::req_perform()
  
  tb <- resp %>% 
    httr2::resp_body_html() %>% 
    rvest::html_table() %>% 
    purrr::pluck(1) %>% 
    dplyr::select(c(1:5)) %>% 
    dplyr::slice(c(1:15)) %>% 
    janitor::clean_names()
  Sys.sleep(sleep)
  tb
}

possibly_scrape_gamertags <- purrr::possibly(
  scrape_gamertags,
  otherwise = tibble::tibble(),
  quiet = FALSE
)

## upon last checking, there are 482 players
do_scrape_gamertags <- function(pages = 0:(ceiling(482/15)), update_time = lubridate::now(), overwrite = TRUE) {
  
  cli::cli_alert_info(
    'Scraping gamertags.'
  )
  
  if(file.exists(path_gamertags) & !overwrite) {
    cli::cli_alert_info(
      'Returning gamertags without updating.'
    )
    return(arrow::read_parquet(path_gamertags))
  }
  
  gamertags <- pages %>% 
    setNames(., .) %>% 
    purrr::map_dfr(possibly_scrape_gamertags, .id = 'page')
  
  gamertags <- gamertags %>% 
    dplyr::group_by(.data$gamertag) %>% 
    dplyr::slice_min(.data$page, with_ties = FALSE, n = 1) %>% 
    dplyr::ungroup() %>% 
    dplyr::arrange(.data$gamertag)
  
  gamertags$update_time <- update_time
  
  cli::cli_alert_success(
    'Done scraping gamertags.'
  )
  
  arrow::write_parquet(gamertags, path_gamertags)
  gamertags
}

