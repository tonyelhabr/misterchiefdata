
do_insert_player_derived_brackets <- function(players, scrape_time) {
  existing_raw_brackets <- readr::read_rds(path_raw_brackets)
  existing_tournaments <- import_csv(path_tournaments)
  player_tournament_urls <- players %>%
    dplyr::select(.data$tournaments) %>% 
    tidyr::unnest(.data$tournaments) %>% 
    dplyr::distinct(.data$url) %>% 
    dplyr::filter(!is.na(.data$url))
  
  new_bracket_urls <- player_tournament_urls %>% 
    dplyr::anti_join(
      existing_raw_brackets %>% 
        dplyr::distinct(.data$url),
      by = 'url'
    )

  if(nrow(new_bracket_urls) == 0) {
    cli::cli_alert_info(
      'No new player-derived tournaments found.'
    )
    return(existing_raw_brackets)
  }
  
  cli::cli_alert_warning(
    'At least one tournament cannot be classified as finished or in the future:'
  )
  purrr::walk(
    new_bracket_urls$url,
    cli::cli_li
  )
  
  new_raw_brackets <- new_bracket_urls %>% scrape_new_brackets(scrape_time)

  all_raw_brackets <- dplyr::bind_rows(
    existing_raw_brackets,
    new_raw_brackets
  ) %>% 
    ## don't think i need this, but whatever
    dplyr::group_by(.data$url) %>% 
    dplyr::slice_max(.data$scrape_time, n = 1, with_ties = FALSE) %>% 
    dplyr::ungroup()
  
  readr::write_rds(all_raw_brackets, path_raw_brackets)
  all_brackets <- all_raw_brackets %>% clean_brackets()
  readr::write_rds(all_brackets, path_brackets)
  cli::cli_alert_success('Done scraping brackets.')
  all_brackets
}

## i don't think i need to do this at all. the tournaments csv shouldn't be used. instead, the
## tournament urls should be inferred from brackets
# update_tournaments <- function(raw_brackets = NULL, tournaments = NULL) {
#   if(is.null(brackets)) {
#     raw_brackets <- readr::read_rds(path_raw_brackets)
#   }
#   if(is.null(tournaments)) {
#     tournaments <- import_csv(path_tournaments)
#   }
#   
#   new_tournament_urls <- raw_brackets %>% 
#     dplyr::distinct(.data$url) %>% 
#     dplyr::anti_join(
#       tournaments %>% 
#         dplyr::distinct(.data$url),
#       by = 'url'
#     )
#   ## todo...
# }

