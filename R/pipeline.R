
#' Update all data
#' 
#' @param scrape_time Time that data was last scraped
#' @param overwrite Whether to overwrite all existing data
#' @export
update_data <- function(
  scrape_time = lubridate::now(), 
  overwrite = FALSE
) {
  t1 <- Sys.time()
  # scrape_time = lubridate::now(); overwrite = FALSE
  tournaments <- do_scrape_tournaments(
    scrape_time = scrape_time, 
    overwrite = overwrite
  )
  # scrape_time <- tournaments %>% slice_max(scrape_time, with_ties = FALSE, n = 1) %>% pull(scrape_time) ## quick fix
  brackets <- tournaments %>%
    do_scrape_brackets(
    scrape_time = scrape_time, 
    overwrite = overwrite
  )
  rosters <- tournaments %>% 
    do_scrape_rosters(
      scrape_time = scrape_time,
      overwrite = overwrite
    )
  teams <- brackets %>% 
    do_update_teams(
      update_time = scrape_time
    )
  players <- rosters %>% 
    do_scrape_players(
      scrape_time = scrape_time, 
      overwrite = FALSE
    )
  t2 <- Sys.time()
  cli::cli_alert_success(
    sprintf('Done updating data at %s (%s).', Sys.time(), t2 - t1)
  )
}
