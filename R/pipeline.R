
library(dplyr)
library(lubridate)

update_data <- function(
  scrape_time = lubridate::now(), 
  overwrite = FALSE
) {
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
}
