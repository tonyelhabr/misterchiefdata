
library(dplyr)
library(lubridate)

update_data <- function(scrape_time = lubridate::now(), overwrite = list(
  tournaments = FALSE,
  brackets = FALSE,
  rosters = FALSE,
  teams = TRUE,
  players = FALSE
  )) {
  scrape_time = lubridate::now(); overwrite = FALSE
  tourneys <- do_scrape_tournaments(scrape_time = scrape_time, overwrite = overwrite)
  # scrape_time <- tourneys %>% slice_max(scrape_time, with_ties = FALSE, n = 1) %>% pull(scrape_time) ## quick fix
  brackets <- tourneys %>% do_scrape_brackets(scrape_time = scrape_time, overwrite = overwrite)
  rosters <- tourneys %>% scrape_rosters(scrape_time = scrape_time, overwrite = overwrite)
  teams <- brackets %>% do_extract_teams(scrape_time = scrape_time, overwrite = TRUE)
  players <- teams %>% do_scrape_players(scrape_time = scrape_time, overwrite = FALSE)
}
