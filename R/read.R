
#' @importFrom memoise memoise
.import_bad_urls <- memoise::memoise({function() {
  import_csv('data-raw/bad_urls.csv')
}})

import_bad_urls <- function(entity) {
  bad_urls <- .import_bad_urls()
  bad_urls %>% 
    dplyr::filter(.data$entity == !!entity) %>% 
    dplyr::select(-.data$entity)
}

import_csv <- function(..., .show_col_types = FALSE) {
  readr::read_csv(show_col_types = .show_col_types, ...)
}

export_csv <- function(..., .na = '') {
  readr::write_csv(na = .na, ...)
}

# Reference: https://github.com/nflverse/nflreadr/blob/main/R/from_url.R
rds_from_url <- function(url) {
  con <- base::url(url)
  on.exit(close(con))
  load <- try(readRDS(con), silent = TRUE)
  
  if (inherits(load, 'try-error')) {
    warning(paste0('Failed to `readRDS()` from <', url, '>'), call. = FALSE)
    return(tibble::tibble())
  }
  tibble::as_tibble(load)
}

base_gh_path <- 'https://github.com/tonyelhabr/halo-data/raw/master/data'
#' Read in professional Halo bracket data
#' 
#' @export
#' @source <https://liquipedia.net>
read_halo_brackets <- function() {
  url <- sprintf('%s/brackets.rds', base_gh_path)
  res <- url %>% rds_from_url()
  cli::cli_alert_success(
    sprintf('Imported data from %s.', url)
  )
  res
}

read_gh_csv <- function(x) {
  url <- sprintf('%s/%s.csv', base_gh_path, x)
  res <- url %>% import_csv()
  cli::cli_alert_success(
    sprintf('Imported data from %s.', url)
  )
  res
}

#' Read in professional Halo player data
#' 
#' @export
#' @source <https://liquipedia.net>
read_halo_players <- function() {
  read_gh_csv('players')
}

#' Read in professional Halo roster data
#' 
#' @export
#' @source <https://liquipedia.net>
read_halo_rosters <- function() {
  read_gh_csv('rosters')
}

#' Read in professional Halo team data
#' 
#' @export
#' @source <https://liquipedia.net>
read_halo_teams <- function() {
  read_gh_csv('teams')
}

#' Read in professional Halo team data
#' 
#' @export
#' @source <https://halodatahive.com>
read_halo_gamertags <- function() {
  read_gh_csv('gamertags')
}
