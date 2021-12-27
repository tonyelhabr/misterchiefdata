
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


parquet_from_url <- function(url){
  load <- try(curl::curl_fetch_memory(url), silent = TRUE)
  
  if (load$status_code != 200) {
    warning(
      paste0(
        'HTTP error',
        load$status_code,
        ' while retrieving data from <',
        url,
        '> \n Returning request payload.'
      ),
      call. = FALSE
    )
    return(tibble::tibble())
  }
  
  arrow::read_parquet(load$content)
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

read_parquet <- function(x) {
  url <- sprintf('%s/%s.parquet', base_gh_path, x)
  res <- url %>% parquet_from_url()
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
  read_parquet('players')
}

#' Read in professional Halo roster data
#' 
#' @export
#' @source <https://liquipedia.net>
read_halo_rosters <- function() {
  read_parquet('rosters')
}

#' Read in professional Halo team data
#' 
#' @export
#' @source <https://liquipedia.net>
read_halo_teams <- function() {
  read_parquet('teams')
}

#' Read in professional Halo team data
#' 
#' @export
#' @source <https://halodatahive.com>
read_halo_gamertags <- function() {
  read_parquet('gamertags')
}
