
.get_valid_continents <- function() {
  c('Europe', 'North_America', 'South_America', 'Asia_Pacific', 'Middle_East_Africa')
}

.validate_continents <- function(x = .get_valid_continents(), ...) {
  match.arg(x, ...)
}

scrape_player_ids <- function(continent = .get_valid_continents()) {

  continent <- .validate_continents(continent)
  cli::cli_alert_info(
    sprintf('Scraping player ids for %s', continent)
  )
  url <- sprintf('https://liquipedia.net/halo/Portal:Players/%s', continent)
  page <- url %>% rvest::read_html()
  
  trs <- page %>% rvest::html_elements('tbody > tr')
  tds <- trs %>% purrr::map(~rvest::html_elements(.x, 'td'))
  .parse_row <- function(i) {
    # i <- 9
    td1 <- tds[[i]][1]
    id <- td1 %>% rvest::html_text2() %>% stringr::str_trim()
    a1 <- td1 %>% rvest::html_elements('a')
    link <- a1[2] %>% rvest::html_attr('href')
    player_url <- ifelse(
      !is.na(link),
      sprintf('https://liquipedia.net%s', link),
      NA_character_
    )
    name <- tds[[i]][2] %>% rvest::html_text2() %>% stringr::str_trim()
    tibble::tibble(
      id = id,
      name = name,
      player_url = as.character(player_url)
    )
  }
  rows <- seq_along(tds) %>%
    purrr::map_dfr(purrr::possibly(.parse_row, otherwise = tibble::tibble())) %>% 
    dplyr::filter(.data$id != '' & !is.na(.data$player_url)) %>% 
    dplyr::distinct(.data$id, .data$name, .data$player_url)
  rows
}


possibly_scrape_player_ids <- purrr::possibly(
  scrape_player_ids,
  otherwise = tibble::tibble(), 
  quiet = TRUE
)

do_scrape_player_ids <- memoise::memoise({function() {

  .get_valid_continents() %>% 
    setNames(., .) %>% 
    purrr::map_dfr(possibly_scrape_player_ids, .id = 'continent') %>% 
    dplyr::mutate(
      dplyr::across(.data$id, .clean_player_id)
    ) %>% 
    dplyr::group_by(.data$id) %>% 
    dplyr::slice_head(n = 1) %>% 
    dplyr::ungroup() %>% 
    dplyr::arrange(.data$id)
}})
