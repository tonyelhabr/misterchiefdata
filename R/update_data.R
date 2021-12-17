
#' Update package data
#' 
#' @param scrape_time Time at which data is being updated
#' @export
update_data <- function(scrape_time = lubridate::now()) {
  
  scrape_tournaments(scrape_time = scrape_time)
}
