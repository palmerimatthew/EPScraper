#' Name Search Scrapper
#'
#' Returns a list of URLs of the players from a league stats URL.
#' @param Name A string of the desired player's name
#' @return A vector of player eliteprospects URLs with the same name
#' @export

EP_Name_Search <- function(Name) {
  website <- paste0('https://www.eliteprospects.com/search/player?q=', gsub(' ', '+', Name))
  html <- website %>%
    readLines()
  
  start <- html %>%
    grep('<table (.*) players', .) %>%
    as.numeric()
  
  end <- html %>%
    grep('</table>', .) %>%
    .[. > start] %>%
    .[1] %>%
    as.numeric()
  
  html %>%
    .[start:end] %>%
    paste(collapse = '\n') %>%
    stringr::str_match_all("<a href=\"(.*?)\"") %>%
    .[[1]] %>%
    .[, 2] %>%
    .[grep('/player/', .)]
}