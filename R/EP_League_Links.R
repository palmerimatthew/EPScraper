#' League URL gatherer
#'
#' Reutrns a list of URLs of the players from a league stats URL.
#' @param Data the url of the league and year stats page from eliteprospects.
#' @param undrafted Boolean about whether to return only undrafted players, or any players (regardless of whether or not they were drafted).
#' @return A vector of player eliteprospects URLs
#' @export

EP_League_Links <- function(Data, undrafted = T) {
  num_pages <- Data %>%
    readLines() %>%
    .[grep('table-pagination', .)+1] %>%
    .[1] %>%
    gsub(' players found', '', .) %>%
    gsub(' ', '', .) %>%
    as.numeric() %>%
    divide_by(100) %>%
    ceiling()
  
  all_links <- character(0)
  
  for(i in 1:num_pages) {
    website <- paste0(Data, '?page=', i)
    html <- website %>%
      readLines()
    links <- html %>%
      paste(collapse = '\n') %>%
      str_match_all("<a href=\"(.*?)\"") %>%
      .[[1]] %>%
      .[-(1:300),2] %>%
      .[grep('player', .)]
    
    first_goalie_link <- html %>%
      get_EP_table('AB', 'Undrafted') %$%
      Player %>%
      .[1] %>%
      as.character() %>%
      gsub(' ', '-', .) %>%
      tolower() %>%
      grep(links) %>%
      as.numeric()
    
    links <- links[-(first_goalie_link:length(links))]
    all_links <- c(all_links, links)
  }
  boolean <- rep(T, length(all_links))
  for(i in 1:length(all_links)) {
    boolean[i] <- !draft_boolean(all_links[i])
  }
  all_links[boolean]
}


draft_boolean <- function(website) {
  html <- website %>%
    readLines()
  
  information <- get_EP_Information(html)
  
  drafted <- information %>%
    grep('Drafted', .)
  
  length(drafted) != 0
}