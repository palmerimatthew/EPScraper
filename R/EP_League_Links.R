#' League URL gatherer
#'
#' Reutrns a list of URLs of the players from a league stats URL.
#' @param Data the url of the league and year stats page from eliteprospects.
#' @param undrafted Boolean about whether to return only undrafted players, or all players (regardless of whether or not they were drafted).
#' @param age_resctriction Use this to select the values that would be in the 'All ages' dropdown on eliteprospects. If you want all players, set this to F or NA. valid values: (u14-u30, o30, o35, o40)
#' @return A vector of player eliteprospects URLs
#' @export

EP_League_Links <- function(Data, undrafted = T, age_restriction = F) {
  if (grepl('\\?', Data)) {
    if (is.character(age_restriction)) {
      Data <- paste0(Data, '&age=', age_restriction)
    }
  } else {
    if (is.character(age_restriction)) {
      Data <- paste0(Data, '?age=', age_restriction)
    }
  }
  num_pages <- Data %>%
    readLines() %>%
    .[grep('table-pagination', .)+1] %>%
    .[1] %>%
    gsub(' players found', '', .) %>%
    gsub(' ', '', .) %>%
    as.numeric() %>%
    magrittr::divide_by(100) %>%
    ceiling()
  
  if (is.na(num_pages)) {num_pages <- 1}
  
  all_links <- character(0)
  
  for(i in 1:num_pages) {
    if(grepl('\\?', Data)) {
      website <- paste0(Data, '&page=', i)
    } else {
      website <- paste0(Data, '?page=', i)
    }
    
    html <- website %>%
      readLines()
    
    start <- html %>%
      grep('<table(.*) player-stats', .) %>%
      as.numeric()
    
    end <- html %>%
      grep('</table>', .) %>%
      .[. > start] %>%
      .[1] %>%
      as.numeric()
    
    links <- html %>%
      .[start:end] %>%
      paste(collapse = '\n') %>%
      stringr::str_match_all("<a href=\"(.*?)\"") %>%
      .[[1]] %>%
      .[, 2] %>%
      .[grep('/player/', .)]
    
    all_links <- c(all_links, links)
  }
  if(undrafted) {
    if(grepl('\\?', Data)) {
      drafted_links <- EP_League_Links(paste0(Data, '&prospects=drafted-players'), undrafted = F)
    } else {
      drafted_links <- EP_League_Links(paste0(Data, '?prospects=drafted-players'), undrafted = F)
    }
    all_links <- all_links[!(all_links %in% drafted_links)]
  }
  all_links
}
