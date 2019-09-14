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
    links <- html %>%
      paste(collapse = '\n') %>%
      stringr::str_match_all("<a href=\"(.*?)\"") %>%
      .[[1]] %>%
      .[-(1:300),2] %>%
      .[grep('/player/', .)]
    
    first_goalie_link <- html %>%
      get_EP_table('AB', 'Undrafted')
    if(length(first_goalie_link) == 0) {
      first_goalie_link <- length(links) + 1
    } else {
      first_goalie_link <- first_goalie_link %$%
        Player %>%
        .[1] %>%
        as.character() %>%
        gsub(' ', '-', .) %>%
        iconv(., to="ASCII//TRANSLIT") %>%
        gsub('~', '', .) %>%
        gsub('\\!', '', .) %>%
        tolower() %>%
        grep(links) %>%
        as.numeric() %>%
        .[1]
    }
    if (first_goalie_link <= length(links)) {
      links <- links[-(first_goalie_link:length(links))]
    }
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


get_EP_Information <- function(html) {
  
  right_start <- html %>%
    grep('plyr_details', .) %>%
    as.numeric()
  
  right_end <- html %>%
    grep('</section>', .) %>%
    .[. > right_start] %>%
    .[1] %>%
    as.numeric()
  
  html[right_start:right_end]
}

get_EP_table <- function(html, Season, Need = 'Stats') {
  
  if(Need == 'Stats') {
    right_start <- html %>%
      grep('<table(.*) player-stats', .) %>%
      .[1] %>%
      as.numeric()
  } else if(Need == 'Career') {
    right_start <- html %>%
      grep('<table(.*) total-player-stats', .) %>%
      as.numeric()
  } else if(Need == 'Undrafted') {
    right_start <- html %>%
      grep('<table(.*) goalie-stats', .) %>%
      as.numeric()
  }
  
  right_end <- html %>%
    grep('</table>', .) %>%
    .[. > right_start] %>%
    .[1] %>%
    as.numeric()
  
  full_table <- html %>%
    .[right_start:right_end] %>%
    paste(collapse = '\n') %>%
    XML::readHTMLTable() %>%
    .[[1]]
  
  if (length(full_table) == 0) {
    full_table
  } else if (Season == 'R') {
    full_table %>%
      .[,-(10:ncol(.))]
    
  } else if (Season == 'P') {
    full_table %>%
      .[, -(4:10)]
    
  } else if (Season == 'RP') {
    regularseason_table <- full_table %>%
      .[,-(10:ncol(.))] %>%
      dplyr::mutate(Regular_Playoffs = 'Regular')
    playoff_table <- full_table %>%
      .[, -(4:11)] %>%
      dplyr::mutate(Regular_Playoffs = 'Playoffs')
    rbind(regularseason_table, playoff_table)
  } else {
    full_table
  }
  
}
