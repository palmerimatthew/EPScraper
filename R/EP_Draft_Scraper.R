#' Draft URL Scraper
#'
#' Returns data from all players in the given draft year url
#' @param Data the url of the draft year webpage on eliteprospects.com.
#' @param Agerange 2-length vector for the age ranges desired. first number is lower-bound, second number is upper-bound. This will be applied to all players.
#' @param draft.year Boolean about whether to include the draft year of the player.
#' @param draft.pick Boolean about whether to include the draft pick used on the player.
#' @param round Boolean about whether to include the round the player was drafted in.
#' @param Agerel This determines how the exact age included in the data frame is calculated. Default is 9/15 due to this being the cutoff data for draft eligibility. For example, during the 2019-2020 season, if Agerel is 9/15, the age in the data frame will represent the players age on 9/15/2020. The offset here is so that for a player's first draft eligible year, their age will be >18.
#' @param Goalie Boolean about whether information on goalies is wanted. If true, output will be a list of data frames, with one for players, and one for goalies. Currently this doesn't do anything, as there isn't a goalie scraper function built out yet.
#' @param position Boolean about whether to include the position of the player.
#' @param shoots Boolean about whether to include the handedness of the player.
#' @param Stats vector of the wanted stats. 
#'     S - Season
#'     Team - Team
#'     League - League
#'     GP - Games Played
#'     G - Goals
#'     A - Assists
#'     TP - Total Points
#'     PIM - Penalties in Minutes
#'     +/- - Plus/Minus
#'     sv% - Save Percentage (for goalies)
#'     GAA - Goals Against Average (for goalies)
#' @param place.birth Boolean about whether to include the birthplace of the player.
#' @param pbsep Boolean about whether the birthplace should be split into Country, State, and City. place.birth has to be true for this to matter.
#' @param Country Boolean about whether to include the country the player represents (or would represent) in international tournaments. Currently just grabs the first if a player has multiple, but eliteprospects is generally good about putting the correct one first.
#' @param height Boolean about whether to include the height of the player. This is in centimeters.
#' @param weight Boolean about whether to include the weight of the player. This is in pounds.
#' @param date.birth Boolean about whether to include the date of birth of the player.
#' @param dbsep Boolean about whether the date of birth should be seperated into year, month, and day. date.birth has to be true for this to matter.
#' @param drafted.team Boolean about whether to include the team who drafted the player.
#' @param reg.playoffs Determines if regular season data, playoff data, or both will be returned for all the players. Currently only 'R' works.
#' @return data frame (or list of data frames) with data from all the players drafted in the given year.
#' @export

EP_Draft_Scraper <- function(Data, Agerange = c(17, 25), draft.year = T, draft.pick = T, round = T, 
                          Agerel = "9/15", Goalie = F, position = T, shoots = T, 
                          Stats = c("S", "Team", "League", "GP", "G", "A", "TP", "PIM", "+/-", "sv%", "GAA"),
                          place.birth = T, pbsep = T, Country = T, height = T, weight = T, date.birth = T, 
                          dbsep = T, drafted.team = T, reg.playoffs = 'R') {
  links <- paste(readLines(Data), collapse = "\n") %>%
    str_match_all("<a href=\"(.*?)\"") %>%
    extract2(1)  %>%
    .[-(1:300),2] %>%
    .[grep('player',.)]
  
  goalie_spots <- read_html(Data) %>%
    html_nodes("table") %>%
    html_table(header = T, fill = T) %>%
    extract2(2) %>%
    filter(!Team %in% paste('ROUND', c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)) &
             Player != 'No selection was made') %>%
    separate(Player, c('Name', 'Position'), '\\(', fill = 'right') %$%
    Position %>%
    substr(1,1) %>%
    grep("G", .)
  
  player_links <- links[-goalie_spots]
  if (Goalie) {
    goalie_links <- links[goalie_spots]
  }
  player_template <- Ind_Scraper(player_links[1], Agerange, draft.year, draft.pick, round, draft.elig, Agerel, position, 
                                 shoots, Stats, place.birth, pbsep, Country, height, weight, date.birth, dbsep, drafted.team, reg.playoffs)
  
  player_data <- player_template %>%
    filter(Season == 'F')
  
  for(link in player_links) {
    temp <- Ind_Scraper(link, Agerange, draft.year, draft.pick, round, draft.elig, Agerel, position, 
                        shoots, Stats, place.birth, pbsep, Country, height, weight, date.birth, dbsep, drafted.team, reg.playoffs)
    player_data <- player_data %>%
      rbind(temp)
  }
  player_data
}