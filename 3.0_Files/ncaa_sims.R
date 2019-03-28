### NCAA Tournament SIMS
ncaa_sim <- function(nsims) {
  
  ### Hard Code Teams, Regions, and Seeds
  east <- c("Duke", "Michigan St.", "LSU", "Virginia Tech", 
            "Mississippi St.", "Maryland", "Louisville",
            "VCU", "UCF", "Minnesota", "Belmont", "Temple",
            "Liberty", "Saint Louis", "Yale", "Bradley", 
            "North Dakota St.", "N.C. Central")
  south <- c("Virginia", "Tennessee", "Purdue", "Kansas St.",
             "Wisconsin", "Villanova", "Cincinnati", "Ole Miss",
             "Oklahoma", "Iowa", "Saint Mary's (CA)", "Oregon",
             "UC Irvine", "Old Dominion", "Colgate", "Gardner-Webb")
  west <- c("Gonzaga", "Michigan", "Texas Tech", "Florida St.", 
            "Marquette", "Buffalo", "Nevada", "Syracuse", 
            "Baylor", "Florida", "Arizona St.", "St. John's (NY)",
            "Murray St.", "Vermont", "Northern Ky.", "Montana",
            "Fairleigh Dickinson", "Prairie View")
  midwest <- c("North Carolina", "Kentucky", "Houston", "Kansas",
               "Auburn", "Iowa St.", "Wofford", "Utah St.", 
               "Washington", "Seton Hall", "Ohio St.", "New Mexico St.", 
               "Northeastern", "Georgia St.", "Abilene Christian", "Iona")
  east_seeds <- c(1:10, 11, 11, 12:15, 16, 16)
  west_seeds <- c(1:10, 11, 11, 12:15, 16, 16)
  south_seeds <- 1:16
  mw_seeds <- 1:16
  
  ### Keep Track of teams that have won
  winners <- list()
  winners[[1]]<- c("Belmont", "Arizona St.", "Fairleigh Dickinson", "North Dakota St.")
  winners[[2]] <- c("Minnesota", "LSU", "Auburn", "Florida St.", "Maryland",
                    "Murray St.", "Kansas", "Michigan St.", "Gonzaga", 
                    "Kentucky", "Baylor", "Wofford", "Purdue", "Michigan",
                    "Florida", "Villanova", "Tennessee", "Iowa", "Washington",
                    "North Carolina", "UCF", "Duke", "Buffalo", "Texas Tech",
                    "Liberty", "Virginia Tech", "Oklahoma", "Virginia", 
                    "Ohio St.", "Houston", "UC Irvine",
                    "Oregon")
  winners[[3]] <- c("Kentucky", "LSU", "Gonzaga", "Florida St.", "Michigan",
                         "Purdue", "Michigan St.", "Auburn", "Duke", "North Carolina",
                    "Virginia", "Texas Tech", "Tennessee", "Oregon", "Houston",
                    "Virginia Tech")
  winners[[4]] <- vector()
  winners[[5]] <- vector()
  winners[[6]] <- vector()
  
  first_four <- data.frame("team" = c(east[c(11, 17)], west[c(11, 17)]),
                           "opponent" = c(east[c(12, 18)], west[c(12, 18)]),
                           "location" = "N",
                           stringsAsFactors = F)
  first_four$pred_score_diff <- predict(lm.hoops, newdata = first_four)
  first_four$win_prob <- predict(glm.pointspread, newdata = first_four, type = "response")
  first_four <- mutate(first_four, win_prob = 
                         case_when(team %in% winners[[1]] ~ 1,
                                   opponent %in% winners[[1]] ~ 0,
                                   T ~ win_prob))
  
  ### Storage of Sim Results
  ncaa_sims <- data.frame("team" = c(east, west, south, midwest),
                          "seed" = c(east_seeds, west_seeds, south_seeds, mw_seeds),
                          "region" = c(rep("East", length(east)), 
                                       rep("West", length(west)),
                                       rep("South", length(south)), 
                                       rep("Midwest", length(midwest))),
                          "r64" = 0,
                          "r32" = 0,
                          "s16" = 0,
                          "e8" = 0,
                          "f4" = 0,
                          "ncg" = 0,
                          "champ" = 0,
                          stringsAsFactors = F)
  for(j in 1:nsims) {
    if(j %% 100 == 0) {
      cat("Sim:", j, "\n")
    }
    
    ### Sim First 4
    first_four_losers <- ifelse(runif(4) <= first_four$win_prob, 
                                 first_four$opponent, 
                                 first_four$team)
    
    ### Sim Up to Final 4
    final4 <- rep(NA, 4)
    for(k in 1:4) {
      if(k == 1) {
        vec <- setdiff(east, first_four_losers)
      }
      else if(k == 2) {
        vec <- setdiff(west, first_four_losers)
      }
      else if(k == 3) {
        vec <- south
      }
      else{
        vec <- midwest
      }
      ncaa_sims$r64[ncaa_sims$team %in% vec] <- 
        ncaa_sims$r64[ncaa_sims$team %in% vec] + 100/nsims
      round <- 2
      ### Sim Remainder of Tournament
      while(round < 6) {
        ngame <- 16/(2^(round-1))
        games <- data.frame("team" = rep(NA, ngame),
                            "opponent" = rep(NA, ngame),
                            "location" = rep("N", ngame),
                            "pred_score_diff" = rep(NA, ngame),
                            "win_prob" = rep(NA, ngame),
                            stringsAsFactors = F)
        games$team <- vec[1:ngame]
        games$opponent <- vec[(2*ngame):(ngame + 1)]
        games$pred_score_diff <- predict(lm.hoops, newdata = games)
        games$win_prob <- predict(glm.pointspread, newdata = games, type = "response")
        games <- mutate(games, win_prob = 
                 case_when(team %in% winners[[round]] ~ 1,
                           opponent %in% winners[[round]] ~ 0,
                           T ~ win_prob))
        vec <- ifelse(runif(ngame) <= games$win_prob, games$team, games$opponent)
        ncaa_sims[ncaa_sims$team %in% vec, round+3] <- 
          ncaa_sims[ncaa_sims$team %in% vec, round+3] + 100/nsims
        round <- round + 1
      }
      final4[k] <- vec
    }
    final4 <- data.frame("team" = final4[1:2],
                         "opponent" = final4[4:3],
                         "location" = rep("N", 2),
                         stringsAsFactors = F)
    final4$pred_score_diff <- predict(lm.hoops, newdata = final4)
    final4$win_prob <- predict(glm.pointspread, newdata = final4, type = "response")
    final4 <- mutate(final4, win_prob = 
             case_when(team %in% winners[[6]] ~ 1,
                       opponent %in% winners[[6]] ~ 0,
                       T ~ win_prob))
    ncg <- ifelse(runif(2) <= final4$win_prob, final4$team, final4$opponent)
    ncaa_sims$ncg[ncaa_sims$team %in% ncg] <- 
      ncaa_sims$ncg[ncaa_sims$team %in% ncg] + 100/nsims
    ncg <- data.frame("team" = ncg[1], 
                      "opponent" = ncg[2],
                      "location" = "N",
                      stringsAsFactors = F)
    ncg$pred_score_diff <- predict(lm.hoops, newdata = ncg)
    ncg$win_prob <- predict(glm.pointspread, newdata = ncg, type = "response")
    champ <- ifelse(runif(1) <= ncg$win_prob, ncg$team, ncg$opponent)
    ncaa_sims$champ[ncaa_sims$team == champ] <- 
      ncaa_sims$champ[ncaa_sims$team == champ] + 100/nsims
  }
  return(ncaa_sims)
}
