### NCAA Tournament SIMS
ncaa_sim <- function(nsims) {
  east11 <- c("St. Bonaventure", "UCLA")
  east16 <- c("LIU Brooklyn", "Radford")
  east <- c("Villanova", "Purdue", "Texas Tech", "Wichita St.", "West Virginia",
            "Florida", "Arkansas", "Virgina Tech", "Alabama", "Butler", east11,
            "Murray St.", "Marshall", "SFA", "Cal St. Fullerton", east16)
  south <- c("Virginia", "Cincinnati", "Tennessee", "Arizona", "Kentucky",
             "Miami", "Nevada", "Creighton", "Kansas St.", "Texas", "Loyola Chicago", 
             "Davidson", "Buffalo", "Wright St.", "Georgia St.", "UMBC")
  west16 <- c("N.C. Central", "Texas Southern")
  west <- c("Xavier", "North Carolina", "Michigan", "Gonzaga", "Ohio St.", "Houston",
            "Texas A&M", "Missouri", "Florida St.", "Providence", "San Diego St.",
            "South Dakota St.", "UNCG", "Montana", "Lipscomb", west16)
  midwest11 <- c("Arizona St.", "Syracuse")
  midwest <- c("Kansas", "Duke", "Michigan St.", "Auburn", "Clemson", "TCU",
               "Rhode Island", "Seton Hall", "North Carolina St.", "Oklahoma",
               midwest11, "New Mexico St.", "Col. of Charleston", "Bucknell",
               "Iona", "Penn")
  
  
  ncaa_sims <- data.frame("team" = c(east, west, south, midwest),
                          "fr" = rep(0, 68),
                          "sr" = rep(0, 68),
                          "s16" = rep(0, 68),
                          "e8" = rep(0, 68),
                          "f4" = rep(0, 68),
                          "ncg" = rep(0, 68),
                          "champ" = rep(0, 68))
  for(j in 1:nsims) {
    print(paste("Sim #:", j))
    
    ### Reset Teams
    east11 <- c("St. Bonaventure", "UCLA")
    east16 <- c("LIU Brooklyn", "Radford")
    east <- c("Villanova", "Purdue", "Texas Tech", "Wichita St.", "West Virginia",
              "Florida", "Arkansas", "Virginia Tech", "Alabama", "Butler", east11,
              "Murray St.", "Marshall", "SFA", "Cal St. Fullerton", east16)
    south <- c("Virginia", "Cincinnati", "Tennessee", "Arizona", "Kentucky",
               "Miami (FL)", "Nevada", "Creighton", "Kansas St.", "Texas", "Loyola Chicago", 
               "Davidson", "Buffalo", "Wright St.", "Georgia St.", "UMBC")
    west16 <- c("N.C. Central", "Texas Southern")
    west <- c("Xavier", "North Carolina", "Michigan", "Gonzaga", "Ohio St.", "Houston",
              "Texas A&M", "Missouri", "Florida St.", "Providence", "San Diego St.",
              "South Dakota St.", "UNCG", "Montana", "Lipscomb", west16)
    midwest11 <- c("Arizona St.", "Syracuse")
    midwest <- c("Kansas", "Duke", "Michigan St.", "Auburn", "Clemson", "TCU",
                 "Rhode Island", "Seton Hall", "North Carolina St.", "Oklahoma",
                 midwest11, "New Mexico St.", "Col. of Charleston", "Bucknell",
                 "Iona", "Penn")
    
    ### First 4
    first_4 <- data.frame("team" = c(east11[1], east16[1], midwest11[1], west16[1]),
                          "opponent" = c(east11[2], east16[2], midwest11[2], west16[2]),
                          "location" = rep("N", 4),
                          stringsAsFactors = F)
    first_4$predscorediff <- predict(lm.hoops, newdata = first_4)
    first_4$winprob <- predict(glm.pointspread, newdata = first_4, type = "response")
    winners <- ifelse(runif(4) <= first_4$winprob, first_4$team, first_4$opponent)
    lossers <- setdiff(c(first_4$team, first_4$opponent), winners)
    east <- east[! east %in% lossers]
    west <- west[! west %in% lossers]
    midwest <- midwest[! midwest %in% lossers]
    ncaa_sims$fr[! ncaa_sims$team %in% c(first_4$team, first_4$opponent)] <- 
      ncaa_sims$fr[! ncaa_sims$team %in% c(first_4$team, first_4$opponent)] + 100/nsims
    ncaa_sims$fr[ncaa_sims$team %in% winners] <- 
      ncaa_sims$fr[ncaa_sims$team %in% winners] + 100/nsims
    
    ### Sim Up to Final 4
    final4 <- rep(NA, 4)
    for(k in 1:4) {
      if(k == 1) {
        vec <- east
      }
      else if(k == 2) {
        vec <- west
      }
      else if(k == 3) {
        vec <- south
      }
      else{
        vec <- midwest
      }
      round <- 3
      ### Sim Remainder of Tournament
      while(round < 7) {
        ngame <- 16/(2^(round-2))
        games <- data.frame("team" = rep(NA, ngame),
                            "opponent" = rep(NA, ngame),
                            "location" = rep("N", ngame),
                            "predscorediff" = rep(NA, ngame),
                            "win_prob" = rep(NA, ngame),
                            stringsAsFactors = F)
        games$team <- vec[1:ngame]
        games$opponent <- vec[(2*ngame):(ngame + 1)]
        games$predscorediff <- predict(lm.hoops, newdata = games)
        games$win_prob <- predict(glm.pointspread, newdata = games, type = "response")
        vec <- ifelse(runif(ngame) <= games$win_prob, games$team, games$opponent)
        ncaa_sims[ncaa_sims$team %in% vec, round] <- 
          ncaa_sims[ncaa_sims$team %in% vec, round] + 100/nsims
        round <- round + 1
      }
      final4[k] <- vec
    }
    final4 <- data.frame("team" = final4[1:2],
                         "opponent" = final4[4:3],
                         "location" = rep("N", 2),
                         stringsAsFactors = F)
    final4$predscorediff <- predict(lm.hoops, newdata = final4)
    final4$win_prob <- predict(glm.pointspread, newdata = final4, type = "response")
    ncg <- ifelse(runif(2) <= final4$win_prob, final4$team, final4$opponent)
    ncaa_sims$ncg[ncaa_sims$team %in% ncg] <- 
      ncaa_sims$ncg[ncaa_sims$team %in% vec] + 100/nsims
    ncg <- data.frame("team" = ncg[1], 
                      "opponent" = ncg[2],
                      "location" = "N",
                      stringsAsFactors = F)
    ncg$predscorediff <- predict(lm.hoops, newdata = ncg)
    ncg$win_prob <- predict(glm.pointspread, newdata = ncg, type = "response")
    champ <- ifelse(runif(1) <= ncg$win_prob, ncg$team, ncg$opponent)
    ncaa_sims$champ[ncaa_sims$team == champ] <- 
      ncaa_sims$champ[ncaa_sims$team == champ] + 100/nsims
  }
  return(ncaa_sims)
}
