### NCAA Tournament SIMS
ncaa_sim <- function(nsims) {
  east <- c("Villanova", "Texas Tech")
  south <- c("Loyola Chicago", "Kansas St.")
  west <- c("Michigan","Florida St.")
  midwest <- c("Kansas", "Duke")
  east_seeds <- c(1, 3)
  west_seeds <- c(3, 9)
  south_seeds <- c(11, 9)
  mw_seeds <- c(1, 2)
  
  
  ncaa_sims <- data.frame("team" = c(east, west, south, midwest),
                          "seed" = c(east_seeds, west_seeds, south_seeds, mw_seeds),
                          "region" = c(rep("East", 2), rep("West", 2),
                                       rep("South", 2), rep("Midwest", 2)),
                          "f4" = rep(0, 8),
                          "ncg" = rep(0, 8),
                          "champ" = rep(0, 8),
                          stringsAsFactors = F)
  for(j in 1:nsims) {
    print(paste("Sim #:", j))
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
      round <- 5
      ### Sim Remainder of Tournament
      while(round < 6) {
        ngame <- 16/(2^(round-1))
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
        ncaa_sims[ncaa_sims$team %in% vec, round-1] <- 
          ncaa_sims[ncaa_sims$team %in% vec, round-1] + 100/nsims
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
      ncaa_sims$ncg[ncaa_sims$team %in% ncg] + 100/nsims
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
