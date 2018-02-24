### Generic Simulator for CBB Tournament
### Luke Benz
### Feb 2018

tourney_sim  <- function(teams, seeds, byes, double_byes, hca, nsims) {
  ### Champ Storage
  n <- length(teams)
  simresults <- data.frame("team" = teams,
                           "champ" = rep(0, n),
                           "finals" = rep(0, n))
  for(j in 1:nsims) {
    print(paste("Sim #:", j))
    ### Fill in First Round
    games <- data.frame("team" = rep(NA, n-1),
                        "opponent" = rep(NA, n-1),
                        "location" = rep(NA, n-1),
                        "team_seed" = rep(NA, n-1),
                        "opp_seed" = rep(NA, n-1),
                        "predscorediff" = rep(NA, n-1),
                        "win_prob" = rep(NA, n-1),
                        "winner" = rep(NA, n-1))
    game_count <- 0
    non_bye <- n - byes - double_byes
    
    for(i in 1:((non_bye)/2)) {
      games$team[i] <- teams[double_byes + byes + i]
      games$team_seed[i] <- seeds[double_byes + byes + i]
      games$opponent[i] <- teams[n + 1 - i]
      games$opp_seed[i] <- seeds[n + 1 - i]
      
      if(is.na(hca)) {
        games$location[i] <- "N" 
      }
      else if(hca == "seed") {
        games$location[i] <- "H"
      }
      else if(games$team[i] == hca) {
        games$location[i] <- "H"
      }
      else if(games$opponent[i]  == hca) {
        games$location[i] <- "V"
      }
      else{
        games$location[i] <- "N" 
      }
      game_count <- game_count + 1
    }
    cur_round <- 1:(non_bye/2)
    next_round <- ((non_bye)/2 + 1):((non_bye)/2 + (byes + (non_bye/2))/2)
    
    ### Fill in teams w/ first round byes
    if(byes > 0) {
      one_bye <- (double_byes + 1):(double_byes + byes)
      games$team[next_round[1:length(cur_round)]] <- teams[one_bye[1:length(cur_round)]]
      games$team_seed[next_round[1:length(cur_round)]] <- seeds[one_bye[1:length(cur_round)]]
      left <- one_bye[-c(1:length(cur_round))]
      games$team[next_round[-(1:length(cur_round))]] <- teams[left[1:(length(left)/2)]]
      games$team_seed[next_round[-(1:length(cur_round))]] <- seeds[left[1:(length(left)/2)]]
      games$opponent[next_round[-(1:length(cur_round))]] <- teams[sort(left[((length(left)/2) + 1):length(left)], decreasing = T)]
      games$opp_seed[next_round[-(1:length(cur_round))]] <- seeds[sort(left[((length(left)/2) + 1):length(left)], decreasing = T)]

      ### Sim Any Non-Bye Games
      games$predscorediff[cur_round] <- predict(lm.hoops, newdata = games[cur_round,])
      games$win_prob[cur_round] <- predict(glm.pointspread, newdata = games[cur_round,] , type = "response")
      games$winner[cur_round] <- ifelse(games$win_prob[cur_round] >= runif(length(cur_round)), games$team[cur_round],
                                        games$opponent[cur_round])
      games$opponent[next_round[length(cur_round):1]] <- games$winner[cur_round] 
      games$opp_seed[next_round[length(cur_round):1]] <- 
        seeds[unlist(apply(as.data.frame(gsub("[()]", "", paste("^", games$winner[cur_round], "$", sep = ""))), 
              1, grep, gsub("[()]", "", teams)))]
      
      cur_round <- next_round
      next_round <- (max(cur_round) + 1):(max(cur_round) + (double_byes + length(cur_round))/2)
    }
    
    if(double_byes > 0) {
      ### Set Double Bye Teams
      these <- (max(cur_round) + 1):(max(cur_round) + double_byes)
      games$team[these] <- teams[1:double_byes]
      games$team_seed[these] <- 1:double_byes
      
      ### Sim Games with Single Bye Teams
      for(i in cur_round) {
        if(is.na(hca)) {
          games$location[i] <- "N" 
        }
        else if(hca == "seed") {
          games$location[i] <- "H"
        }
        else if(games$team[i] == hca) {
          games$location[i] <- "H"
        }
        else if(games$opponent[i]  == hca) {
          games$location[i] <- "V"
        }
        else{
          games$location[i] <- "N" 
        }
      }
      
      games$predscorediff[cur_round] <- predict(lm.hoops, newdata = games[cur_round,])
      games$win_prob[cur_round] <- predict(glm.pointspread, newdata = games[cur_round,] , type = "response")
      games$winner[cur_round] <- ifelse(games$win_prob[cur_round] >= runif(length(cur_round)), games$team[cur_round],
                                        games$opponent[cur_round])
      games$opponent[next_round[length(cur_round):1]] <- games$winner[cur_round] 
      games$opp_seed[next_round] <- 
        seeds[apply(as.data.frame(gsub("[()]", "", paste("^", games$opponent[next_round], "$", sep = ""))), 
              1, grep, gsub("[()]", "", teams))]
      
      cur_round <- next_round
      next_round <- (max(cur_round) + 1):(max(cur_round) + length(cur_round)/2)
      
    }
    
    
    ### Sim Remainder of Tournament
    while(is.na(games$winner[n-1])) {
      for(i in cur_round) {
        if(is.na(hca)) {
          games$location[i] <- "N" 
        }
        else if(hca == "seed") {
          games$location[i] <- "H"
        }
        else if(games$team[i] == hca) {
          games$location[i] <- "H"
        }
        else if(games$opponent[i]  == hca) {
          games$location[i] <- "V"
        }
        else{
          games$location[i] <- "N" 
        }
      }
      
      
      games$predscorediff[cur_round] <- predict(lm.hoops, newdata = games[cur_round,])
      games$win_prob[cur_round] <- predict(glm.pointspread, newdata = games[cur_round,] , type = "response")
      games$winner[cur_round] <- ifelse(games$win_prob[cur_round] >= runif(length(cur_round)), games$team[cur_round],
                                        games$opponent[cur_round])
      
      if(next_round[1] < n) {
        for(i in 1:(length(cur_round)/2)) {
          team1 <- games$winner[cur_round[i]]
          team2 <- games$winner[cur_round[length(cur_round) + 1 - i]]
          team_seed1 <- seeds[grep(gsub("[()]", "", paste("^", team1, "$", sep = "")), 
                             gsub("[()]", "", teams))]
          team_seed2 <- seeds[grep(gsub("[()]", "", paste("^", team2, "$", sep = "")), 
                             gsub("[()]", "", teams))]
          if(team_seed1 < team_seed2) {
            games$team[next_round[i]] <- team1
            games$opponent[next_round[i]] <- team2
            games$team_seed[next_round[i]] <- team_seed1
            games$opp_seed[next_round[i]] <- team_seed2
          }
          else{
            games$team[next_round[i]] <- team2
            games$opponent[next_round[i]] <- team1
            games$team_seed[next_round[i]] <- team_seed2
            games$opp_seed[next_round[i]] <- team_seed1
          }
        }
        
        ### Update Indicies of Rounds
        cur_round <- next_round
        next_round <- (next_round[length(next_round)] + 1):(next_round[length(next_round)] + length(next_round)/2)
      }
    }
    simresults$champ[simresults$team == games$winner[n-1]] <- 
      simresults$champ[simresults$team == games$winner[n-1]] + 1/nsims
    simresults$finals[simresults$team == games$winner[n-1]] <- 
      simresults$finals[simresults$team == games$winner[n-1]] + 1/nsims
    loser <- setdiff(c(games$team[n-1], games$opponent[n-1]), games$winner[n-1])
    simresults$finals[simresults$team == loser] <- 
      simresults$finals[simresults$team == loser] + 1/nsims
  }
  return(simresults)
}
