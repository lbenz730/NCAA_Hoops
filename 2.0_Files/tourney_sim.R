### Generic Simulator for CBB Tournament
### Luke Benz
### Feb 2018

tourney_sim  <- function(teams, byes, double_byes, hca, nsims) {
  ### Champ Storage
  n <- length(teams)
  simresults <- data.frame("team" = teams,
                           "champ" = rep(0, n))
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
    non_bye <- n - byes
    
    for(i in 1:((non_bye)/2)) {
      games$team[i] <- teams[byes + i]
      games$team_seed[i] <- byes + i
      games$opponent[i] <- teams[n + 1 - i]
      games$opp_seed[i] <- n + 1 - i
      
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
      games$team[(game_count + 1):(1 + byes)] <- teams[1:(byes-game_count+1)]
      games$team_seed[(game_count + 1):(1 + byes)] <- 1:(byes-game_count+1)
      remaining <- setdiff(1:n, c(1:(byes-game_count+1), non_bye:n))
      game_count  <- game_count + (byes-game_count+1)
      k <- length(remaining)/2
      games$team[(game_count+1):(game_count+k)] <- teams[remaining[1:k]]
      games$team_seed[(game_count+1):(game_count+k)] <- remaining[1:k]
      games$opponent[(game_count+1):(game_count+k)] <- teams[remaining[(k+1):length(remaining)]]
      games$opp_seed[(game_count+1):(game_count+k)] <- remaining[(k+1):length(remaining)]
      game_count <- game_count + k
      
      ### Sim Any Non-Bye Games
      games$predscorediff[cur_round] <- predict(lm.hoops, newdata = games[cur_round,])
      games$win_prob[cur_round] <- predict(glm.pointspread, newdata = games[cur_round,] , type = "response")
      games$winner[cur_round] <- ifelse(games$win_prob[cur_round] >= runif(length(cur_round)), games$team[cur_round],
                                        games$opponent[cur_round])
      games$opponent[next_round] <- games$winner[cur_round] 
      games$opp_seed[next_round] <- 
        apply(as.data.frame(gsub("[()]", "", paste("^", games$opponent[next_round], "$", sep = ""))), 
              1, grep, gsub("[()]", "", teams))
      
      cur_round <- (non_bye/2 + 1):game_count
      next_round <- (game_count + 1):(game_count + length(cur_round)/2) ### Fix with Double Byes
    }
    
    if(double_byes > 0) {
      
      
    }
    
    
    ### Sim Remainder of Tournament
    j <- 1
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
          team_seed1 <- grep(gsub("[()]", "", paste("^", team1, "$", sep = "")), 
                             gsub("[()]", "", teams))
          team_seed2 <- grep(gsub("[()]", "", paste("^", team2, "$", sep = "")), 
                             gsub("[()]", "", teams))
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
  }
}
