### Generic Simulator for CBB Tournament
### Luke Benz
### Feb 2018

tourney_sim  <- function(teams, byes, double_byes, hca, nsims) {
  ### Champ Storage
  n <- length(teams)
  simresults <- data.frame("team" = teams,
                           champ = rep(NA, n))
  
  ### Sim Tourney
  games <- data.frame("team" = rep(NA, n-1),
                      "opponent" = rep(NA, n-1),
                      "location" = rep(NA, n-1),
                      "team_seed" = rep(NA, n-1),
                      "opp_seed" = rep(NA, n-1),
                      "win_prob" = rep(NA, n-1))
  
  ### Fill in First Round
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
    game_count <- game_count + 1
  }
  
  ### Fill in teams w/ first round byes
  games$team[(game_count + 1):(1 + byes)] <- teams[1:(byes-game_count+1)]
  games$team_seed[(game_count + 1):(1 + byes)] <- 1:(byes-game_count+1)
  remaining <- setdiff(1:n, c(1:(byes-game_count+1), non_bye:n))
  game_count  <- game_count + (byes-game_count+1)
  k <- length(remaining)/2
  games$team[(game_count+1):(game_count+k)] <- teams[remaining[1:k]]
  games$team_seed[(game_count+1):(game_count+k)] <- remaining[1:k]
  games$opponent[(game_count+1):(game_count+k)] <- teams[remaining[(k+1):length(remaining)]]
  games$opp_seed[(game_count+1):(game_count+k)] <- remaining[(k+1):length(remaining)]
  
  ### 
 
}
