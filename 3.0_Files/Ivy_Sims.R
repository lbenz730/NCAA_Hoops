############################# Palestra Sims (Ivy Tourney) ########################################
### Simulates Ivy League Tournament
palestra.sim <- function(teams) {
  tmp <- data.frame("team" = c(teams[1:2], NA),
                    "opponent" = c(teams[4:3], NA), 
                    stringsAsFactors = F)
  tmp$location <- "N"
  
  ### Find Penn in Tournament
  if(teams[1] == "Yale") {
    tmp$location[1] <- "H"
  }else if(teams[4] == "Yale") {
    tmp$location[1] <- "V"
  }
  if(teams[2] == "Yale") {
    tmp$location[2] <- "H"
  }else if(teams[3] == "Yale") {
    tmp$location[2] <- "V"
  }
  
  tmp$pred_score_diff <- predict(lm.hoops, newdata = tmp)
  tmp$winprob <- predict(glm.pointspread, newdata = tmp, type = "response")
  
  ### Sim Semifinals
  rands <- runif(2)
  tmp[3, c("team", "opponent")] <- ifelse(rands <= tmp$winprob[1:2], tmp$team, tmp$opponent)
  
  ### Finals
  if(tmp$team[3] == "Yale") {
    tmp$location[3] <- "H"
  }else if(tmp$opponent[3] == "Yale") {
    tmp$location[3] <- "V"
  }
  
  tmp$pred_score_diff <- predict(lm.hoops, newdata = tmp)
  tmp$winprob <- predict(glm.pointspread, newdata = tmp, type = "response")
  
  # Final
  rand <- runif(1)
  champ <- ifelse(rand <= tmp$winprob[3], tmp$team[3], tmp$opponent[3])
  
  return(champ)
}

################################### IVY SIMS ##################################
### Simulates Ivy League Regular Season
ivy.sim <- function(nsims) {
  games <- x[x$location == "H" & x$team_conf == "Ivy" & x$conf_game & x$reg_season, ]
  ivy <- unique(x$team[x$team_conf == "Ivy"])
  champ <- rep(NA, nsims)
  
  # Data Frame to Hold Team Wins by Sim
  simresults <- as.data.frame(matrix(nrow = nsims, ncol = length(ivy), byrow = T))
  names(simresults) <- ivy
  
  # Stores pre (and later post) tie-break position in standings
  prebreak.pos <- as.data.frame(matrix(nrow = nsims, ncol = length(ivy), byrow = T))
  names(prebreak.pos) <- ivy
  
  # Simulate All Games
  for (j in 1:nrow(simresults)){
    if(j %% 100 == 0) {
      cat("Sim:", j, "\n")
    }
    games$simwins <- NA
    games$oppsimwins <- NA
    rand <- runif(nrow(games))
    games$simwins[games$wins == 1] <- 1
    games$oppsimwins[games$wins == 1] <- 0
    games$simwins[games$wins == 0] <- 0
    games$oppsimwins[games$wins == 0] <- 1
    sims <- games$wins > 0 & games$wins < 1
    games$simwins[sims] <- ifelse(rand[sims] <= games$wins[sims], 1, 0)
    games$oppsimwins[sims] <- ifelse(rand[sims] > games$wins[sims], 1, 0)
    
    # get team win totals for current sim
    for(i in 1:8) {
      simresults[j, i] <- (sum(games$simwins[games$team == ivy[i]]) + 
                             sum(games$oppsimwins[games$opponent == ivy[i]]))
    }
    
    # H2H records (Row Team Wins Over Column Team)
    head2head <- matrix(nrow = 8, ncol = 8)
    colnames(head2head) <- ivy
    rownames(head2head) <- ivy
    for(i in 1:8) {
      for(k in 1:8) {
        head2head[i,k] <- sum(games$simwins[games$team == ivy[i] & games$opponent == ivy[k]]) +
          sum(games$oppsimwins[games$team == ivy[k] & games$opponent == ivy[i]])
      }
    }
    
    # Get order of finish Pre-Tiebreak
    preBreak <- sort(as.vector(simresults[j,], mode = "numeric"), decreasing = T)
    
    for(z in 1:8) {
      prebreak.pos[j,z] <- c(1:length(ivy))[preBreak == simresults[j, z]][1]
    }
    
    # Break any ties 
    for(i in 1:(length(ivy) - 1)) {
      if(sum(prebreak.pos[j,] == i) > 1){
        # Get teams to between which to break tie
        teams <- ivy[prebreak.pos[j,] == i]
        tie <- length(teams)
        teamIDs <- c(1:length(ivy))[is.element(ivy, teams)]
        
        # Tiebreak 1 (H2H)
        h2h <- rep(0, length(teams))
        for(k in 1:length(teams)) {
          h2h[k] <- sum(head2head[teams[k], teams[-k]])
        }
        if(sum(h2h == max(h2h)) == 1) {
          winner <- teams[grep(max(h2h), h2h)]
          winnerID <- teamIDs[grep(max(h2h), h2h)]
          # Winner wins tie-break
          simresults[j, winnerID] <- simresults[j, winnerID] + 0.1 * tie
          # Change current standing of losers
          change <- teams[teams != winner]
          prebreak.pos[j, change] <- i + 1
          next
        }
        else if(sum(h2h == max(h2h)) > 1 & sum(h2h == max(h2h)) < length(teams)){
          change <- setdiff(teams, teams[grep(max(h2h), h2h)])
          teams <- teams[grep(max(h2h), h2h)]
          prebreak.pos[j, change] <- i + 1
        }
        
        # Tiebreak 2 (Record vs. 1-8, descending order)
        for(z in 1:length(ivy)) {
          if(z == i) {
            next
          }
          comp_teams <- ivy[prebreak.pos[j,] == z]
          if(length(comp_teams) == 0) {
            next
          }
          comp_teamsIDs <- c(1:length(ivy))[is.element(ivy, comp_teams)]
          
          h2h <- rep(0, length(teams))
          for(k in 1:length(teams)) {
            h2h[k] <- sum(head2head[teams[k], comp_teams])
          }
          
          if(sum(h2h == max(h2h)) == 1) {
            winner <- teams[grep(max(h2h), h2h)]
            winnerID <- teamIDs[grep(max(h2h), h2h)]
            # Winner wins tie-break
            simresults[j, winnerID] <- simresults[j, winnerID] + 0.1 * tie
            # Change current standing of losers
            change <- teams[teams != winner]
            prebreak.pos[j, change] <- i + 1
            break
          }
          else if(sum(h2h == max(h2h)) > 1 & sum(h2h == max(h2h)) < length(teams)){
            change <- setdiff(teams, teams[grep(max(h2h), h2h)])
            teams <- teams[grep(max(h2h), h2h)]
            prebreak.pos[j, change] <- i + 1
          }
        }
        if(z < 8){
          next
        }
        
        # Tiebreak 3 (Analytics)
        tmp <- power_rankings[is.element(power_rankings$team, teams),]
        tmp <- tmp[order(tmp$team),]
        winner <- tmp$Team[grep(max(tmp$yusag_coeff), tmp$yusag_coeff)]
        winnerID <- c(1:8)[ivy == winner]
        # Winner wins tie-break
        simresults[j, winnerID] <- simresults[j, winnerID] + 0.1 * tie
        # Change current standing of losers
        change <- teams[teams != winner]
        prebreak.pos[j, change] <- i + 1
      }
    }
    
    # Sim Ivy tournament
    palestra <- c(ivy[as.vector(prebreak.pos[j,] == 1)], ivy[as.vector(prebreak.pos[j,] == 2)],
                  ivy[as.vector(prebreak.pos[j,] == 3)], ivy[as.vector(prebreak.pos[j,] == 4)])
    champ[j] <- palestra.sim(palestra)
  }
  
  ### store playoff odds
  playoffs <- data.frame(team = ivy,
                         auto_bid = rep(NA, length(ivy)),
                         playoff_prob = rep(NA, length(ivy)),
                         seed1_prob = rep(NA, length(ivy)),
                         seed2_prob = rep(NA, length(ivy)),
                         seed3_prob = rep(NA, length(ivy)),
                         seed4_prob = rep(NA, length(ivy)))
  
  
  for(i in 1:length(ivy)) {
    playoffs$auto_bid[i] <- round(sum(is.element(champ, ivy[i]))/nsims * 100, 1)
    playoffs$seed1_prob[i] <- round(sum(prebreak.pos[,i] == 1)/nsims * 100, 1)
    playoffs$seed2_prob[i] <- round(sum(prebreak.pos[,i] == 2)/nsims * 100, 1)
    playoffs$seed3_prob[i] <- round(sum(prebreak.pos[,i] == 3)/nsims * 100, 1)
    playoffs$seed4_prob[i] <- round(sum(prebreak.pos[,i] == 4)/nsims * 100, 1)
    playoffs$playoff_prob[i] <- sum(playoffs[i,4:7], na.rm = T)
  }
  write.csv(playoffs, "3.0_Files/Predictions/playoffs.csv", row.names = F)
  playoff_history <- read.csv("playoff_history.csv", as.is = T) %>%
    mutate(date = as.Date(date)) %>%
    filter(date != Sys.Date()) %>%
    rbind(playoffs %>% mutate(date = Sys.Date()))
  write.csv(playoff_history, "3.0_Files/Predictions/playoff_history.csv")
  return(playoffs)
}

############################ Playoff Swing Factor ##############################
### compute playoff swing factor (leverage) of each game
psf <- function(nsims, year, min_date, max_date) {
  tochange <- filter(x, date >= min_date, data <= max_date, team_conf == "Ivy",
                     conf_game, location == "H")
  
  games <- x[x$location == "H" & x$team_conf == "Ivy" & x$conf_game & x$reg_season, ]
  ivy <- unique(x$team[x$team_conf == "Ivy"])
  
  # Data Frame to Hold Team Wins by Sim
  simresults <- as.data.frame(matrix(nrow = nsims, ncol = length(ivy), byrow = T))
  names(simresults) <- ivy
  
  prebreak.pos <- as.data.frame(matrix(nrow = nsims, ncol = length(ivy), byrow = T))
  names(prebreak.pos) <- ivy
  
  
  
  # Create Data Frame to Store SwingFactor
  swingfactor <- data.frame(home = tochange$team,
                            away = tochange$opponent,
                            psf = rep(NA, nrow(tochange)),
                            auto_bid_sf = rep(NA, nrow(tochange)))
  
  # Switch
  q = 0
  for(k in 1:(2 * nrow(tochange))) {
    if(q == 0) {
      games$wins[games$team == tochange$team[ceiling(k/2)] & 
                   games$opponent == tochange$opponent[ceiling(k/2)]] <- 1
      q <- 1
    }
    # Second time through game to change
    else{
      games$wins[games$team == tochange$team[ceiling(k/2)] & 
                   games$opponent == tochange$opponent[ceiling(k/2)]] <- 0
      q <- 0
    }
    
    ### Simulate Games
    champ <- rep(NA, nsims)
    for (j in 1:nrow(simresults)){
      if(j %% 100 == 0) {
        cat("Game_id:", k, "Sim:", j, "\n")
      }
      games$simwins <- NA
      games$oppsimwins <- NA
      rand <- runif(nrow(games))
      games$simwins[games$wins == 1] <- 1
      games$oppsimwins[games$wins == 1] <- 0
      games$simwins[games$wins == 0] <- 0
      games$oppsimwins[games$wins == 0] <- 1
      sims <- games$wins > 0 & games$wins < 1
      games$simwins[sims] <- ifelse(rand[sims] <= games$wins[sims], 1, 0)
      games$oppsimwins[sims] <- ifelse(rand[sims] > games$wins[sims], 1, 0)
      
      # get team win totals for current sim
      for(i in 1:8) {
        simresults[j, i] <- (sum(games$simwins[games$team == ivy[i]]) + 
                               sum(games$oppsimwins[games$opponent == ivy[i]]))
      }
      
      # H2H records (Row Team Wins Over Column Team)
      head2head <- matrix(nrow = 8, ncol = 8)
      colnames(head2head) <- ivy
      rownames(head2head) <- ivy
      for(i in 1:8) {
        for(m in 1:8) {
          head2head[i,m] <- sum(games$simwins[games$team == ivy[i] & games$opponent == ivy[m]]) +
            sum(games$oppsimwins[games$team == ivy[m] & games$opponent == ivy[i]])
        }
      }
      
      # Get order of finish Pre-Tiebreak
      preBreak <- sort(as.vector(simresults[j,], mode = "numeric"), decreasing = T)
      
      for(z in 1:8) {
        prebreak.pos[j,z] <- c(1:length(ivy))[preBreak == simresults[j, z]][1]
      }
      
      # Break any ties 
      for(i in 1:(length(ivy) - 1)) {
        if(sum(prebreak.pos[j,] == i) > 1){
          # Get teams to between which to break tie
          teams <- ivy[prebreak.pos[j,] == i]
          tie <- length(teams)
          teamIDs <- c(1:length(ivy))[is.element(ivy, teams)]
          
          # Tiebreak 1 (H2H)
          h2h <- rep(0, length(teams))
          for(m in 1:length(teams)) {
            h2h[m] <- sum(head2head[teams[m], teams[-m]])
          }
          if(sum(h2h == max(h2h)) == 1) {
            winner <- teams[grep(max(h2h), h2h)]
            winnerID <- teamIDs[grep(max(h2h), h2h)]
            # Winner wins tie-break
            simresults[j, winnerID] <- simresults[j, winnerID] + 0.1 * tie
            # Change current standing of losers
            change <- teams[teams != winner]
            prebreak.pos[j, change] <- i + 1
            next
          }
          else if(sum(h2h == max(h2h)) > 1 & sum(h2h == max(h2h)) < length(teams)){
            change <- setdiff(teams, teams[grep(max(h2h), h2h)])
            teams <- teams[grep(max(h2h), h2h)]
            prebreak.pos[j, change] <- i + 1
          }
          
          # Tiebreak 2 (Record vs. 1-8, descending order)
          for(z in 1:length(ivy)) {
            if(z == i) {
              next
            }
            comp_teams <- ivy[prebreak.pos[j,] == z]
            if(length(comp_teams) == 0) {
              next
            }
            comp_teamsIDs <- c(1:length(ivy))[is.element(ivy, comp_teams)]
            
            h2h <- rep(0, length(teams))
            for(m in 1:length(teams)) {
              h2h[m] <- sum(head2head[teams[m], comp_teams])
            }
            
            if(sum(h2h == max(h2h)) == 1) {
              winner <- teams[grep(max(h2h), h2h)]
              winnerID <- teamIDs[grep(max(h2h), h2h)]
              # Winner wins tie-break
              simresults[j, winnerID] <- simresults[j, winnerID] + 0.1 * tie
              # Change current standing of losers
              change <- teams[teams != winner]
              prebreak.pos[j, change] <- i + 1
              break
            }
            else if(sum(h2h == max(h2h)) > 1 & sum(h2h == max(h2h)) < length(teams)){
              change <- setdiff(teams, teams[grep(max(h2h), h2h)])
              teams <- teams[grep(max(h2h), h2h)]
              prebreak.pos[j, change] <- i + 1
            }
          }
          if(z < 8){
            next
          }
          
          # Tiebreak 3 (Analytics)
          tmp <- power_rankings[is.element(power_rankings$team, teams),]
          tmp <- tmp[order(tmp$team),]
          winner <- tmp$Team[grep(max(tmp$yusag_coeff), tmp$yusag_coeff)]
          winnerID <- c(1:8)[ivy == winner]
          # Winner wins tie-break
          simresults[j, winnerID] <- simresults[j, winnerID] + 0.1 * tie
          # Change current standing of losers
          change <- teams[teams != winner]
          prebreak.pos[j, change] <- i + 1
        }
      }
      # Sim Ivy tournament
      palestra <- c(ivy[as.vector(prebreak.pos[j,] == 1)], ivy[as.vector(prebreak.pos[j,] == 2)],
                    ivy[as.vector(prebreak.pos[j,] == 3)], ivy[as.vector(prebreak.pos[j,] == 4)])
      champ[j] <- palestra.sim(palestra)
    }
    
    
    playoffs <- data.frame(Team = ivy,
                           playoff_prob = rep(NA, length(ivy)),
                           seed1_prob = rep(NA, length(ivy)),
                           seed2_prob = rep(NA, length(ivy)),
                           seed3_prob = rep(NA, length(ivy)),
                           seed4_prob = rep(NA, length(ivy)), 
                           auto_bid = rep(NA, length(ivy)))
    
    
    for(i in 1:length(ivy)) {
      playoffs$seed1_prob[i] <- round(sum(prebreak.pos[,i] == 1)/nsims * 100, 1)
      playoffs$seed2_prob[i] <- round(sum(prebreak.pos[,i] == 2)/nsims * 100, 1)
      playoffs$seed3_prob[i] <- round(sum(prebreak.pos[,i] == 3)/nsims * 100, 1)
      playoffs$seed4_prob[i] <- round(sum(prebreak.pos[,i] == 4)/nsims * 100, 1)
      playoffs$playoff_prob[i] <- sum(playoffs[i,3:6], na.rm = T)
      playoffs$auto_bid[i] <- round(sum(champ == ivy[i])/nsims * 100, 1)
    }
    
    if(q == 1) {
      ### store playoff odds
      simplayoffs <- data.frame(Team = ivy,
                                playoff_prob1 = rep(NA, length(ivy)),
                                playoff_prob2 = rep(NA, length(ivy)),
                                auto_bid_1 = rep(NA, length(ivy)),
                                auto_bid_2 = rep(NA, length(ivy)))
      simplayoffs$playoff_prob1 <- playoffs$playoff_prob
      simplayoffs$auto_bid_1 <- playoffs$auto_bid
    }
    else{
      simplayoffs$playoff_prob2 <- playoffs$playoff_prob
      simplayoffs$auto_bid_2 <- playoffs$auto_bid
      swingfactor$psf[k/2] <- sum(abs(simplayoffs$playoff_prob2 - simplayoffs$playoff_prob1))
      swingfactor$auto_bid_sf[k/2] <- sum(abs(simplayoffs$auto_bid_2 - simplayoffs$auto_bid_1))
    }
  }
  write.csv(swingfactor, "3.0_Files/Predictions/psf.csv", row.names = F)
  return(swingfactor)
}


fast.sim <- function(nsims) {
  games <- x[x$location == "H" & x$team_conf == "Ivy" & x$conf_game & x$reg_season, ]
  ivy <- unique(x$team[x$team_conf == "Ivy"])
  
  # Data Frame to Hold Team Wins by Sim
  simresults <- as.data.frame(matrix(nrow = nsims, ncol = length(ivy), byrow = T))
  names(simresults) <- ivy
  
  # Stores pre (and later post) tie-break position in standings
  prebreak.pos <- as.data.frame(matrix(nrow = nsims, ncol = length(ivy), byrow = T))
  names(prebreak.pos) <- ivy
  
  # Simulate All Games
  for (j in 1:nrow(simresults)){
    if(j %% 100 == 0) {
      cat("Sim:", j, "\n")
    }
    games$simwins <- NA
    games$oppsimwins <- NA
    rand <- runif(nrow(games))
    games$simwins[games$wins == 1] <- 1
    games$oppsimwins[games$wins == 1] <- 0
    games$simwins[games$wins == 0] <- 0
    games$oppsimwins[games$wins == 0] <- 1
    sims <- games$wins > 0 & games$wins < 1
    games$simwins[sims] <- ifelse(rand[sims] <= games$wins[sims], 1, 0)
    games$oppsimwins[sims] <- ifelse(rand[sims] > games$wins[sims], 1, 0)
    
    # get team win totals for current sim
    for(i in 1:8) {
      simresults[j, i] <- (sum(games$simwins[games$team == ivy[i]]) + 
                             sum(games$oppsimwins[games$opponent == ivy[i]]))
    }
  }
  return(simresults)
}