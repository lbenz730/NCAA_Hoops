############################# Palestra Sims (Ivy Tourney) ########################################
### Simulates Ivy League Tournament
palestra.sim <- function(teams) {
  tmp <- data.frame("team" = c(teams[1:2], NA),
                    "opponent" = c(teams[4:3], NA), 
                    stringsAsFactors = F)
  tmp$location <- "N"
  
  ### Find Penn in Tournament
  if(teams[1] == "Princeton") {
    tmp$location[1] <- "H"
  }else if(teams[4] == "Princeton") {
    tmp$location[1] <- "V"
  }
  if(teams[2] == "Princeton") {
    tmp$location[2] <- "H"
  }else if(teams[3] == "Princeton") {
    tmp$location[2] <- "V"
  }
  
  tmp$pred_score_diff <- predict(lm.hoops, newdata = tmp)
  tmp$winprob <- predict(glm.pointspread, newdata = tmp, type = "response")
  
  ### Sim Semifinals
  rands <- runif(2)
  tmp[3, c("team", "opponent")] <- ifelse(rands <= tmp$winprob[1:2], tmp$team, tmp$opponent)
  
  ### Finals
  if(tmp$team[3] == "Princeton") {
    tmp$location[3] <- "H"
  }else if(tmp$opponent[3] == "Princeton") {
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
  games <- ivy_games(x)
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
        winner <- tmp$team[grep(max(tmp$yusag_coeff), tmp$yusag_coeff)]
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
  
  
  df_wins <- 
    inner_join(simresults %>% 
                 floor() %>% 
                 mutate('sim_id' = 1:nrow(.)) %>% 
                 pivot_longer(-sim_id,
                              names_to = 'team',
                              values_to = 'n_wins'),
               prebreak.pos %>% 
                 mutate('sim_id' = 1:nrow(.)) %>% 
                 pivot_longer(-sim_id,
                              names_to = 'team',
                              values_to = 'place'),
               by = c("sim_id", "team")) %>% 
    mutate('playoff' = (place <= 4)) %>% 
    group_by(sim_id) %>% 
    mutate('tiebreak' = 
             (n_wins == sort(n_wins, decreasing = T)[4]) & 
             (n_wins == sort(n_wins, decreasing = T)[5])) %>% 
    ungroup()
  
  write_csv(df_wins, '3.0_Files/Predictions/win_place_results.csv')
  
  ### store playoff odds
  playoffs <- data.frame(team = ivy,
                         auto_bid = rep(NA, length(ivy)),
                         playoff_prob = rep(NA, length(ivy)),
                         seed1_prob = rep(NA, length(ivy)),
                         seed2_prob = rep(NA, length(ivy)),
                         seed3_prob = rep(NA, length(ivy)),
                         seed4_prob = rep(NA, length(ivy)),
                         stringsAsFactors = F)
  
  
  for(i in 1:length(ivy)) {
    playoffs$auto_bid[i] <- sum(is.element(champ, ivy[i]))/nsims * 100
    playoffs$seed1_prob[i] <- sum(prebreak.pos[,i] == 1)/nsims * 100
    playoffs$seed2_prob[i] <- sum(prebreak.pos[,i] == 2)/nsims * 100
    playoffs$seed3_prob[i] <- sum(prebreak.pos[,i] == 3)/nsims * 100
    playoffs$seed4_prob[i] <- sum(prebreak.pos[,i] == 4)/nsims * 100
    playoffs$playoff_prob[i] <- sum(prebreak.pos[,i] <= 4)/nsims * 100
  }
  write.csv(playoffs, "3.0_Files/Predictions/playoffs.csv", row.names = F)
  playoff_history <- 
    read.csv("3.0_Files/Predictions/playoff_history.csv", as.is = T) %>%
    mutate(date = as.Date(date)) %>%
    filter(date != Sys.Date()) %>%
    rbind(playoffs %>% mutate(date = Sys.Date()))
  write.csv(playoff_history, "3.0_Files/Predictions/playoff_history.csv", row.names = F)
  return(playoffs)
}

############################ Playoff Swing Factor ##############################
### compute playoff swing factor (leverage) of each game
psf <- function(nsims, min_date, max_date) {
  tochange <- 
    filter(x, date >= min_date, date <= max_date) %>% 
    ivy_games()
  
  
  ivy <- unique(x$team[x$team_conf == "Ivy"])
  
  # Data Frame to Hold Team Wins by Sim
  simresults <- as.data.frame(matrix(nrow = nsims, ncol = length(ivy), byrow = T))
  names(simresults) <- ivy
  
  prebreak.pos <- as.data.frame(matrix(nrow = nsims, ncol = length(ivy), byrow = T))
  names(prebreak.pos) <- ivy
  
  # Create Data Frame to Store SwingFactor
  swingfactor <- tibble(date = tochange$date,
                        home = tochange$team,
                        away = tochange$opponent,
                        psf = rep(NA, nrow(tochange)),
                        auto_bid_sf = rep(NA, nrow(tochange)),
  )
  df_list <- list()
  
  # Switch
  q = 0
  for(k in 1:(2 * nrow(tochange))) {
    games <- x[x$location == "H" & x$team_conf == "Ivy" & x$conf_game & x$reg_season & !x$postponed, ]
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
          winner <- tmp$team[grep(max(tmp$yusag_coeff), tmp$yusag_coeff)]
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
      playoffs$playoff_prob[i] <- round(sum(prebreak.pos[,i] <= 4)/nsims * 100, 1)
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
      simplayoffs$seed1_prob_1 <- playoffs$seed1_prob
      simplayoffs$seed2_prob_1 <- playoffs$seed2_prob
      simplayoffs$seed3_prob_1 <- playoffs$seed3_prob
      simplayoffs$seed4_prob_1 <- playoffs$seed4_prob
    }
    else{
      simplayoffs$playoff_prob2 <- playoffs$playoff_prob
      simplayoffs$auto_bid_2 <- playoffs$auto_bid
      simplayoffs$seed1_prob_2 <- playoffs$seed1_prob
      simplayoffs$seed2_prob_2 <- playoffs$seed2_prob
      simplayoffs$seed3_prob_2 <- playoffs$seed3_prob
      simplayoffs$seed4_prob_2 <- playoffs$seed4_prob
      swingfactor$psf[k/2] <- sum(abs(simplayoffs$playoff_prob2 - simplayoffs$playoff_prob1))
      swingfactor$auto_bid_sf[k/2] <- sum(abs(simplayoffs$auto_bid_2 - simplayoffs$auto_bid_1))
      df_list[[k/2]] <- simplayoffs
    }
  }
  
  swingfactor$df <- df_list
  
  
  psf_history <- read.csv("3.0_Files/Predictions/psf_history.csv", as.is = T) %>%
    mutate(date = as.Date(date)) %>%
    filter(date < Sys.Date())
  psf_history <- rbind(psf_history, swingfactor %>% select(-df))
  psf_history <- 
    filter(psf_history, date >= "2021-11-05") %>% 
    distinct(date, home, away, .keep_all = T)
  write.csv(psf_history, "3.0_Files/Predictions/psf_history.csv", row.names = F) 
  write.csv(swingfactor  %>% select(-df), "3.0_Files/Predictions/psf.csv", row.names = F)
  
  swingfactor$df_home <- map(df_list, ~select(.x, Team, 'playoff' = playoff_prob1))
  swingfactor$df_away <- map(df_list, ~select(.x, Team, 'playoff' = playoff_prob2))
  
  write_rds(swingfactor, '3.0_Files/Predictions/ivy_psf_full.rds')
  
  ivy_psf <- swingfactor
  delta <- unlist(map(1:nrow(ivy_psf), ~{ivy_psf$df[[.x]]$playoff_prob1 -ivy_psf$df[[.x]]$playoff_prob2}))
  for(i in 1:nrow(ivy_psf)) {
    df <- ivy_psf$df_home[[i]]
    df <- inner_join(df, ncaa_colors, by = c('Team' = 'ncaa_name'))
    labels <- paste0("<img src ='3.0_Files/Predictions/psf_figures/ivy_logos/", df$Team, ".png', width = '20'/>")
    
    p <- 
      ggplot(df, aes(x = Team, y = playoff)) + 
      coord_flip() + 
      geom_col(aes(fill = Team)) + 
      theme_void() +
      ggtext::geom_richtext(color = 'white', 
                            angle = -90,
                            nudge_y = 5,
                            size = 8,
                            aes(fill = Team,
                                label = paste0(sprintf('%0.1f', abs(playoff)), '%'))) + 
      scale_fill_manual(values = df$primary_color) +
      scale_color_manual(values = df$secondary_color) +
      scale_y_continuous(limits = c(0, 105)) +
      scale_x_discrete(limits = rev(df$Team), labels = rev(labels)) + 
      theme(legend.position = 'none',
            axis.text.y = ggtext::element_markdown())
    
    ggsave(filename = paste0('3.0_Files/Predictions/psf_figures/home_', i, '.png'))
    
    df <- ivy_psf$df_away[[i]]
    df <- inner_join(df, ncaa_colors, by = c('Team' = 'ncaa_name'))
    p <- 
      ggplot(df, aes(x = Team, y = playoff)) + 
      coord_flip() + 
      geom_col(aes(fill = Team)) + 
      ggtext::geom_richtext(color = 'white', 
                            angle = -90,
                            nudge_y = 5,
                            size = 8,
                            aes(fill = Team,
                                label = paste0(sprintf('%0.1f', abs(playoff)), '%'))) + 
      theme_void() +
      scale_fill_manual(values = df$primary_color) +
      scale_color_manual(values = df$secondary_color) +
      scale_y_continuous(limits = c(0, 105)) +
      scale_x_discrete(limits = rev(df$Team), labels = rev(labels)) + 
      theme(legend.position = 'none',
            axis.text.y = ggtext::element_markdown())
    ggsave(filename = paste0('3.0_Files/Predictions/psf_figures/away_', i, '.png'))
    
    df <- ivy_psf$df[[i]]
    df <- inner_join(df, ncaa_colors, by = c('Team' = 'ncaa_name'))
    
    ggplot(df, aes(x = Team, y = playoff_prob1 - playoff_prob2)) + 
      coord_flip() + 
      geom_col(aes(fill = Team)) + 
      ggtext::geom_richtext(color = 'white', 
                            angle = -90 * ifelse(df$playoff_prob1 >= df$playoff_prob2, 1, -1),
                            nudge_y = 2 * ifelse(df$playoff_prob1 >= df$playoff_prob2, 1, -1),
                            size = 8,
                            aes(fill = Team,
                                label = paste0(sprintf('%0.1f', abs(playoff_prob1 - playoff_prob2)), '%'))) + 
      theme_void() +
      scale_fill_manual(values = df$primary_color) +
      scale_y_continuous(limits = c(-1.25, 1.25) * max(abs(delta))) +
      scale_x_discrete(limits = rev(df$Team), labels = rev(labels)) + 
      theme(legend.position = 'none',
            axis.text.y = ggtext::element_markdown())
    ggsave(filename = paste0('3.0_Files/Predictions/psf_figures/delta_', i, '.png'))
    
  }
  
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

ivy_games <- function(x) {
  x %>% 
    filter(conf_game, team_conf == 'Ivy', location == 'H',
           !canceled, !duplicated(paste(team, opponent, location), fromLast = T))
}




tiebreak <- function(games) {
  ivy <- unique(games$team)
  games$simwins <- NA
  games$oppsimwins <- NA
  games$simwins[games$wins == 1] <- 1
  games$oppsimwins[games$wins == 1] <- 0
  games$simwins[games$wins == 0] <- 0
  games$oppsimwins[games$wins == 0] <- 1
  
  # get team win totals for current sets of games
  simresults <- rep(NA, 8)
  names(simresults) <- ivy
  for(i in 1:8) {
    simresults[i] <- (sum(games$simwins[games$team == ivy[i]]) + 
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
  preBreak <- sort(simresults, decreasing = T)
  
  prebreak.pos <- rep(NA, 8)
  names(prebreak.pos) <- ivy
  for(z in 1:8) {
    prebreak.pos[z] <- c(1:length(ivy))[preBreak == simresults[z]][1]
  }
  
  # Break any ties 
  for(i in 1:(length(ivy) - 1)) {
    if(sum(prebreak.pos == i) > 1){
      # Get teams to between which to break tie
      teams <- ivy[prebreak.pos == i]
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
        simresults[winnerID] <- simresults[winnerID] + 0.1 * tie
        # Change current standing of losers
        change <- teams[teams != winner]
        prebreak.pos[change] <- i + 1
        next
      }
      else if(sum(h2h == max(h2h)) > 1 & sum(h2h == max(h2h)) < length(teams)){
        change <- setdiff(teams, teams[grep(max(h2h), h2h)])
        teams <- teams[grep(max(h2h), h2h)]
        prebreak.pos[change] <- i + 1
      }
      
      # Tiebreak 2 (Record vs. 1-8, descending order)
      for(z in 1:length(ivy)) {
        if(z == i) {
          next
        }
        comp_teams <- ivy[prebreak.pos == z]
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
          simresults[winnerID] <- simresults[winnerID] + 0.1 * tie
          # Change current standing of losers
          change <- teams[teams != winner]
          prebreak.pos[change] <- i + 1
          break
        }
        else if(sum(h2h == max(h2h)) > 1 & sum(h2h == max(h2h)) < length(teams)){
          change <- setdiff(teams, teams[grep(max(h2h), h2h)])
          teams <- teams[grep(max(h2h), h2h)]
          prebreak.pos[change] <- i + 1
        }
      }
      if(z < 8){
        next
      }
      
      # Tiebreak 3 (Analytics)
      tmp <- power_rankings[is.element(power_rankings$team, teams),]
      tmp <- tmp[order(tmp$team),]
      winner <- tmp$team[grep(max(tmp$yusag_coeff), tmp$yusag_coeff)]
      winnerID <- c(1:8)[ivy == winner]
      # Winner wins tie-break
      simresults[winnerID] <- simresults[winnerID] + 0.1 * tie
      # Change current standing of losers
      change <- teams[teams != winner]
      prebreak.pos[change] <- i + 1
    }
  }
  
  return(names(sort(prebreak.pos)[1:4]))
}


clinch_scenarios <- function() {
  games <-
    ivy_games(x) %>%
    mutate('ivy_id' = 1:nrow(.))
  
  to_play <-
    games %>%
    filter(is.na(team_score)) %>%
    select(ivy_id, team, opponent, 'win_prob' = wins) %>% 
    mutate('opp_win_prob' = 1-win_prob)
  
  df_cases <- expand.grid(map(1:nrow(to_play), ~c(to_play$team[.x], to_play$opponent[.x])))
  names(df_cases) <- paste0('game_', to_play$ivy_id)
  ix_toplay <- games$ivy_id %in% to_play$ivy_id
  
  df_results <- 
    df_cases %>% 
    mutate('seed_1' = NA, 'seed_2' = NA, 'seed_3' = NA, 'seed_4' = NA, 'prob' = NA)
  names(df_results)[1:nrow(to_play)] <- paste(to_play$opponent, '@', to_play$team)
  n <- nrow(df_cases)
  for(i in 1:n) {
    cat(i, 'of', n, '\n')
    df_tmp <- games
    df_tmp$wins[ix_toplay] <- as.numeric(df_tmp$team[ix_toplay] == df_cases[i,])
    playoffs <- tiebreak(df_tmp)
    df_results[i, c('seed_1', 'seed_2', 'seed_3', 'seed_4')] <- playoffs
    df_results$prob[i] <- prod(df_tmp$wins[ix_toplay] * to_play$win_prob + (1-df_tmp$wins[ix_toplay]) * to_play$opp_win_prob)
  }
  
  
  
  write_csv(df_results, '3.0_Files/Predictions/ivy_clinch_scenarios.csv')
}
