record_eval <- function(team) {
  ### Get Team's games
  games <- x[x$team == team,]
  games <- games[!games$postponed & !games$canceled,]
  if(nrow(games) == 0) {
    return(list('team' = team, "qual_bonus" = NA, "sor" = NA, "wab" = NA,
                "wins" = NA, "losses" = NA))
  }
  
  games <- inner_join(select(games, -opp_rank), 
                      select(power_rankings, team, rank), 
                      by = c("opponent" = "team")) %>%
    rename(team_rank = rank.x , opp_rank = rank.y)
  
  ### Classify wins into NCAA's 4 tiers
  tierAw <- 
    sum(games$wins[games$opp_rank <= 50 & games$location == "N"]) + 
    sum(games$wins[games$opp_rank <= 30 & games$location == "H"]) + 
    sum(games$wins[games$opp_rank <= 75 & games$location == "V"])
  tierBw <- 
    sum(games$wins[games$opp_rank >= 51 & games$opp_rank <= 100 & games$location == "N"]) + 
    sum(games$wins[games$opp_rank >= 31 & games$opp_rank <= 75 & games$location == "H"]) + 
    sum(games$wins[games$opp_rank >= 76 & games$opp_rank <= 135 & games$location == "V"]) 
  tierCw <- 
    sum(games$wins[games$opp_rank >= 101 & games$opp_rank <= 200 & games$location == "N"]) + 
    sum(games$wins[games$opp_rank >= 76 & games$opp_rank <= 160 & games$location == "H"]) + 
    sum(games$wins[games$opp_rank >= 136 & games$opp_rank <= 240 & games$location == "V"])
  tierDw <- 
    sum(games$wins[games$opp_rank >= 201 & games$opp_rank <= 358 & games$location == "N"]) + 
    sum(games$wins[games$opp_rank >= 161 & games$opp_rank <= 358 & games$location == "H"]) + 
    sum(games$wins[games$opp_rank >= 241 & games$opp_rank <= 358 & games$location == "V"])
  tierAl <- 
    sum(1 - games$wins[games$opp_rank <= 50 & games$location == "N"]) + 
    sum(1 - games$wins[games$opp_rank <= 30 & games$location == "H"]) + 
    sum(1 - games$wins[games$opp_rank <= 75 & games$location == "V"])
  tierBl <- 
    sum(1 - games$wins[games$opp_rank >= 51 & games$opp_rank <= 100 & games$location == "N"]) + 
    sum(1 - games$wins[games$opp_rank >= 31 & games$opp_rank <= 75 & games$location == "H"]) + 
    sum(1 - games$wins[games$opp_rank >= 76 & games$opp_rank <= 135 & games$location == "V"]) 
  tierCl <- 
    sum(1 - games$wins[games$opp_rank >= 101 & games$opp_rank <= 200 & games$location == "N"]) + 
    sum(1 - games$wins[games$opp_rank >= 76 & games$opp_rank <= 160 & games$location == "H"]) + 
    sum(1 - games$wins[games$opp_rank >= 136 & games$opp_rank <= 240 & games$location == "V"])
  tierDl <- 
    sum(1 - games$wins[games$opp_rank >= 201 & games$opp_rank <= 358 & games$location == "N"]) + 
    sum(1 - games$wins[games$opp_rank >= 161 & games$opp_rank <= 358 & games$location == "H"]) + 
    sum(1 - games$wins[games$opp_rank >= 241 & games$opp_rank <= 358 & games$location == "V"])
  
  ### Resume Bonus
  qual_bonus <- 16 * tierAw + 8 * tierBw + 2 * tierCw + tierDw - 
    4 * tierAl - 8 * tierBl - 16 * tierCl - 32 * tierDl - 
    25 * (tierAl >= 8) - 25 * (tierAl >= 10) - 50 * (tierAl >= 12) 
  
  ### Compute Strength of Record
  test <- power_rankings$team[1:25]
  sor <- rep(0, 25)
  for(j in 1:length(sor)) {
    data <- games
    data$team <- test[j]
    data$pred_score_diff <- round(predict(lm.hoops, newdata = data), 1)
    data$wins <- 
      round(predict.glm(glm.pointspread, newdata = data, type = "response"), 3)
    sor[j] <- sum(games$wins) - sum(data$wins)
  }
  
  ### Compute Wins Above Bubble (Ignore eligibity)
  autobids <- by_conf$team[by_conf$conference_rank == 1]
  atlarge <- power_rankings[!is.element(power_rankings$team, autobids),]
  
  bubble <- arrange(power_rankings, desc(yusag_coeff))$team[32:40]
  wab <- rep(0, length(bubble))
  for(j in 1:length(bubble)) {
    data <- games
    data$team <- bubble[j]
    data$pred_score_diff <- round(predict(lm.hoops, newdata = data), 1)
    data$wins <- 
      round(predict.glm(glm.pointspread, newdata = data, type = "response"), 3)
    wab[j] <- sum(games$wins) - sum(data$wins)
  }
  wins <- sum(games$wins) 
  losses = sum(1 - games$wins) 
  
  return(list('team' = team, "qual_bonus" = qual_bonus, "sor" = mean(sor), "wab" = mean(wab),
              "wins" = wins, "losses" = losses))
}


### Get and Return Team's resumes


get_resumes <- function(new){
  if(new){
    tmp <- future_map(teams, ~record_eval(.x))
    tmp <- bind_rows(tmp)
    write.csv(tmp, "3.0_Files/Bracketology/resumes.csv", row.names = F)
  }
  else{
    tmp <- read.csv("3.0_Files/Bracketology/resumes.csv", as.is = T)
  }
  return(tmp)
}

