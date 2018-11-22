record_eval <- function(team) {
  ### Get Team's games
  games <- x[x$team == team,]

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
    sum(games$wins[games$opp_rank >= 201 & games$opp_rank <= 353 & games$location == "N"]) + 
    sum(games$wins[games$opp_rank >= 161 & games$opp_rank <= 353 & games$location == "H"]) + 
    sum(games$wins[games$opp_rank >= 241 & games$opp_rank <= 353 & games$location == "V"])
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
    sum(1 - games$wins[games$opp_rank >= 201 & games$opp_rank <= 353 & games$location == "N"]) + 
    sum(1 - games$wins[games$opp_rank >= 161 & games$opp_rank <= 353 & games$location == "H"]) + 
    sum(1 - games$wins[games$opp_rank >= 241 & games$opp_rank <= 353 & games$location == "V"])
  
  ### Resume Bonus
  qual_bonus <- 16 * tierAw + 8 * tierBw + 2 * tierCw + tierDw - 
    tierAl - 2 * tierBl - 8 * tierCl - 16 * tierDl
  
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
  
  bubble <- atlarge$team[32:40]
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
  
  return(list("qual_bonus" = qual_bonus, "sor" = mean(sor), "wab" = mean(wab),
              "wins" = wins, "losses" = losses))
}


### Get and Return Team's resumes
get_resumes <- function(new){
  if(new){
    tmp <- data.frame("team" = teams,
                      "sor" = rep(0, length(teams)),
                      "qual_bonus" = rep(0, length(teams)),
                      "wab" = rep(0, length(teams)),
                      "wins" = rep(0, length(teams)),
                      "losses" = rep(0, length(teams)))
    for(i in 1:nrow(tmp)) {
      print(paste("Evaluating Team #: ", i, sep = ""))
      rec_eval <- record_eval(teams[i])
      tmp$sor[i] <- rec_eval$sor
      tmp$wab[i] <- rec_eval$wab
      tmp$qual_bonus[i] <- rec_eval$qual_bonus
      tmp$wins[i] <- rec_eval$wins
      tmp$losses[i] <- rec_eval$losses
    }
    write.csv(tmp, "3.0_Files/Bracketology/resumes.csv", row.names = F)
  }
  else{
    tmp <- read.csv("3.0_Files/Bracketology/resumes.csv", as.is = T)
  }
  return(tmp)
}

