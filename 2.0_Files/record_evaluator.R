record_eval <- function(team) {
  games <- y[y$team == team & y$reg_season,]
  games$opp_rank <- NA
  for(k in 1:nrow(games)) {
    games$opp_rank[k] <- powranks$rank[powranks$Team == games$opponent[k]]
  }
  tierAw <- sum(games$wins[games$opp_rank <= 50])
  tierBw <- sum(games$wins[games$opp_rank >= 51 & games$opp_rank <= 100])
  tierCw <- sum(games$wins[games$opp_rank >= 101 & games$opp_rank <= 150])
  tierDw <- sum(games$wins[games$opp_rank >= 151 & games$opp_rank <= 200])
  tierAl <- sum(1 - games$wins[games$opp_rank <= 50])
  tierBl <- sum(1 - games$wins[games$opp_rank >= 51 & games$opp_rank <= 100])
  tierCl <- sum(1 - games$wins[games$opp_rank >= 101 & games$opp_rank <= 150])
  tierDl <- sum(1 - games$wins[games$opp_rank >= 151 & games$opp_rank <= 200])
  qual_bonus <- 4 * tierAw + 3 * tierBw + 2 * tierDw + tierDw - 
    tierAl - 2 * tierBl - 3 * tierCl - 4 * tierDl
  
  test <- powranks$Team[25]
  data <- games
  data$team <- test
  data$predscorediff <- round(predict(lm.hoops, newdata = data), 1)
  data$wins <- 
    round(predict.glm(glm.pointspread, newdata = data, type = "response"), 3)
  sor <- sum(games$wins) - sum(data$wins)
  
  autobids <- by_conf$Team[by_conf$Conference_Rank == 1]
  atlarge <- powranks[!is.element(powranks$Team, autobids),]
  
  bubble <- atlarge$Team[32:40]
  wab <- rep(0, length(bubble))
  for(j in 1:length(bubble)) {
    data <- games
    data$team <- bubble[j]
    data$predscorediff <- round(predict(lm.hoops, newdata = data), 1)
    data$wins <- 
      round(predict.glm(glm.pointspread, newdata = data, type = "response"), 3)
    wab[j] <- sum(games$wins) - sum(data$wins)
  }
  
  return(list("qual_bonus" = qual_bonus, "sor" = sor, "wab" = mean(wab)))
}

get_resumes <- function(new){
  if(new){
    tmp <- data.frame("team" = teams,
                      "sor" = rep(0, length(teams)),
                      "qual_bonus" = rep(0, length(teams)),
                      "wab" = rep(0, length(teams)))
    for(i in 1:nrow(tmp)) {
      print(i)
      rec_eval <- record_eval(teams[i])
      tmp$sor[i] <- rec_eval$sor
      tmp$wab[i] <- rec_eval$wab
      tmp$qual_bonus[i] <- rec_eval$qual_bonus
    }
    write.table(tmp, "2.0_Files/Bracketology/resumes.csv", row.names = F, col.names = T, sep = ",")
  }
  else{
    tmp <- read.csv("2.0_Files/Bracketology/resumes.csv", as.is = T)
  }
  return(tmp)
}

