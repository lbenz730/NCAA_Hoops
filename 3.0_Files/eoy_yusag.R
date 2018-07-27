library(dplyr)
source("helpers.R")
y <- read.csv("Results/2015-16/NCAA_Hoops_Results_2016_Final.csv", as.is = T)
y <- y %>%
  mutate(scorediff = teamscore - oppscore, predscorediff = NA, wins = NA,
         season_id = "2016-17", game_id = NA, opp_game_id = NA, 
         team_conf = NA, opp_conf = NA, conf_game = NA, GEI = NA)

teams <- unique(y$team)
for(i in 1:length(teams)) {

  y[y$team == teams[i],] <- y %>%
    filter(team == teams[i]) %>%
    mutate(game_id = seq(1, sum(team == teams[i]), 1))
}

for(i in 1:nrow(y)) {
  y$opp_game_id[i] <- get_opp_id(y, i)[1]
}


confs <- read.csv("Info/conferences.csv")
for(i in 1:nrow(y)) {
  w_team <- 1 - (max(c(0, y$game_id[y$team == y$team[i] & !is.na(y$scorediff)])) - y$game_id[i])/
    max(c(1, y$game_id[y$team == y$team[i] & !is.na(y$scorediff)]))
  w_opponent <- 1 - (max(c(0, y$game_id[y$team == y$opponent[i] & !is.na(y$scorediff)])) - y$opp_game_id[i])/
    max(c(1, y$game_id[y$team == y$opponent[i] & !is.na(y$scorediff)]))
  rr <- mean(c(w_team, w_opponent))
  y$weights[i] <- 1/(1 + (0.5^(5 * rr)) * exp(-rr))
}   

lm.hoops <- lm(scorediff ~ team + opponent + location, weights = weights, data = y) 

avg <- mean(lm.hoops$coefficients[2:351])
opp_avg <- mean(lm.hoops$coefficients[352:701])

powerrankings <- data.frame(Team = rep(NA, length(teams)),
                            Conference = rep(NA, length(teams)),
                            YUSAG_Coefficient = rep(NA, length(teams)))
powerrankings[1, ] <- c(teams[1], "Southland", lm.hoops$coefficients[1] - mean(avg, abs(opp_avg)))


### Get YUSAG Coefficients
for(i in 2:(length(teams))) {
  teamcoef <- 
    lm.hoops$coefficients[paste("team", teams[i], sep = "")] - mean(avg, abs(opp_avg))
  opponentcoef <- 
    lm.hoops$coefficients[paste("opponent", teams[i], sep = "")] + mean(avg, abs(opp_avg))
  tmp <- c(teams[i], get_conf(teams[i]), round((teamcoef - opponentcoef)/2, 2))
  powerrankings[i, ] <- tmp
}
powerrankings$YUSAG_Coefficient <- round(as.numeric(powerrankings$YUSAG_Coefficient),2)

write.csv(powerrankings[,-2], "Info/2016_yusag_coefficients.csv", row.names = F)
