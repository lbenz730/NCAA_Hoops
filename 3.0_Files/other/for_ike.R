library(dplyr)
source("3.0_Files/helpers.R")
confs <- read.csv("3.0_Files/Info/conferences.csv", as.is = T)
x <- read.csv("3.0_Files/Results/2017-18/NCAA_Hoops_Results_3_25_2018.csv", as.is = T)
teams <- unique(x$team)

x <- x %>%
  rename(team_score = teamscore, opp_score = oppscore) %>%
  mutate(date = as.Date(paste(year, month, day, sep = "-")),
         score_diff = team_score - opp_score,
         total_score = team_score + opp_score,
         season_id = "2016-17", game_id = NA, opp_game_id = NA, 
         team_conf = NA, opp_conf = NA, conf_game = NA) %>%
  select(date, team, opponent, location, team_score, opp_score,
         score_diff, total_score, game_id, opp_game_id, team_conf, opp_conf,
         year, month, day, season_id, D1, OT)

### Game IDs
for(i in 1:length(teams)) {
  x[x$team == teams[i],] <- x %>%
    filter(team == teams[i]) %>%
    mutate(game_id = seq(1, sum(team == teams[i]), 1))
}

### Opp Game IDs
for(i in 1:nrow(x)) {
  x$opp_game_id[i] <- get_opp_id(x, i)[1]
}

### Confs
for(i in 1:length(teams)) {
  x$team_conf[x$team == teams[i]] <- get_conf(teams[i])
  x$opp_conf[x$opponent == teams[i]] <- get_conf(teams[i])
}
x$conf_game <- x$team_conf == x$opp_conf

ivy <- c("Brown", "Columbia", "Cornell", "Dartmouth", "Harvard", 
         "Penn", "Princeton", "Yale")

dates <- 
  filter(x, team %in% ivy) %>% 
  pull(date) %>%
  unique()

dates <- dates[dates >= "2017-12-01"]

for(j in 1:length(dates)) {
  print(j)
  y <- filter(x, date < dates[j])
  y$weights <- 0
  for(i in 1:nrow(y)) {
    w_team <- 1 - (max(c(0, y$game_id[y$team == y$team[i] & !is.na(y$scorediff)])) - y$game_id[i])/
      max(c(0, y$game_id[y$team == y$team[i] & !is.na(y$scorediff)]))
    w_opponent <- 1 - (max(c(0, y$game_id[y$team == y$opponent[i] & !is.na(y$scorediff)])) - y$opp_game_id[i])/
      max(c(1, y$game_id[y$team == y$opponent[i] & !is.na(y$scorediff)]))
    rr <- mean(c(w_team, w_opponent))
    y$weights[i] <- 1/(1 + (0.5^(5 * rr)) * exp(-rr))
  }   
  
  lm.hoops <- lm(score_diff ~ team + opponent + location, weights = weights, data = y) 
  lm.off <- lm(team_score ~ team + opponent + location, weights = weights, data = y) 
  lm.def <- lm(opp_score ~ team + opponent + location, weights = weights, data = y) 
  
  
  z <- filter(x, team_conf == "Ivy", date == dates[j], ! (location == "V" & conf_game))
  z$pred_score_diff <- predict(lm.hoops, newdata = z)
  z$pred_total_score <- predict(lm.off, newdata = z) + predict(lm.def, newdata = z)
  
  if(j ==  1) {
    ivy_games <- z
  }else{
    ivy_games <- bind_rows(ivy_games, z)
  }
}

ivy_games <- 
  select(ivy_games, date, team, opponent, location, team_score, opp_score,
       score_diff, pred_score_diff, total_score, pred_total_score) %>%
  arrange(date)

write.csv(ivy_games, "history_2017_18.csv", row.names = F)
