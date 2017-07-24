### Get opponent game id
get_opp_id <- function(data, i) {
  return(data$game_id[data$team == data$opponent[i] & data$opponent == data$team[i] & data$month == data$month[i] & data$day == data$day[i]])
}

### get team's conference
get_conf <- function(team) {
  return(confs$conference[confs$team == team])
}

### Compute Haromonic Mean
harmonic_mean <- function(a,b) {
  return(1/(mean(1/c(a, b))))
}

### Compute Game Excitement Index
compute_GEI <- function(data) {
  adj <- abs(min(powranks$YUSAG_Coefficient))
  team_strength <- powranks$YUSAG_Coefficient[powranks$Team == data$team] + adj + 1
  opp_strength <- powranks$YUSAG_Coefficient[powranks$Team == data$opponent] + adj + 1
  score <- abs(data$predscorediff)
  team_conf_rank <- by_conf$Conference_Rank[by_conf$Team == data$team]
  opp_conf_rank <- by_conf$Conference_Rank[by_conf$Team == data$opponent]
  return(harmonic_mean(team_strength, opp_strength)/(1 + abs(team_strength - opp_strength)))
}