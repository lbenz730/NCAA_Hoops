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
  return(20 * harmonic_mean(team_strength, opp_strength)/(10 + abs(team_strength - opp_strength)))
}

### Get Games for a certain date
get_games <- function(today, D, M, Y) {
  if(today){
    date<- unclass(as.POSIXlt(Sys.time()))
    Y <- 1900 + date$year
    M <- 1 + date$mon
    D <- date$mday
  }
  return(filter(y, day == D, month == M, year == Y))
}


### Update History (Weekly Basis)
write_history <- function(update) {
  history <- supressWarnings(read.csv("2.0_Files/History/2017_18_history.csv", as.is = T))
  if(update) {
    date<- unclass(as.POSIXlt(Sys.time()))
    Y <- 1900 + date$year
    M <- 1 + date$mon
    D <- date$mday
    
    ### Get one week from current date
    if((D <= 25 & M != 11 & M != 2) | (D <= 24 & M == 11) | (D <= 21 & M == 2)) {
      days <- D:(D + 6)
      months <- rep(M, 7)
      years <- rep(Y, 7)
    }else if(D > 25 & is.element(M, c(1,3))) {
      days <- c(D:31, 1:(D - 25))
      months <- c(rep(M, (31 - D)), rep(M + 1, (D - 24)))
      years <- rep(Y, 7)
    }else if(D > 25 & M == 12) {
      days <- c(D:31, 1:(D - 25))
      months <- c(rep(M, (31 - D)), rep(1, (D - 24)))
      years <- c(rep(Y, (31 - D)), rep(Y + 1, (D - 24)))
    }else if(D > 21 & M == 2){
      days <- c(D:28, 1:(D - 22))
      months <- c(rep(M, (28 - D)), rep(M + 1, (D - 21)))
      years <- rep(Y, 7)
    }else{
      days <- c(D:30, 1:(D - 24))
      months <- c(rep(M, (30 - D)), rep(M + 1, (D - 24)))
      years <- rep(Y, 7)
    }
    
    dates <- paste(months, days, years, sep = "_")
    tmp <- y %>% select(year, month, day, team, opponent, 
                        location, teamscore, oppscore, scorediff, 
                        predscorediff, GEI) %>%
      mutate(date = paste(month, day, year, sep = "_")) %>%
      filter(is.element(date, dates))
    
    history <- rbind(history, tmp[-12])
    
    ### Get past week of Results
    if(D > 7) {
      days <- (D - 7):(D - 1) 
      months <- rep(M, 7)
      years <- rep(Y, 7)
    }else if(D <= 7 & M != 12 & M != 1 & M != 3) {
      days <- c((24 + D):31, 1:(D - 1))
      months <- c(rep((M - 1), (8 - D)), rep(M, (D - 1)))
      years <- rep(Y, 7)
    }else if(D <= 7 & M == 1) {
      days <- c((24 + D):31, 1:(D - 1))
      months <- c(rep(12, (8 - D)), rep(1, (D - 1)))
      years <- c(rep((Y - 1), (8 - D)), rep(Y, (D - 1)))
    }else if(D <= 7 & M == 12) {
      days <- c((23 + D):30, 1:(D - 1))
      months <- c(rep(12, (8 - D)), rep(1, (D - 1)))
      years <- rep(Y, 7)
    }else{
      days <- c((21 + D):28, 1:(D - 1))
      months <- c(rep(12, (8 - D)), rep(1, (D - 1)))
      years <- rep(Y, 7)
    }
    
    dates <- paste(months, days, years, sep = "_")
    tmp <- history %>% select(year, month, day, team, opponent, 
                              location, teamscore, oppscore, scorediff, 
                              predscorediff, GEI) %>%
      mutate(date = paste(month, day, year, sep = "_")) %>%
      filter(is.element(date, dates))
    
    history <- mutate(history, "date" = paste(month, day, year, sep = "_")) 
    history[is.element(history$date, dates),] <- history %>% 
      filter(is.element(date, dates)) %>%
      mutate(teamscore = tmp$teamscore, oppscore = tmp$oppscore, scorediff = tmp$scorediff)
    
    write.table(history[-12], "2.0_Files/History/2017_18_history.csv", row.names = F, col.names = T, sep = ",")
  }
  return(history)
}

