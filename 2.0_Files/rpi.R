teams <- unique(x$team)
stats <- data.frame("team" = teams,
                    "rpi" = rep(NA, length(teams)))

wp_compute <- function(team, team_iq) {
  games <- y[y$team == team & y$opponent != team_iq & y$reg_season,]
  wwins <- sum(1.4 * games$win[games$location == "V"]) + 
    sum(0.6 * games$win[games$location == "H"]) +
    sum(games$win[games$location == "N"])
  wloses <- sum(1.4 * (1 - games$win[games$location == "H"])) + 
    sum(0.6 * (1 - games$win[games$location == "V"])) +
    sum((1 - games$win[games$location == "N"]))
  wp <- wwins/(wwins + wloses)
  return(wp)
}

owp_compute <- function(team) {
  opponents <- y$opponent[y$team == team & y$reg_season]
  owp <- rep(0, length(opponents))
  for(i in 1:length(opponents)) {
    owp[i] <- wp_compute(opponents[i], team)
  }
  return(mean(owp))
}

oowp_compute <- function(team) {
  opponents <- y$opponent[y$team == team & y$reg_season]
  oowp <- rep(0, length(opponents))
  for(i in 1:length(opponents)) {
    oowp[i] <- owp_compute(opponents[i])
  }
  return(mean(oowp))
}

rpi_calc <- function(team) {
  rpi <- 0.25 * wp_compute(team, team) + 0.5 * owp_compute(team) + 0.25 * oowp_compute(team)
  return(round(rpi, 4))
}

rpi_compute <- function(new) {
  if(new) {
    for(i in 1:length(teams)) {
      print(i)
      stats$rpi[i] <- rpi_calc(teams[i])
    }
    write.table(stats, "2.0_Files/Bracketology/rpi.csv", row.names = F, col.names = T, sep = ",")
  }
  else{
    stats <- read.csv("2.0_Files/Bracketology/rpi.csv", as.is = T)
  }
  return(stats)
}
