teams <- unique(y$team)

### Compute team's winning percentage
wp_compute <- function(team, team_iq) {
  games <- y[y$team == team & y$opponent != team_iq,]
  wwins <- sum(1.3 * games$win[games$location == "V"]) + 
    sum(0.7 * games$win[games$location == "H"]) +
    sum(games$win[games$location == "N"])
  wloses <- sum(1.3 * (1 - games$win[games$location == "H"])) + 
    sum(0.7 * (1 - games$win[games$location == "V"])) +
    sum((1 - games$win[games$location == "N"]))
  wp <- wwins/(wwins + wloses)
  return(wp)
}

### Compute team's opponent winning percentage
owp_compute <- function(team) {
  opponents <- y$opponent[y$team == team]
  owp <- apply(as.data.frame(opponents), 1, wp_compute, team_iq = team)
  return(mean(owp))
}

### Compute team's opponent's opponent winning percentage
oowp_compute <- function(team) {
  opponents <- y$opponent[y$team == team]
  oowp <- rep(0, length(opponents))
  for(i in 1:length(opponents)) {
    oowp[i] <- owp_compute(opponents[i])
  }
  return(mean(oowp))
}

### Compute RPI
rpi_calc <- function(team) {
  rpi <- 0.25 * wp_compute(team, team) + 0.5 * owp_compute(team) + 0.25 * oowp_compute(team)
  return(round(rpi, 4))
}

### Get and return team's RPI
rpi_compute <- function(new) {
  stats <- data.frame("team" = teams,
                      "rpi" = rep(NA, length(teams)),
                      stringsAsFactors = F)
  if(new) {
    for(i in 1:length(teams)) {
      print(paste("RPI #: ", i, sep = ""))
      stats$rpi[i] <- rpi_calc(teams[i])
    }
    stats <- stats[order(stats$rpi, decreasing = T),]
    stats$rpi_rank <- 1:351
    write.table(stats, "2.0_Files/Bracketology/rpi.csv", row.names = F, col.names = T, sep = ",")
  }
  else{
    stats <- read.csv("2.0_Files/Bracketology/rpi.csv", as.is = T)
  }
  return(stats)
}