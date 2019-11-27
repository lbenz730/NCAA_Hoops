teams <- unique(x$team)

### Compute team's winning percentage
wp_compute <- function(team, team_iq) {
  games <- x[x$team == team & x$opponent != team_iq,]
  wwins <- sum(1.3 * games$wins[games$location == "V"]) + 
    sum(0.7 * games$wins[games$location == "H"]) +
    sum(games$wins[games$location == "N"])
  wloses <- sum(1.3 * (1 - games$wins[games$location == "H"])) + 
    sum(0.7 * (1 - games$wins[games$location == "V"])) +
    sum((1 - games$wins[games$location == "N"]))
  wp <- wwins/(wwins + wloses)
  return(wp)
}

### Compute team's opponent winning percentage
owp_compute <- function(team) {
  opponents <- x$opponent[x$team == team]
  owp <- sapply(opponents, wp_compute, team_iq = team)
  return(mean(owp))
}

### Compute team's opponent's opponent winning percentage
oowp_compute <- function(team) {
  opponents <- x$opponent[x$team == team]
  oowp <- sapply(opponents, owp_compute)
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
    stats$rpi <- sapply(teams, rpi_calc)
    stats <- stats[order(stats$rpi, decreasing = T),]
    stats$rpi_rank <- 1:length(teams)
    write.table(stats, "3.0_Files/Bracketology/rpi.csv", row.names = F, col.names = T, sep = ",")
  }
  else{
    stats <- read.csv("3.0_Files/Bracketology/rpi.csv", as.is = T)
  }
  return(stats)
}