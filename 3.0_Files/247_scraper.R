library(dplyr)
conferences <- c("ACC", "Big-12", "Big-Ten", "Pac-12", "SEC", "AAC",
                 "C-USA", "MAC", "M-West", "A-10", "A-East", "A-Sun",
                 "Big-East", "Big-west", "Big-Sky", "B-South", "CAA",
                 "Horizon", "Ivy", "MAAC", "MEAC", "MVC", "NEC", "OVC",
                 "Patriot", "SBC", "SLC", "Southern", "Summit", "SW-AC",
                 "WAC", "WCC")

year <- 2019

for(conf in conferences) {
  print(paste("Scraping", conf))
  url <- paste0("https://247sports.com/Season/", year, "-Basketball/CompositeTeamRankings?Conference=", conf)
  x <- scan(url, sep = "\n", what = "")
  x <- x[75]
  y <- strsplit(x, '<div class=\"rank-column\">')[[1]][-1]
  
  if(length(y) > 0) {
    recruits <- data.frame("team" = rep(NA, length(y)),
                           "conf" = rep(NA, length(y)),
                           "conf_rank" = rep(NA, length(y)),
                           "stars_5" = rep(NA, length(y)),
                           "stars_4" = rep(NA, length(y)),
                           "stars_3" = rep(NA, length(y)),
                           "num_star_players" = rep(NA, length(y)),
                           "avg_recruit_score" = rep(NA, length(y)),
                           "composite_score" = rep(NA, length(y)))
    
    for(i in 1:nrow(recruits)) {
      recruits$team[i] <- gsub("\".*", "", strsplit(y[i], "<img alt=\"", "")[[1]][2])
      recruits$conf[i] <- conf
      recruits$conf_rank[i] <- as.numeric(gsub("[^0-9]", "", gsub(' </div>  <div class=\"other\">.*', "", y[i])))
      z <- strsplit(y[i], "/Commits/\">")[[1]][3]  
      recruits$num_star_players[i] <- as.numeric(substring(z, 1, 1))
      star5 <- substring(gsub("^\\s*", "", gsub("[^0-9 ]", "", gsub(".*<h2>5-Star</h2>", "", y[i]))), 1, 1)
      star4 <- substring(gsub("^\\s*", "", gsub("[^0-9 ]", "", gsub(".*<h2>4-Star</h2>", "", y[i]))), 1, 1)
      star3 <- substring(gsub("^\\s*", "", gsub("[^0-9 ]", "", gsub(".*<h2>3-Star</h2>", "", y[i]))), 1, 1)
      recruits[i,c("stars_5", "stars_4", "stars_3")] <- as.numeric(c(star5, star4, star3))
      recruits$avg_recruit_score[i] <- as.numeric(gsub(" </div>.*", "", gsub(".*<div class=\"avg\"> ", "", y[i])))
      recruits$composite_score[i] <- as.numeric(gsub(" </a>.*", "", strsplit(y[i], paste0('/Season/', year, '-Basketball/Commits/\">'))[[1]][4]))
    }
    
    if(conf == "ACC") {
      master <- recruits 
    }
    else{
      master <- rbind(master, recruits)
    }
  }
}

### Fix Errors by hard code (check for composite score NA)
if(year == 2018) {
  master$composite_score[master$team == "Santa Clara"] <- 37.44
}else if(year == 2017) {
  master$composite_score[master$team == "Santa Clara"] <- 27.40
  master$composite_score[master$team == "Loyola Marymount"] <- 13.33
  master$composite_score[master$team == "San Francisco"] <- 13.33
}else if(year == 2016) {
  master <- filter(master, team != "Abilene Christian")
  master$composite_score[master$team == "Loyola Marymount"] <- 13.33
  master$composite_score[master$team == "San Francisco"] <- 6.67
}

### Save
master <- master[order(master$composite_score, decreasing = T),]
write.csv(master, paste0("247_rankings_", year, ".csv"), row.names = F)
