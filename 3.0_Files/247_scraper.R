library(dplyr)
conferences <- c("ACC", "Big-12", "Big-Ten", "Pac-12", "SEC", "AAC",
                 "C-USA", "MAC", "M-West", "A-10", "A-East", "A-Sun",
                 "Big-East", "Big-west", "Big-Sky", "B-South", "CAA",
                 "Horizon", "Ivy", "MAAC", "MEAC", "MVC", "NEC", "OVC",
                 "Patriot", "SBC", "SLC", "Southern", "Summit", "SW-AC",
                 "WAC", "WCC")

year <- 2009

for(conf in conferences) {
  print(paste("Scraping", conf))
  url <- paste0("https://247sports.com/Season/", year, "-Basketball/CompositeTeamRankings?Conference=", conf)
  x <- scan(url, sep = "\n", what = "")
  x <- x[85]
  y <- strsplit(x, '<div class=\"rank left\">')[[1]][-c(1:2)]
  
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
      recruits$team[i] <- strsplit(gsub("</span>.*", "", y[i]), "<span>")[[1]][2] 
      recruits$conf[i] <- conf
      recruits$conf_rank[i] <- as.numeric(gsub("[^0-9]", "", gsub(' </div>  <div class=\"other\">.*', "", y[i])))
      z <- strsplit(y[i], "Total: ")[[1]][2]  
      recruits$num_star_players[i] <- as.numeric(substring(z, 1, 1))
      recruits[i,c("stars_5", "stars_4", "stars_3")] <- 
        as.numeric(substring(strsplit(z, "<span class=\"icon-starsolid grey\"></span>: ")[[1]], 1, 1)[-1])
      recruits$avg_recruit_score[i] <- as.numeric(strsplit(strsplit(z, "<b>Avg:</b> ")[[1]][2], "</li>")[[1]][1])
      recruits$composite_score[i] <- as.numeric(strsplit(strsplit(z, paste0('/Season/', year, '-Basketball/Commits/\">'))[[1]][2], "</a>")[[1]][1])
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