pr_compute <- function(by_conf) {
  ### Avg to scale coefficintes to 0
  avg <- mean(lm.hoops$coefficients[2:length(teams)], na.rm = T)
  off_avg <-  mean(lm.off$coefficients[2:length(teams)], na.rm = T)
  def_avg <- mean(lm.def$coefficients[2:length(teams)], na.rm = T)
  
  ### Store Power Rankings Data Frame
  power_rankings <- data.frame("team" = teams,
                               "conference" = sapply(teams, get_conf),
                               stringsAsFactors = F)
  
  power_rankings <- mutate(power_rankings,
                           yusag_coeff = c(0, lm.hoops$coefficients[2:length(teams)]) - avg,
                           off_coeff = c(0, lm.off$coefficients[2:length(teams)]) - off_avg,
                           def_coeff = -(c(0, lm.def$coefficients[2:length(teams)]) - def_avg))
  power_rankings <- 
    arrange(power_rankings, desc(yusag_coeff)) %>%
    mutate(rank = 1:length(teams)) %>%
    arrange(desc(off_coeff)) %>%
    mutate(off_rank = 1:length(teams)) %>%
    arrange(desc(def_coeff)) %>%
    mutate(def_rank = 1:length(teams)) %>%
    arrange(desc(yusag_coeff))
  
  ### Return w/out sorting by conference
  if(!by_conf) {
    write.csv(power_rankings, "3.0_Files/Power_Rankings/power_rankings.csv", row.names = F)
    history <- read_csv("3.0_Files/History/history.csv")
    today <- unclass(as.POSIXlt(Sys.time()))
    year <- 1900 + today$year
    month <- 1 + today$mon
    day <- today$mday
    today <- as.Date(paste(year, month, day, sep = "-"))
    history <- filter(history, as.Date(date) != today)
    if(nrow(history) == 0) {
      history <- NULL 
    }
    history <- bind_rows(history, mutate(power_rankings, date = Sys.Date()))
    write.csv(history, "3.0_Files/History/history.csv", row.names = F)
    return(power_rankings)
  }
  else{
    ### Sort by conference
    conferences <- sort(unique(confs$conference))
    
    sum_stats <- 
      group_by(power_rankings, conference) %>% 
      summarise("mean" = mean(yusag_coeff),
                "median" = median(yusag_coeff),
                "min" = min(yusag_coeff),
                "max" = max(yusag_coeff),
                "sd" = sd(yusag_coeff)) %>%
      arrange(desc(median))
    
    write.csv(sum_stats, "3.0_Files/Power_Rankings/conf_summary.csv", row.names = F)
    
    for(i in 1:length(conferences)) {
      tmp <- filter(power_rankings, conference == conferences[i])
      tmp$info <- paste0("(Conference Rank:  ", which(sum_stats$conference == conferences[i]), ")")
      tmp$conference_rank <- 1:nrow(tmp)
      tmp$record <- "--"
      tmp$conf_record <- "--"
      for(j in 1:nrow(tmp)) {
        wins <- round(sum(x$wins[x$team == tmp$team[j] & x$reg_season]))
        losses <- max(x$game_id[x$team == tmp$team[j] & x$reg_season]) - wins
        tmp$record[j] <- paste(wins, losses, sep = " - " )
        conf_wins <- round(sum(x$wins[x$team == tmp$team[j] & x$reg_season & x$conf_game]))
        conf_losses <- length(x$wins[x$team == tmp$team[j] & x$reg_season & x$conf_game]) - conf_wins
        tmp$conf_record[j] <- paste(conf_wins, conf_losses, sep = " - " )
      }
      if(i > 1){
        pr <- rbind(pr, tmp)
      }
      else{
        pr <- tmp
      }
    }
    write.csv(pr, "3.0_Files/Power_Rankings/pr_by_conf.csv", row.names = F)
    return(pr)
  }
}

