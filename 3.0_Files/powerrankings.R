pr_compute <- function(by_conf) {
  ### Avg to scale coefficintes to 0
  avg <- mean(lm.hoops$coefficients[2:353], na.rm = T)
  off_avg <-  mean(lm.off$coefficients[2:353], na.rm = T)
  def_avg <- mean(lm.def$coefficients[2:353], na.rm = T)
  
  ### Store Power Rankings Data Frame
  power_rankings <- data.frame("team" = teams,
                               "conference" = sapply(teams, get_conf))
  
  power_rankings <- mutate(power_rankings,
                           yusag_coeff = c(0, lm.hoops$coefficients[2:353]) - avg,
                           off_coeff = c(0, lm.off$coefficients[2:353]) - off_avg,
                           def_coeff = c(0, lm.def$coefficients[2:353]) - def_avg)
  power_rankings <- 
    arrange(power_rankings, desc(yusag_coeff)) %>%
    mutate(rank = 1:353) %>%
    arrange(desc(off_coeff)) %>%
    mutate(off_rank = 1:353) %>%
    arrange(desc(def_coeff)) %>%
    mutate(def_rank = 1:353) %>%
    arrange(desc(yusag_coeff))
  
  ### Return w/out sorting by conference
  if(!by_conf) {
    write.csv(power_rankings, "3.0_Files/Power_Rankings/power_rankings.csv", row.names = F)
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
      # for(j in 1:nrow(tmp)) {
      #   wins <- round(sum(x$wins[x$team == tmp$team[j] & x$reg_season]))
      #   losses <- max(x$game_id[x$team == tmp$team[j] & x$reg_season]) - wins
      #   tmp$record[j] <- paste(wins, losses, sep = " - " )
      #   conf_wins <- round(sum(x$wins[x$team == tmp$team[j] & x$reg_season & x$conf_game]))
      #   conf_losses <- length(x$wins[x$team == tmp$team[j] & x$reg_season & x$conf_game]) - conf_wins
      #   tmp$conf_record[j] <- paste(conf_wins, conf_losses, sep = " - " )
      # }
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

