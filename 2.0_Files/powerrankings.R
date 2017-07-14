pr_compute <- function(by_conf) {
  powerrankings <- data.frame(Team = rep(NA, length(teams)),
                              Conference = rep(NA, length(teams)),
                              YUSAG_Coefficient = rep(NA, length(teams)))
  powerrankings[1, ] <- c(teams[1], "Southland", 0)
  
  for(i in 2:(length(teams))) {
    teamcoef <- 
      lm.hoops$coefficients[paste("team", teams[i], sep = "")] 
    opponentcoef <- 
      lm.hoops$coefficients[paste("opponent", teams[i], sep = "")]
    tmp <- c(teams[i], confs$conference[confs$team == teams[i]], round((teamcoef - opponentcoef)/2, 2))
    powerrankings[i, ] <- tmp
  }
  powerrankings$YUSAG_Coefficient <- as.numeric(powerrankings$YUSAG_Coefficient)
  
  if(!by_conf) {
    powerrankings <- powerrankings[order(powerrankings$YUSAG_Coefficient, decreasing = T), ]
    powerrankings$rank <- seq(1, 351, 1)
    write.table(powerrankings, "2.0_Files/Power_Rankings/Powerrankings.csv", row.names = F, col.names = T, sep = ",")
    return(powerrankings)
  }
  else{
    conferences <- sort(unique(confs$conference))
    
    summ <- data.frame(conf = conferences,
                       mean = rep(NA, length(conferences)),
                       median = rep(NA, length(conferences)),
                       min = rep(NA, length(conferences)),
                       max = rep(NA, length(conferences)),
                       sd = rep(NA, length(conferences)))
                       
    summ$mean <- round(aggregate(YUSAG_Coefficient ~ Conference, data = powerrankings, mean)$YUSAG_Coefficient, 1)
    summ$median <- round(aggregate(YUSAG_Coefficient ~ Conference, data = powerrankings, median)$YUSAG_Coefficient, 1)
    summ$max <- round(aggregate(YUSAG_Coefficient ~ Conference, data = powerrankings, max)$YUSAG_Coefficient, 1)
    summ$min <- round(aggregate(YUSAG_Coefficient ~ Conference, data = powerrankings, min)$YUSAG_Coefficient, 1)
    summ$sd <- round(aggregate(YUSAG_Coefficient ~ Conference, data = powerrankings, sd)$YUSAG_Coefficient, 1)
    summ <- summ[order(summ$median, decreasing = T), ]
    write.table(summ, "2.0_Files/Power_Rankings/conf_summary.csv", row.names = F, col.names = T, sep = ",")
    
    for(i in 1:length(conferences)) {
      powerrankings <- powerrankings[order(powerrankings$YUSAG_Coefficient, decreasing = T), ]
      powerrankings$rank <- seq(1, 351, 1)
      tmp <- powerrankings[powerrankings$Conference == conferences[i], ]
      tmp$Conference <- tmp$Conference 
      tmp$info <- paste("(Conference Rank:  ", grep(conferences[i], summ$conf), ")", sep = "")
      tmp <- tmp[order(tmp$YUSAG_Coefficient, decreasing = T), ]
      tmp$Conference_Rank <- 1:nrow(tmp)
      tmp$record <- NA
      tmp$conf_record <- NA
      for(j in 1:nrow(tmp)) {
        wins <- round(sum(x$wins[x$team == tmp$Team[j] & x$reg_season]))
        losses <- max(x$game_id[x$team == tmp$Team[j] & x$reg_season]) - wins
        tmp$record[j] <- paste(wins, losses, sep = " - " )
        conf_wins <- round(sum(x$wins[x$team == tmp$Team[j] & x$reg_season & x$conf_game]))
        conf_losses <- length(x$wins[x$team == tmp$Team[j] & x$reg_season & x$conf_game]) - conf_wins
        tmp$conf_record[j] <- paste(conf_wins, conf_losses, sep = " - " )
      }
      if(i > 1){
        pr <- rbind(pr, tmp)
      }
      else{
        pr <- tmp
      }
    }
    write.table(pr, "2.0_Files/Power_Rankings/pr_by_conf.csv", row.names = F, col.names = T, sep = ",")
    return(pr)
  }
}

