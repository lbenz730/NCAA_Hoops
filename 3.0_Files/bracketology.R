library(randomForest)
make_bracket <- function(tourney) {
  bracket <- data.frame("team" = teams,
                        "conf" = rep(NA, 351),
                        "yusag_coeff" = rep(NA, 351),
                        "rpi" = rep(NA, 351),
                        "sor" = rep(NA, 351),
                        "wab" = rep(NA, 351),
                        "qual_bonus" = rep(NA, 351),
                        "yusag_rank" = rep(NA, 351),
                        "rpi_rank" = rep(NA, 351),
                        "sor_rank" = rep(NA, 351),
                        "resume_rank" = rep(NA, 351),
                        "wab_rank" = rep(NA, 351),
                        "mid_major" = rep(NA, 351),
                        "loss_bonus" = rep(NA, 351),
                        stringsAsFactors = F)
  
  ### Get Advanced Metric Ranks
  rpi <- rpi[order(rpi$rpi, decreasing = T),]
  rpi$rank <- 1:351
  
  resumes <- resumes[order(resumes$sor, decreasing = T),]
  resumes$sor_rank <- 1:351
  
  resumes <- resumes[order(resumes$wab, decreasing = T),]
  resumes$wab_rank <- 1:351
  
  resumes <- resumes[order(resumes$qual_bonus, decreasing = T),]
  resumes$resume_rank <- 1:351
  
  
  
  for(i in 1:length(teams)) {
    bracket$yusag_coeff[i] <- powranks$YUSAG_Coefficient[powranks$Team == teams[i]]
    bracket$conf[i] <- get_conf(teams[i])
    bracket$rpi[i] <- rpi$rpi[rpi$team == teams[i]]
    bracket$sor[i] <- resumes$sor[resumes$team == teams[i]]
    bracket$wab[i] <- resumes$wab[resumes$team == teams[i]]
    bracket$qual_bonus[i] <- resumes$qual_bonus[resumes$team == teams[i]]
    bracket$yusag_rank[i] <- powranks$rank[powranks$Team == teams[i]]
    bracket$rpi_rank[i] <- rpi$rank[rpi$team == teams[i]]
    bracket$sor_rank[i] <- resumes$sor_rank[resumes$team == teams[i]]
    bracket$wab_rank[i] <- resumes$wab_rank[resumes$team == teams[i]]  
    bracket$resume_rank[i] <- resumes$resume_rank[resumes$team == teams[i]]
    bracket$mid_major[i] <- confs$mid_major[confs$team == teams[i]]
    bracket$wins[i] <- resumes$wins[resumes$team == teams[i]]
    bracket$losses[i] <- resumes$losses[resumes$team == teams[i]]
    bracket$loss_bonus[i] <- resumes$losses[resumes$team == teams[i]] <= 4 &
       bracket$conf[i] %in% c("Big 10", "Big 12", "Big East", "ACC", "Pac 12", "Big 12")
  }
  
  bracket$blend <- 0.25 * bracket$rpi_rank + 0.1 * bracket$wab_rank + 
    0.1 * bracket$sor_rank + 0.1 * bracket$yusag_rank + 0.45 * bracket$resume_rank +  
    2 * as.numeric(bracket$mid_major)
  bracket <- bracket[order(bracket$blend, decreasing = F),]
  
  autobid_calc <- function(conf) {
    tmp <- bracket$team[bracket$conf == conf]
    for(i in 1:length(tmp)) {
      if(confs$eligible[confs$team == tmp[i]] & !confs$eliminated[confs$team == tmp[i]]) {
        return(tmp[i])
      }
    }
  }
  
  
  
  if(tourney == T) {
    ### Get Autobids
    autobids <- vector()
    for(j in 1:length(unique(confs$conference))){
      autobids[j] <- autobid_calc(unique(confs$conference)[j])
    }
    bracket$autobid <- is.element(bracket$team, autobids)
    
    ### Get At-Large bids
    tmp <- bracket[!bracket$autobid, ]
    j <- 1
    z <- 1
    atlarge <- vector()
    while(j <= 36) {
      for(k in z:length(teams)){
        z <- z + 1
        if(confs$eligible[confs$team == tmp$team[k]]) {
          atlarge[j] <- as.character(tmp$team)[k]
          j <- j + 1
          break
        }
      }
    }
    
    ### Write Bracket    
    bracket$atlarge <- is.element(bracket$team, atlarge)
    bracket <-rbind(bracket[bracket$autobid,], bracket[bracket$atlarge,])
    
    ### Seed Teams 
    z <- read.csv("2.0_Files/Bracketology/historical/bracket_math_2017.csv", as.is = T)
    z <- rbind(z, read.csv("2.0_Files/Bracketology/historical/bracket_math_2016.csv", as.is = T))
    z <- z[!is.na(z$seed),]
    seed.rf <- randomForest(seed ~ yusag_rank + sor_rank + wab_rank + rpi_rank + resume_rank + mid_major
                            + wins + losses + loss_bonus, data = z, mtry = 3, nodesize = 1)
    bracket$pred_seed <- predict(seed.rf, newdata = bracket)
    bracket <- bracket[order(bracket$pred_seed, decreasing = F),]
    remove <- names(bracket) %in% c("mid_major", "wins", "losses", "loss_bonus")
    bracket <- bracket[, !remove]
    bracket$seed_overall <- 1:68
    bracket$seed_line <- c(rep(1,4), rep(2,4), rep(3,4), rep(4,4), rep(5,4),
                           rep(6,4), rep(7,4), rep(8,4), rep(9,4), rep(10,4),
                           rep(11,6), rep(12,4), rep(13,4), rep(14,4), rep(15,4),
                           rep(16,6))
    f4 <- bracket$seed_overall[!bracket$autobid][33:36]
    bracket$first4 <- is.element(bracket$seed_overall, f4) | is.element(bracket$seed_overall, c(65:68))
    write.table(bracket, "2.0_Files/Bracketology/bracket.csv", row.names = F, col.names = T, sep = ",")
    
    ### First teams out
    j <- 37
    z <- 37
    bubble <- vector()
    while(j <= 52) {
      for(k in z:length(teams)){
        z <- z + 1
        if(confs$eligible[confs$team == tmp$team[k]]) {
          bubble[j - 36] <- as.character(tmp$team)[k]
          j <- j + 1
          break
        }
      }
    }
    bubble <- tmp[is.element(tmp$team, bubble),]
    write.table(bubble, "2.0_Files/Bracketology/bubble.csv", row.names = F, col.names = T, sep = ",")

    ### Bid Summary by Conference
    tmp <- bracket
    tmp$bid <- 1
    bids <- as.data.frame(aggregate(bid ~ conf, data = tmp, sum))
    bids <- bids[order(bids$bid, decreasing = T),]
    write.table(bids, "2.0_Files/Bracketology/bids.csv", row.names = F, col.names = T, sep = ",")
    
    return(bracket)
  }
  else{
    write.table(bracket, "2.0_Files/Bracketology/bracket_math.csv", row.names = F, col.names = T, sep = ",")
    return(bracket)
  }
}