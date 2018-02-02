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
  }
  
  bracket$blend <- 0.10 * bracket$rpi_rank + 0.35 * bracket$wab_rank + 
    0.10 * bracket$sor_rank + 0.10 * bracket$yusag_rank + 0.35 * bracket$resume_rank
  
  autobid_calc <- function(conf) {
    for(i in 1:length(confs$team[confs$conference == conf])) {
      tmp <- by_conf$Team[by_conf$Conference_Rank == i & by_conf$Conference == conf]
      if(confs$eligible[confs$team == tmp] & !confs$eliminated[confs$team == tmp]) {
        return(tmp)
      }
    }
  }
  
  bracket <- bracket[order(bracket$blend, decreasing = F),]
  
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
    bracket <- bracket[order(bracket$blend, decreasing = F),]
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