make_bracket <- function(tourney) {
  bracket <- data.frame("team" = teams,
                        "conf" = rep(NA, 351),
                        "yusag_coeff" = rep(NA, 351),
                        "rpi" = rep(NA, 351),
                        "sor" = rep(NA, 351),
                        "wab" = rep(NA, 351),
                        "yusag_rank" = rep(NA, 351),
                        "rpi_rank" = rep(NA, 351),
                        "sor_rank" = rep(NA, 351),
                        "resume_rank" = rep(NA, 351),
                        "wab_rank" = rep(NA, 351))
  
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
    bracket$conf[i] <- powranks$Conference[powranks$Team == teams[i]]
    bracket$rpi[i] <- rpi$rpi[rpi$team == teams[i]]
    bracket$sor[i] <- resumes$sor[resumes$team == teams[i]]
    bracket$wab[i] <- resumes$wab[resumes$team == teams[i]]
    bracket$yusag_rank[i] <- powranks$rank[powranks$Team == teams[i]]
    bracket$rpi_rank[i] <- rpi$rank[rpi$team == teams[i]]
    bracket$sor_rank[i] <- resumes$sor_rank[resumes$team == teams[i]]
    bracket$wab_rank[i] <- resumes$wab_rank[resumes$team == teams[i]]  
    bracket$resume_rank[i] <- resumes$resume_rank[resumes$team == teams[i]]
  }
  
  bracket$blend <- 0.3 * bracket$rpi_rank + 0.2 * bracket$wab_rank + 
    0.2 * bracket$sor_rank + 0.15 * bracket$yusag_rank + 0.15 * bracket$resume_rank
  
  bracket <- bracket[order(bracket$blend, decreasing = F), ]
  if(tourney == T) {
    autobids <- by_conf$Team[by_conf$Conference_Rank == 1]
    bracket$autobid <- is.element(bracket$team, autobids)
    atlarge <- bracket[!bracket$autobid, ]
    bracket <- rbind(bracket[bracket$autobid,], atlarge[1:36,])
    bracket <- bracket[order(bracket$blend, decreasing = F), ]
    bracket$seed_overall <- 1:68
    bracket$seed_line <- c(rep(1,4), rep(2,4), rep(3,4), rep(4,4), rep(5,4),
                           rep(6,4), rep(7,4), rep(8,4), rep(9,4), rep(10,4),
                           rep(11,6), rep(12,4), rep(13,4), rep(14,4), rep(15,4),
                           rep(16,6))
    f4 <- bracket$seed_overall[!bracket$autobid][33:36]
    bracket$first4 <- is.element(bracket$seed_overall, f4) | is.element(bracket$seed_overall, c(65:68))
    write.table(bracket, "2.0_Files/Bracketology/bracket.csv", row.names = F, col.names = T, sep = ",")
    tmp <- bracket
    tmp$bid <- 1
    bids <- as.data.frame(aggregate(bid ~ conf, data = tmp, sum))
    bids <- bids[order(bids$bid, decreasing = T),]
    write.table(bids, "2.0_Files/Bracketology/bids.csv", row.names = F, col.names = T, sep = ",")
    bubble <- atlarge[37:52,]
    write.table(bubble, "2.0_Files//Bracketology/bubble.csv", row.names = F, col.names = T, sep = ",")
    return(bracket)
  }
  else{
    write.table(bracket, "2.0_Files//Bracketology/bracket_math.csv", row.names = F, col.names = T, sep = ",")
    return(bracket)
  }
}