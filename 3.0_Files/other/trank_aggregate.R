library(dplyr)

players_2015 <- read.csv("Info/trank_ppg_2015.csv", as.is = T)
players_2016 <- read.csv("Info/trank_ppg_2016.csv", as.is = T)
players_2017 <- read.csv("Info/trank_ppg_2017.csv", as.is = T)
players_2018 <- read.csv("Info/trank_ppg_2018.csv", as.is = T)
players_2019 <- read.csv("Info/trank_ppg_2019_pre.csv", as.is = T)
transfers <- read.csv("Info/transfers.csv", as.is = T)
grad_transfers <- read.csv("Info/grad_transfers.csv", as.is = T)

mins_2017 <- read.csv("Info/mins_2017.csv", as.is = T)
mins_2018 <- read.csv("Info/mins_2018.csv", as.is = T)
mins_2019 <- read.csv("Info/mins_2019.csv", as.is = T)
mins_2020 <- read.csv("Info//mins_2020.csv", as.is = T)

players_2016 <- mutate(players_2016, "id" = paste(player, team))
players_2017 <- mutate(players_2017, "id" = paste(player, team))
players_2018 <- mutate(players_2018, "id" = paste(player, team))
players_2019 <- mutate(players_2019, "id" = paste(player, team))

players_2016$returning <- players_2016$id %in% players_2017$id
players_2017$returning <- players_2017$id %in% players_2018$id
players_2018$returning <- players_2018$id %in% players_2019$id

ppg_2016 <- 
  as.data.frame(filter(players_2016, !returning) %>% 
  group_by(team) %>% summarise("departing_ppg" = sum(trank_ppg * as.numeric(mins_pct >= 40))))

tmp <- 
  as.data.frame(filter(players_2016, returning) %>% 
  group_by(team) %>% summarise("returning_ppg" = sum(trank_ppg * as.numeric(mins_pct >= 40))))
tmp <- rbind(tmp, data.frame("team" = setdiff(ppg_2016$team, tmp$team),
                             "returning_ppg" = rep(0, length(setdiff(ppg_2016$team, tmp$team)))))
            
ppg_2016 <- merge(ppg_2016, tmp, "team")
ppg_2016$team <- gsub("IPFW", "Fort Wayne", ppg_2016$team)
ppg_2016$team <- gsub("Arkansas Little Rock", "Little Rock", ppg_2016$team)


ppg_2017 <- 
  as.data.frame(filter(players_2017, !returning) %>% 
                  group_by(team) %>% summarise("departing_ppg" = sum(trank_ppg * as.numeric(mins_pct >= 40))))

tmp <- 
  as.data.frame(filter(players_2017, returning) %>% 
                  group_by(team) %>% summarise("returning_ppg" = sum(trank_ppg * as.numeric(mins_pct >= 40))))
tmp <- rbind(tmp, data.frame("team" = setdiff(mins_2018$team, tmp$team),
                             "returning_ppg" = rep(0, length(setdiff(mins_2018$team, tmp$team)))))

ppg_2017 <- merge(ppg_2017, tmp, "team")


ppg_2016 <- mutate(ppg_2016, "transfer_ppg" = 0)
ppg_2017 <- mutate(ppg_2017, "transfer_ppg" = 0)

x <- rbind(filter(transfers, year == 2017), filter(grad_transfers, year == 2017))

for(i in 1:nrow(x)) {
  if(x$player[i] %in% players_2016$player) {
    index <- grep(x$player[i], players_2016$player)
    ppg <- players_2016$trank_ppg[index]
    ppg_2016$transfer_ppg[ppg_2016$team == x$team[i]] <- 
      ppg_2016$transfer_ppg[ppg_2016$team == x$team[i]] + ppg * (players_2016$mins_pct[index] >= 40)
  }
  else if(x$player[i] %in% players_2015$player) {
    index <- grep(x$player[i], players_2015$player)
    ppg <- players_2015$trank_ppg[index]
    ppg_2016$transfer_ppg[ppg_2016$team == x$team[i]] <- 
      ppg_2016$transfer_ppg[ppg_2016$team == x$team[i]] + ppg * (players_2015$mins_pct[index] >= 40)
  }
}

x <- rbind(filter(transfers, year == 2018), filter(grad_transfers, year == 2018))
for(i in 1:nrow(x)) {
  if(x$player[i] %in% players_2017$player) {
    index <- grep(x$player[i], players_2017$player)
    ppg <- players_2017$trank_ppg[index]
    ppg_2017$transfer_ppg[ppg_2017$team == x$team[i]] <- 
      ppg_2017$transfer_ppg[ppg_2017$team == x$team[i]] + ppg * (players_2017$mins_pct[index] >= 40)
  }
  else if(x$player[i] %in% players_2016$player) {
    index <- grep(x$player[i], players_2016$player)
    ppg <- players_2016$trank_ppg[index]
    ppg_2017$transfer_ppg[ppg_2017$team == x$team[i]] <- 
      ppg_2017$transfer_ppg[ppg_2017$team == x$team[i]] + ppg * (players_2016$mins_pct[index] >= 40)
  }
}

ppg_2016$net_ppg <- ppg_2016$returning_ppg + ppg_2016$transfer_ppg - ppg_2016$departing_ppg
ppg_2017$net_ppg <- ppg_2017$returning_ppg + ppg_2017$transfer_ppg - ppg_2017$departing_ppg

stats_2016 <- merge(ppg_2016, mins_2017, "team")
stats_2017 <- merge(ppg_2017, mins_2018, "team")
write.csv(stats_2016, "Info/stats_2016.csv", row.names = F)
write.csv(stats_2017, "Info/stats_2017.csv", row.names = F)

transfers_2019 <- read.csv("Info/transfers_2019.csv", as.is = T)

ppg_2018 <- 
  as.data.frame(filter(players_2018, !returning) %>% 
                  group_by(team) %>% summarise("departing_ppg" = sum(trank_ppg * as.numeric(mins_pct >= 40))))
ppg_2018 <- rbind(ppg_2018, data.frame("team" = setdiff(mins_2019$team, ppg_2018$team),
                      "departing_ppg" = rep(0, length(setdiff(mins_2019$team, ppg_2018$team)))))

tmp <- 
  as.data.frame(filter(players_2018, returning) %>% 
                  group_by(team) %>% summarise("returning_ppg" = sum(trank_ppg * as.numeric(mins_pct >= 40))))
tmp <- rbind(tmp, data.frame("team" = setdiff(mins_2019$team, tmp$team),
                             "returning_ppg" = rep(0, length(setdiff(mins_2019$team, tmp$team)))))

ppg_2018 <- merge(ppg_2018, tmp, "team")
ppg_2018$transfer_ppg <- 0

for(i in 1:nrow(transfers_2019)) {
  new_team <- transfers_2019$new_team[i]
  ppg_2018$transfer_ppg[ppg_2018$team == new_team] <- 
    ppg_2018$transfer_ppg[ppg_2018$team == new_team] + 
    transfers_2019$trank_ppg[i] * (transfers_2019$mins_pct[i] > 40)
}


ppg_2018$net_ppg <- ppg_2018$returning_ppg + ppg_2018$transfer_ppg - ppg_2018$departing_ppg
stats_2018 <- merge(ppg_2018, mins_2019, "team")
write.csv(stats_2018, "Info/stats_2018.csv", row.names = F)
