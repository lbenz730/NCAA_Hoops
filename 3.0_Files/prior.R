library(dplyr)
library(ggplot2)

stats_2016 <- read.csv("Info/stats_2016.csv", as.is = T)
stats_2017 <- read.csv("Info/stats_2017.csv", as.is = T)
stats_2018 <- read.csv("Info/stats_2018.csv", as.is = T)

yusag_2016 <- read.csv("Info/2016_yusag_coefficients.csv", as.is = T)
yusag_2017 <- read.csv("Info/2017_yusag_coefficients.csv", as.is = T)
yusag_2018 <- read.csv("Info/2018_yusag_coefficients.csv", as.is = T)

recruiting <- read.csv("Info/247_recruiting.csv", as.is = T)

name_dict <- read.csv("Info/name_dict.csv", as.is = T)


stats_2016$team <- 
  sapply(stats_2016$team, function(x) { name_dict$NCAA[name_dict$Trank == x] })
stats_2017$team <- 
  sapply(stats_2017$team, function(x) { name_dict$NCAA[name_dict$Trank == x] })
stats_2018$team <- 
  sapply(stats_2018$team, function(x) { name_dict$NCAA[name_dict$Trank == x] })

stats_2016 <- merge(stats_2016, yusag_2016, "team")
stats_2017 <- merge(stats_2017, yusag_2017, "team")
tmp <- data.frame("team" = stats_2018$team[! stats_2018$team %in% yusag_2018$team],
                  "off_coeff" = rep(NA, 2),
                  "def_coeff" = rep(NA, 2),
                  "yusag_coeff" = rep(NA, 2))
yusag_2018 <- rbind(yusag_2018, tmp)
write.csv(yusag_2018, "Info/2018_yusag_coefficients.csv", row.names = F)
stats_2018 <- merge(stats_2018, yusag_2018, "team")

recruiting_2016 <- filter(recruiting, year == 2016) %>% select(-conf, - year, -conf_rank)
recruiting_2016$team <- 
  sapply(recruiting_2016$team, function(x) { name_dict$NCAA[name_dict$name_247 == x] })

teams <- stats_2016$team[! stats_2016$team %in% recruiting_2016$team]
tmp <- data.frame("team" = teams,
                  "stars_5" = rep(0, length(teams)),
                  "stars_4" = rep(0, length(teams)),
                  "stars_3" = rep(0, length(teams)),
                  "num_star_players" = rep(0, length(teams)),
                  "avg_recruit_score" = rep(0, length(teams)),
                  "composite_score" = rep(0, length(teams)))
recruiting_2016 <- rbind(recruiting_2016, tmp)
stats_2016 <- merge(stats_2016, recruiting_2016, "team")
stats_2016$stars_3[is.na(stats_2016$stars_3)] <- 0
stats_2016$stars_4[is.na(stats_2016$stars_4)] <- 0
stats_2016$stars_5[is.na(stats_2016$stars_5)] <- 0

recruiting_2017 <- filter(recruiting, year == 2017) %>% select(-conf, - year, -conf_rank)
recruiting_2017$team <- 
  sapply(recruiting_2017$team, function(x) { name_dict$NCAA[name_dict$name_247 == x] })

teams <- stats_2017$team[! stats_2017$team %in% recruiting_2017$team]
tmp <- data.frame("team" = teams,
                  "stars_5" = rep(0, length(teams)),
                  "stars_4" = rep(0, length(teams)),
                  "stars_3" = rep(0, length(teams)),
                  "num_star_players" = rep(0, length(teams)),
                  "avg_recruit_score" = rep(0, length(teams)),
                  "composite_score" = rep(0, length(teams)))
recruiting_2017 <- rbind(recruiting_2017, tmp)
recruiting_2017 <- filter(recruiting_2017, !duplicated(team))

stats_2017 <- merge(stats_2017, recruiting_2017, "team")
stats_2017$stars_3[is.na(stats_2017$stars_3)] <- 0
stats_2017$stars_4[is.na(stats_2017$stars_4)] <- 0
stats_2017$stars_5[is.na(stats_2017$stars_5)] <- 0


recruiting_2018 <- filter(recruiting, year == 2018) %>% select(-conf, - year, -conf_rank)
recruiting_2018$team <- 
  sapply(recruiting_2018$team, function(x) { name_dict$NCAA[name_dict$name_247 == x] })

teams <- stats_2018$team[! stats_2018$team %in% recruiting_2018$team]
tmp <- data.frame("team" = teams,
                  "stars_5" = rep(0, length(teams)),
                  "stars_4" = rep(0, length(teams)),
                  "stars_3" = rep(0, length(teams)),
                  "num_star_players" = rep(0, length(teams)),
                  "avg_recruit_score" = rep(0, length(teams)),
                  "composite_score" = rep(0, length(teams)))
recruiting_2018 <- rbind(recruiting_2018, tmp)
recruiting_2018 <- filter(recruiting_2018, !duplicated(team))

stats_2018 <- merge(stats_2018, recruiting_2018, "team")
stats_2018$stars_3[is.na(stats_2018$stars_3)] <- 0
stats_2018$stars_4[is.na(stats_2018$stars_4)] <- 0
stats_2018$stars_5[is.na(stats_2018$stars_5)] <- 0

stats_2016$def_coeff <- -stats_2016$def_coeff
stats_2017$def_coeff <- -stats_2017$def_coeff
stats_2018$def_coeff <- -stats_2018$def_coeff

write.csv(stats_2016, "Info/stats_2016.csv", row.names = F)
write.csv(stats_2017, "Info/stats_2017.csv", row.names = F)
write.csv(stats_2018, "Info/stats_2018.csv", row.names = F)


##################################
library(dplyr)
library(ggplot2)
stats_2016 <- read.csv("Info/stats_2016.csv", as.is = T)
stats_2017 <- read.csv("Info/stats_2017.csv", as.is = T)
stats_2018 <- read.csv("Info/stats_2018.csv", as.is = T)


stats_2016$off_coeff_next <- stats_2017$off_coeff
stats_2016$def_coeff_next <- stats_2017$def_coeff
stats_2016$yusag_coeff_next <- stats_2017$yusag_coeff

index <- stats_2018$team %in% stats_2017$team 
stats_2017$off_coeff_next <- stats_2018$off_coeff[index]
stats_2017$def_coeff_next <- stats_2018$def_coeff[index]
stats_2017$yusag_coeff_next <- stats_2018$yusag_coeff[index]

x <- rbind(mutate(stats_2016, year = 2016), mutate(stats_2017, year = 2017))
confs <- read.csv("Info/conferences.csv", as.is = T)
x <- merge(x, confs[,1:2], "team")

### Offensive Prediction
error <- rep(NA, nrow(x))
for(i in 1:nrow(x)) {
  train <- x[-i,]
  test <- x[i,]
  
  lm.prior <- lm(off_coeff_next ~ off_coeff + composite_score +
                   departing_ppg + returning_ppg +
                   (stars_5 > 0) + (stars_5 > 2)   + (num_star_players > 2),
                 data = train)
  summary(lm.prior)
  error[i] <- abs(predict(lm.prior, newdata = test) - test$off_coeff_next)
}
mean(error)

### Defensive Prediction
error <- rep(NA, nrow(x))
for(i in 1:nrow(x)) {
  train <- x[-i,]
  test <- x[i,]
  
  lm.prior <- lm(def_coeff_next ~ def_coeff  + composite_score  + 
                   departing_ppg + returning_ppg + 
                   (stars_5 > 2), 
                 data = train)
  summary(lm.prior)
  error[i] <- abs(predict(lm.prior, newdata = test) - test$off_coeff_next)
}
mean(error)


lm.prior.o <- lm(off_coeff_next ~ off_coeff + composite_score +
                   departing_ppg + returning_ppg  + 
                   (stars_5 > 0) + (stars_5 > 2)  + (num_star_players > 2),
                 data = x)

lm.prior.d <- lm(def_coeff_next ~ def_coeff  + composite_score +
                   departing_ppg + returning_ppg + (stars_5 > 2),
                 data = x)

stats_2018$off_coeff_next <- predict(lm.prior.o, newdata = stats_2018)
stats_2018$def_coeff_next <- predict(lm.prior.d, newdata = stats_2018)
stats_2018$yusag_coeff_next <- stats_2018$off_coeff_next + stats_2018$def_coeff_next

url <- "http://www.barttorvik.com"
library(XML)
y <- as.data.frame(readHTMLTable(url, header = F)[[1]], stringsAsFactors = F)
names(y) <- c("rank", "team", "conf", "rec", "adjoe", "adjde", "barthag",
              "proj_rec", "proj_rec_conf", "mins", "rpms", "sos")
y <- select(y, team, adjoe, adjde, barthag)
y$team <- gsub(" Bid %.*", "", y$team)
y$team <- sapply(y$team, function(x) {name_dict$NCAA[name_dict$Trank == x]})
y$adjoe <- as.numeric(as.character(y$adjoe))
y$adjde <- as.numeric(as.character(y$adjde))
y$barthag <- as.numeric(as.character(y$barthag))
y <- filter(y, !is.na(barthag))
y <- merge(y, select(stats_2018, team, yusag_coeff_next, 
                  off_coeff_next, 
                  def_coeff_next), "team")

lm.d <- lm(def_coeff_next ~ adjde, data = y)
lm.o <- lm(off_coeff_next ~ adjoe, data = y)
stats_2018$off_coeff_next[is.na(stats_2018$off_coeff_next)] <- 
  predict(lm.o, newdata = y[is.na(y$off_coeff_next),])
stats_2018$def_coeff_next[is.na(stats_2018$def_coeff_next)] <- 
  predict(lm.d, newdata = y[is.na(y$def_coeff_next),])
stats_2018$yusag_coeff_next <- stats_2018$off_coeff_next + 
  stats_2018$def_coeff_next

prior <- select(stats_2018, team, "off_coeff" = off_coeff_next,
                "def_coeff" = def_coeff_next, "yusag_coeff" = yusag_coeff_next)
z <- predict(lm.prior.o, newdata = stats_2018, interval = "confidence")
prior$off_sd <- (z[,3] - z[,2])/4
z <- predict(lm.prior.d, newdata = stats_2018, interval = "confidence")
prior$def_sd <- (z[,3] - z[,2])/4
z <- predict(lm.o, newdata = y[is.na(y$off_coeff_next),], 
             interval = "confidence")
prior$off_sd[is.na(prior$off_sd)] <- (z[,3] - z[,2])/4
z <- predict(lm.d, newdata = y[is.na(y$def_coeff_next),], 
             interval = "confidence")
prior$def_sd[is.na(prior$def_sd)] <- (z[,3] - z[,2])/4


prior$yusag_sd <- sqrt(prior$off_sd^2 + prior$def_sd^2)
prior <- merge(prior, select(stats_2018, -off_coeff_next, -def_coeff_next,
                                        -yusag_coeff_next, -off_coeff,
                                        -def_coeff, -yusag_coeff),
                             "team")

write.csv(prior, "prior.csv", row.names = F)