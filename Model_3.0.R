#############################  Read CSVs #######################################
library(dplyr)
x <- read.csv("3.0_Files/Results/2018-19/NCAA_Hoops_Results_10_31_2018.csv", as.is = T)
confs <- read.csv("3.0_Files/Info/conferences.csv", as.is = T)
deadlines <- read.csv("3.0_Files/Info/deadlines.csv", as.is = T)
priors <- read.csv("3.0_Files/Info/prior.csv", as.is = T)
source("3.0_Files/powerrankings.R")
source("3.0_Files/Ivy_Sims.R")
source("3.0_Files/rpi.R")
source("3.0_Files/record_evaluator.R")
source("3.0_Files/bracketology.R")
source("3.0_Files/helpers.R")
source("3.0_Files/tourney_sim.R")
source("3.0_Files/ncaa_sims.R")
########################  Data Cleaning ########################################
x <- x %>%
  rename(team_score = teamscore, opp_score = oppscore) %>%
  mutate(date = as.Date(paste(year, month, day, sep = "-")),
         score_diff = team_score - opp_score,
         total_score = team_score + opp_score,
         season_id = "2018-19", game_id = NA, opp_game_id = NA, 
         team_conf = NA, opp_conf = NA, conf_game = NA) %>%
  select(date, team, opponent, location, team_score, opp_score,
         score_diff, game_id, opp_game_id, team_conf, opp_conf,
         year, month, day, season_id, D1, OT) %>% 
  filter(D1 == 2)
  

teams <- unique(x$team)

### Game IDs
for(i in 1:length(teams)) {
  x[x$team == teams[i],] <- x %>%
    filter(team == teams[i]) %>%
    mutate(game_id = seq(1, sum(team == teams[i]), 1))
}

### Opp Game IDs
for(i in 1:nrow(x)) {
  x$opp_game_id[i] <- get_opp_id(x, i)[1]
}

### Confs
for(i in 1:length(teams)) {
  x$team_conf[x$team == teams[i]] <- get_conf(teams[i])
  x$opp_conf[x$opponent == teams[i]] <- get_conf(teams[i])
}
x$conf_game <- x$team_conf == x$opp_conf

### Reg Season
for(i in 1:nrow(x)) {
  x$reg_season[i] <- reg_season(x$month[i], x$day[i], x$team_conf[i])
}

################################# Set Weights ##################################
x$weights <- 0
for(i in 1:nrow(x)) {
  w_team <- 1 - (max(c(0, x$game_id[x$team == x$team[i] & !is.na(x$scorediff)])) - x$game_id[i])/
    max(c(0, x$game_id[x$team == x$team[i] & !is.na(x$scorediff)]))
  w_opponent <- 1 - (max(c(0, x$game_id[x$team == x$opponent[i] & !is.na(x$scorediff)])) - x$opp_game_id[i])/
    max(c(1, x$game_id[x$team == x$opponent[i] & !is.na(x$scorediff)]))
  rr <- mean(c(w_team, w_opponent))
  x$weights[i] <- 1/(1 + (0.5^(5 * rr)) * exp(-rr))
}   

############################### Create Models ##################################
### Current Season
lm.hoops <- lm(score_diff ~ team + opponent + location, weights = weights, data = x) 
lm.off <- lm(team_score ~ team + opponent + location, weights = weights, data = x) 
lm.def <- lm(opp_score ~ team + opponent + location, weights = weights, data = x) 

### Update With Pre-Season Priors
priors <- mutate(priors, 
                 rel_yusag_coeff = yusag_coeff - yusag_coeff[1],
                 rel_off_coeff = off_coeff - off_coeff[1],
                 rel_def_coeff = def_coeff - def_coeff[1])

w <- sapply(teams, prior_weight)

lm.hoops$coefficients[2:353] <- 
  lm.hoops$coefficients[2:353] * w[-1] + priors$yusag_coeff[-1] * (1-w[-1])
lm.hoops$coefficients[354:705] <- 
  lm.hoops$coefficients[354:705] * w[-1] - priors$yusag_coeff[-1] * (1-w[-1])
lm.off$coefficients[2:353] <- 
  lm.off$coefficients[2:353] * w[-1] + priors$off_coeff[-1] * (1-w[-1])
lm.off$coefficients[354:705] <- 
  lm.off$coefficients[354:705] * w[-1] - priors$off_coeff[-1] * (1-w[-1])
lm.def$coefficients[2:353] <- 
  lm.def$coefficients[2:353] * w[-1] + priors$def_coeff[-1] * (1-w[-1])
lm.def$coefficients[354:705] <- 
  lm.def$coefficients[354:705] * w[-1] - priors$def_coeff[-1] * (1-w[-1])

######################## Point Spread to Win Percentage Model #################
y$predscorediff <- round(predict(lm.hoops, newdata = y), 1)
y$wins[y$score_diff > 0] <- 1
y$wins[y$score_diff < 0] <- 0
glm.pointspread <- glm(wins ~ predscorediff, data = y, family=binomial) 
summary(glm.pointspread)
y$wins[is.na(y$wins)] <- 
  round(predict(glm.pointspread, newdata = y[is.na(y$wins),], type = "response"), 3)

y$date <- as.Date(paste(y$year, y$month, y$day, sep = "/"))
curdate <- as.Date("2018/3/25")
y$dist <- y$date - curdate
preds <- read.csv("predictions.csv")
preds$prediction_made_date <- as.Date(preds$prediction_made_date)
preds$date <- as.Date(preds$date)
z <- filter(y, dist >= 0, dist <= 3)
z$prediction_made_date <- curdate
write.csv(rbind(preds,z), "predictions.csv", row.names = F)
################################ Power Rankings ################################
powranks <- pr_compute(by_conf = F)
by_conf <- pr_compute(by_conf = T)
yusag_plot(powranks)

########################### NCAA Sims ##############################
ncaa_sims <- ncaa_sim(nsims = 10000)
write.csv(ncaa_sims, "2.0_Files/Predictions/ncaa_sims.csv", row.names = F)

########################### Bracketology #######################################
rpi <- rpi_compute(new = F)
resumes <- get_resumes(new = F)
bracket <- make_bracket(tourney = T)
bracket_math <- make_bracket(tourney = F)

################################ Ivy Sims ######################################
playoffs <- ivy.sim(nsims = 5000)
psf_results <- psf(nsims = 1000, year = 2018, months = c(3,3), days = c(2,3))

######################### Conf Undefeated Watch ################################
confs <- confs[order(confs$team), ]
confs$conf_loss <- aggregate(1-wins ~ team, data = filter(y, conf_game, !is.na(scorediff)), sum)[,2]
undefeateds <- filter(confs, conf_loss == 0)
undefeateds$prob <- NA
for(i in 1:nrow(undefeateds)) {
  undefeateds$prob[i] <- prod(y$wins[is.na(y$scorediff) & y$conf_game & y$team == undefeateds$team[i]])
}
write.csv(undefeateds[,c(1,2,7)], "2.0_Files/Predictions/undeafeateds.csv", row.names = F)

############################# Conference Sims (No Tie-Breaks) ##################
conf_results <- conf_sim("Ivy", 10000)

#### Select Games
filter(y, month == 2, day %in% 9:10, conf_game, team_conf == "Ivy", location == "H")

par(mfrow = c(2,4))
colors <- c("brown", "skyblue", "red", "forestgreen", "firebrick4", "maroon", "orange", "navy")
for(i in 1:8) {
  hist(simresults[,i], xlab = "Conference Wins", col = colors[i], main = names(simresults)[i],
       xlim = c(0, 14))
}
######################### Mess Around w/ Ivy Sims ##############################
simresults <- fast.sim(nsims = 20000)
write.csv(simresults, "2.0_Files/Predictions/mens_simresults.csv", row.names = F)

winmat <- apply(simresults, 1, sort, decreasing = T)
table(winmat[4,] == winmat[5,])/20000
table(winmat[3,] == winmat[6,])/20000
apply(winmat, 1, mean)
table(winmat[4,])/20000
table(simresults$Yale)/20000
apply(simresults, 2, mean)

champs <- c("Lipscomb", "Radford", "Michigan", "Col. of Charleston", "Wright St.",
            "Iona", "Loyola Chicago", "LIU Brooklyn", "Murray St.", "Bucknell",
            "UNCG", "South Dakota St.", "Gonzaga", "UMBC", "Virginia", "Kansas",
            "New Mexico St.", "Villanova", "Arizona", "N.C. Central", "Buffalo",
            "Texas Southern", "San Diego St.", "Montana", "Marshall", "SFA",
            "Cal St. Fullerton", "Penn", "Kentucky", "Davidson")
