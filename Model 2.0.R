#############################  Read CSVs #######################################
x <- read.csv("2.0_Files/Results/2016-17/NCAA_Hoops_Results_6_29_2017.csv", as.is = T)
# y <- read.csv("2.0_Files/Results/2017-18/NCAA_Hoops_Results_1_1_2017", as.is = T)
mins <- read.csv("2.0_Files/Info/mins.csv", as.is = T)
rec <- read.csv("2.0_Files/Info/recruiting.csv", as.is = T)
transfers <- read.csv("2.0_Files/Info/transfers.csv", as.is = T)
confs <- read.csv("2.0_Files/Info/conferences.csv", as.is = T)
source("2.0_Files/powerrankings.R")
source("2.0_Files/Ivy_Sims.R")
source("2.0_Files/rpi.R")
source("2.0_Files/record_evaluator.R")
source("2.0_Files/bracketology.R")
########################  Data Cleaning ########################################
# y$scorediff <- NA
# y$predscorediff <- NA
# y$wins <- NA
# y$season_id <- "2017-18"
# y$game_id <- NA
# y$opp_game_id <- NA
# y$team_conf <- NA
# y$opp_conf <- NA
# y$conf_game <- NA

x$season_id <- "2016-17"
x$game_id <- NA
x$opp_game_id <- NA
x$team_conf <- NA
x$opp_conf <- NA
x$conf_game <- NA

teams <- unique(x$team)

### Game IDs
for(i in 1:length(teams)) {
  x$game_id[x$team == teams[i]] <- seq(1, length(x$game_id[x$team == teams[i]]), 1)
  # y$game_id[y$team == teams[i]] <- seq(1, length(y$game_id[y$team == teams[i]]), 1)
}

### Opp Game IDs
for(i in 1:nrow(x)) {
  x$opp_game_id[i] <- x$game_id[x$team == x$opponent[i] & x$opponent == x$team[i] & 
              x$month == x$month[i] & x$day == x$day[i]]
}
# for(i in 1:nrow(y)) {
#   y$opp_game_id[i] <- y$game_id[y$team == y$opponent[i] & y$opponent == y$team[i] & 
#                                   y$month == y$month[i] & y$day == y$day[i]]
# }

### Confs
for(i in 1:length(teams)) {
  x$team_conf[x$team == teams[i]] <- confs$conference[confs$team == teams[i]]
  x$opp_conf[x$opponent == teams[i]] <- confs$conference[confs$team == teams[i]]
#   y$team_conf[y$team == teams[i]] <- confs$conference[confs$team == teams[i]]
#   y$opp_conf[y$opponent == teams[i]] <- confs$conference[confs$team == teams[i]]
  }
x$conf_game <- x$team_conf == x$opp_conf
# y$conf_game <- y$team_conf == y$opp_conf
x$reg_season <- (x$month < 3 | x$month >= 11) | (x$month == 3 & x$day <= 4)
# y$reg_season <- (y$month < 3 | y$month >= 11) | (y$month == 3 & y$day <= 4)
################################# Set Weights ##################################
x$weights <- 0
for(i in 1:nrow(x)) {
  #   tmp1 <- (max(y$game_id[y$team == y$team[i]]) - 2 * max(y$game_id[y$team == y$team[i] & !is.na(y$scorediff)]))/max(y$game_id[y$team == y$team[i]])
  #   tmp2 <- (max(y$game_id[y$opponent == y$opponent[i]])- 2 * max(y$game_id[y$opponent == y$opponent[i] & !is.na(y$scorediff)]))/max(y$game_id[y$opponent == y$opponent[i]])
  x$weights[i] <- min(0.5, max(0, 0.5 * (mins$mins[mins$team == x$team[i]] #* tmp1 
                                         + mins$mins[mins$team == x$opponent[i]]))) #* tmp2)))
}
# y$weights <- 0
# for(i in 1:nrow(y)) {
#   w_team <- (max(y$game_id[y$Team == y$Team[i] & !is.na(y$scorediff)]) - y$game_id[i])/
#     max(y$game_id[y$Team == y$Team[i] & !is.na(y$scorediff)])
#   w_opponent <- (max(y$game_id[y$Team == y$Opponent[i] & !is.na(y$scorediff)]) - y$opp_game_id[i])/
#     max(y$game_id[y$Team == y$Opponent[i] & !is.na(y$scorediff)])
#   rr <- mean(c(w_team, w_opponent))
#   y$weights[i] <- 1/(1 + (0.5^(5 * rr)) * exp(-rr))
# }   

############################### Create Model ###################################
lm.hoops <- lm(scorediff ~ team + opponent + location, weights = weights, data = x) #rbind(x,y)
summary(lm.hoops)

####################### Adjust for Recruiting Class of 2017 ####################
rec$adjustment <- rec$score/sd(rec$score[rec$score > 0])
for(i in 2:(length(teams))) {
  #tmp1 <- (max(y$game_id[y$team == teams[i]]) - 2 * max(y$game_id[y$team == teams[i] & !is.na(y$scorediff)]))/max(y$game_id[y$team == team[i]])
  lm.hoops$coefficients[paste("team", teams[i], sep = "")]  <-
    lm.hoops$coefficients[paste("team", teams[i], sep = "")] +
    rec$adjustment[rec$Team == teams[i]] #*tmp1
  lm.hoops$coefficients[paste("opponent", teams[i], sep = "")] <- 
    lm.hoops$coefficients[paste("opponent", teams[i], sep = "")] -
    rec$adjustment[rec$Team == teams[i]] #*tmp1
}

########################### Adjust for Transfers ###############################
t.teams <- unique(transfers$Team)
for(i in 1:(length(t.teams))) {
  #tmp1 <- (max(y$game_id[y$team == t.teams[i]]) - 2 * max(y$game_id[y$team == t.teams[i] & !is.na(y$scorediff)]))/max(y$game_id[y$team == t.team[i]])
  lm.hoops$coefficients[paste("team", t.teams[i], sep = "")]  <-
    lm.hoops$coefficients[paste("team", t.teams[i], sep = "")] +
    sum(transfers$Adjust[transfers$Team == t.teams[i]])#*tmp1
  lm.hoops$coefficients[paste("opponent", t.teams[i], sep = "")] <- 
    lm.hoops$coefficients[paste("opponent", t.teams[i], sep = "")] -
    sum(transfers$Adjust[transfers$Team == t.teams[i]])#*tmp1
}

######################## Point Spread to Win Percentage Model #################
y$predscorediff <- round(predict(lm.hoops, newdata = y), 1)
y$wins <- NA
y$wins[y$scorediff > 0] <- 1
y$wins[y$scorediff < 0] <- 0
glm.pointspread <- glm(wins ~ predscorediff, data = x, family=binomial(link=logit)) #rbind(x,y)
summary(glm.pointspread)
y$wins[is.na(y$wins)] <- 
  round(predict.glm(glm.pointspread, newdata = y[is.na(y$wins),], type = "response"), 3)

################################ Power Rankings ################################
powranks <- pr_compute(by_conf = F)
by_conf <- pr_compute(by_conf = T)

########################### Game Excitement Index ##############################
harmonic_mean <- function(a,b) {
  return(1/(mean(1/c(a, b))))
}

compute_GEI <- function(data) {
  adj <- abs(min(powranks$YUSAG_Coefficient))
  team_strength <- powranks$YUSAG_Coefficient[powranks$Team == data$team] + adj + 1
  opp_strength <- powranks$YUSAG_Coefficient[powranks$Team == data$opponent] + adj + 1
  score <- abs(data$predscorediff)
  team_conf_rank <- by_conf$Conference_Rank[by_conf$Team == data$team]
  opp_conf_rank <- by_conf$Conference_Rank[by_conf$Team == data$opponent]
  return(harmonic_mean(team_strength, opp_strength)/(1 + abs(team_strength - opp_strength)))
}

x$GEI <- NA
for(i in 1:nrow(x)) {
  x$GEI[i] <- compute_GEI(x[i,])
}

# y$GEI <- NA
# for(i in 1:nrow(x)) {
#   y$GEI[i] <- compute_GEI(y[i,])
# }

y <- x

########################### Bracketology #######################################
rpi <- rpi_compute(new = F)
resumes <- get_resumes(new = F)
bracket <- make_bracket(tourney = T)
bracket_math <- make_bracket(tourney = F)

################################ Ivy Sims ######################################
playoffs <- ivy.sim(nsims = 5000)
psf <- psf(nsims = 1000, year = 2017, months = c(3,3), days = c(3,4))
