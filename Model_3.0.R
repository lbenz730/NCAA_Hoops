#############################  Read CSVs #######################################
library(dplyr)
x <- read.csv("3.0_Files/Results/2018-19/NCAA_Hoops_Results_11_18_2018.csv", as.is = T)
train <- read.csv("3.0_Files/Results/2017-18/training.csv", as.is = T)
confs <- read.csv("3.0_Files/Info/conferences.csv", as.is = T)
deadlines <- read.csv("3.0_Files/Info/deadlines.csv", as.is = T) %>%
  mutate(deadline = as.Date(deadline, "%m/%d/%y"))
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
x <- 
  inner_join(x, select(x, date, team, opponent, game_id), 
             by = c("team" = "opponent",
                    "opponent" = "team", 
                    "date" = "date")) %>%
  filter(!duplicated(paste(team, game_id.x))) %>%
  mutate(opp_game_id = game_id.y) %>%
  rename(game_id = game_id.x) %>%
  select(-game_id.y)

### Confs
x <- mutate(x, team_conf = sapply(x$team, get_conf),
            opp_conf = sapply(x$opponent, get_conf),
            conf_game = team_conf == opp_conf)

### Reg Season
x <- inner_join(x, deadlines, by = c("team_conf" = "conf")) %>%
  mutate(reg_season = date < deadline) %>%
  select(-deadline)

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
                 rel_def_coeff = def_coeff - def_coeff[1]) %>%
  arrange(team)

w <- sapply(teams, prior_weight)

coeffs <- c("(Intercept)", paste0("team", teams[-1]), paste0("opponent", teams[-1]), 
"locationN", "locationV")
lm.hoops$coefficients <- lm.hoops$coefficients[coeffs]
lm.def$coefficients <- lm.def$coefficients[coeffs]
lm.off$coefficients <- lm.off$coefficients[coeffs]
lm.hoops$coefficients[is.na(lm.hoops$coefficients)] <- 0
lm.def$coefficients[is.na(lm.def$coefficients)] <- 0
lm.off$coefficients[is.na(lm.off$coefficients)] <- 0

lm.hoops$coefficients[2:353] <- 
  lm.hoops$coefficients[2:353] * w[-1] + priors$rel_yusag_coeff[-1] * (1-w[-1])
lm.hoops$coefficients[354:705] <- 
  lm.hoops$coefficients[354:705] * w[-1] - priors$rel_yusag_coeff[-1] * (1-w[-1])
lm.off$coefficients[2:353] <- 
  lm.off$coefficients[2:353] * w[-1] + priors$rel_off_coeff[-1] * (1-w[-1])
lm.off$coefficients[354:705] <- 
  lm.off$coefficients[354:705] * w[-1] - priors$rel_off_coeff[-1] * (1-w[-1])
lm.def$coefficients[2:353] <- 
  lm.def$coefficients[2:353] * w[-1] - priors$rel_def_coeff[-1] * (1-w[-1])
lm.def$coefficients[354:705] <- 
  lm.def$coefficients[354:705] * w[-1] + priors$rel_def_coeff[-1] * (1-w[-1])

lm.hoops$coefficients[c(1, 706:707)] <- 
  w[1] * lm.hoops$coefficients[c(1, 706:707)] + 
  (1-w[1]) * c(3.34957772, -3.34957772, -6.69915544)
lm.off$coefficients[c(1, 706:707)] <- 
  w[1] * lm.off$coefficients[c(1, 706:707)] + 
  (1-w[1]) * c(68.19705698, -2.93450334,  -3.34957772)
lm.def$coefficients[c(1, 706:707)] <- 
  w[1] * lm.def$coefficients[c(1, 706:707)] + 
  (1-w[1]) * c(64.84747926, 0.41507438,  3.34957772)

######################## Point Spread to Win Percentage Model #################
x$pred_score_diff <- round(predict(lm.hoops, newdata = x), 1)
x$wins[x$score_diff > 0] <- 1
x$wins[x$score_diff < 0] <- 0
glm.pointspread <- glm(wins ~ pred_score_diff, 
                       data = bind_rows(select(train, wins, pred_score_diff),
                                        select(x, wins, pred_score_diff)), 
                       family=binomial) 
x$wins[is.na(x$wins)] <- 
  round(predict(glm.pointspread, newdata = x[is.na(x$wins),], type = "response"), 3)

################################ Power Rankings ################################
power_rankings <- pr_compute(by_conf = F)
by_conf <- pr_compute(by_conf = T)
history <- read.csv("3.0_Files/History/history.csv", as.is = T)
x <- 
  inner_join(x, select(power_rankings, team, yusag_coeff, rank), by = "team") %>%
  inner_join(select(power_rankings, team, yusag_coeff, rank), 
             by = c("opponent" = "team")) %>% 
  rename(rank = rank.x, opp_rank = rank.y, 
         yusag_coeff = yusag_coeff.x, opp_yusag_coeff = yusag_coeff.y)
yusag_plot(power_rankings)
png("3.0_Files/Power_Rankings/boxplot.png", res = 180, width = 1275, height = 1000)
box_plot(power_rankings)
dev.off()
evo_plot()
rank_plot()


########################### Bracketology #######################################
rpi <- rpi_compute(new = T)
resumes <- get_resumes(new = F)
bracket <- make_bracket(tourney = T)
bracket_math <- make_bracket(tourney = F)

################################ Ivy Sims ######################################
playoffs <- ivy.sim(nsims = 5000)
psf_results <- psf(nsims = 1000, year = 2018, months = c(3,3), days = c(2,3))

############################# Conference Sims (No Tie-Breaks) ##################
conf_results <- conf_sim("Ivy", 10000)