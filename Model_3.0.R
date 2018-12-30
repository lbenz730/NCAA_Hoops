#############################  Read CSVs #######################################
library(dplyr)
x <- read.csv("3.0_Files/Results/2018-19/NCAA_Hoops_Results_12_30_2018.csv", as.is = T)
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
         score_diff, total_score, game_id, opp_game_id, team_conf, opp_conf,
         year, month, day, season_id, D1, OT) %>% 
  filter(D1 == 2)


teams <- sort(unique(x$team))

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
  w_team <- 1 - (max(c(0, x$game_id[x$team == x$team[i] & !is.na(x$score_diff)])) - x$game_id[i])/
    max(c(0, x$game_id[x$team == x$team[i] & !is.na(x$score_diff)]))
  w_opponent <- 1 - (max(c(0, x$game_id[x$team == x$opponent[i] & !is.na(x$score_diff)])) - x$opp_game_id[i])/
    max(c(1, x$game_id[x$team == x$opponent[i] & !is.na(x$score_diff)]))
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

################################ Power Rankings ################################
power_rankings <- pr_compute(by_conf = F)
history <- read.csv("3.0_Files/History/history.csv", as.is = T)
x <- 
  mutate(x, pr_date = sapply(x$date, max_date, hist_dates = unique(history$date))) %>%
  inner_join(select(history, team, yusag_coeff, rank, date, off_coeff, def_coeff), 
             by = c("team" = "team","pr_date" = "date")) %>%
  inner_join(select(history, team, yusag_coeff, rank, date, off_coeff, def_coeff), 
             by = c("opponent" = "team","pr_date" = "date")) %>% 
  rename(rank = rank.x, opp_rank = rank.y, 
         yusag_coeff = yusag_coeff.x, opp_yusag_coeff = yusag_coeff.y,
         off_coeff = off_coeff.x, opp_off_coeff = off_coeff.y,
         def_coeff = def_coeff.x, opp_def_coeff = def_coeff.y) %>%
  select(-pr_date)

############################### Predictions ####################################
x <- 
  mutate(x, "hca" = case_when(location == "H" ~ lm.hoops$coefficients[1],
                              location == "V" ~ lm.hoops$coefficients[706],
                              location == "N" ~ 0),
         "hca_off" = case_when(location == "H" ~ abs(lm.off$coefficients[706]),
                               location == "V" ~ lm.off$coefficients[707] - lm.off$coefficients[706],
                               location == "N" ~ 0),
         "hca_def" = case_when(location == "H" ~ -lm.def$coefficients[706],
                               location == "V" ~ lm.def$coefficients[707] - lm.def$coefficients[706],
                               location == "N" ~ 0)) %>%
  mutate("pred_score_diff" = round(yusag_coeff - opp_yusag_coeff + hca, 1),
         "pred_team_score" = round(71 + off_coeff - opp_def_coeff + hca_off, 1),
         "pred_opp_score" = round(71 -def_coeff + opp_off_coeff + hca_def, 1),
         "pred_total_score" = pred_team_score + pred_opp_score) %>%
  select(-hca, -hca_off, -hca_def)

######################## Point Spread to Win Percentage Model #################
x$wins[x$score_diff > 0] <- 1
x$wins[x$score_diff < 0] <- 0
glm.pointspread <- glm(wins ~ pred_score_diff, 
                       data = bind_rows(select(train, wins, pred_score_diff),
                                        select(x, wins, pred_score_diff)), 
                       family=binomial) 
x$wins[is.na(x$wins)] <- 
  round(predict(glm.pointspread, newdata = x[is.na(x$wins),], type = "response"), 3)
by_conf <- pr_compute(by_conf = T)

####################################### Plots ##################################
yusag_plot(power_rankings)
png("3.0_Files/Power_Rankings/boxplot.png", res = 180, width = 1275, height = 1000)
box_plot(power_rankings)
dev.off()
evo_plot()
rank_plot()

########################### Bracketology #######################################
rpi <- rpi_compute(new = T)
resumes <- get_resumes(new = T)
bracket <- make_bracket(tourney = T)
bracket_math <- make_bracket(tourney = F)

################################ Ivy Sims ######################################
playoffs <- ivy.sim(nsims = 5000)
psf_results <- psf(nsims = 1000, min_date = "2019-01-01", max_date = "2019-01-01")

############################# Conference Sims (No Tie-Breaks) ##################
conf_results <- conf_sim("Ivy", 10000)
conf_results[,2:3] <- round(conf_results[,2:3], 2)
conf_results[,4:5] <- round(conf_results[,4:5], 2)
arrange(conf_results, desc(avg_wins))

########################## Undefeated Watch ####################################
undefeated <- filter(x, !is.na(score_diff)) %>%
  group_by(team) %>%
  summarise("wins" = sum(wins), "losses" = n() - wins) %>%
  ungroup() %>%
  filter(losses == 0) %>%
  pull(team)
df <- filter(x, team %in% undefeated, is.na(score_diff)) %>%
  group_by(team) %>%
  summarise("undefeated_pct" = prod(wins) * 100,
            "expected_losses" = sprintf("%.2f", sum(1-wins))) %>%
  arrange(desc(undefeated_pct)) 
df$undefeated_pct <- 
  case_when(
    df$undefeated_pct > 1 ~ paste0(round(df$undefeated_pct, 1), "%"),
    df$undefeated_pct > 0.01 ~ paste0(round(df$undefeated_pct, 2), "%"),
    T ~ "< 0.01%"
  )
as.data.frame(df)


############################ System Evaluation #################################
min_date <- as.Date("2018-12-01")
max_date <- Sys.Date()
y <- filter(x, date >= min_date, date <= max_date)
cat(paste("System Evaluation:", min_date, "Through", max_date),
    "\n-------------------------------------------------------------\n",
    "Predictive Accuracy: ",
    round(100 * mean(sign(y$pred_score_diff) == sign(y$score_diff), na.rm = T), 1), "%\n", 
    "Mean Absolute Error in Predicted Score Differential: ",
    round(mean(abs(y$pred_score_diff - y$score_diff), na.rm = T), 2), "\n",
    "Games w/in 2 Points of Observed Score Differential: ",
    round(100 * mean(abs(y$pred_score_diff - y$score_diff) <= 2, na.rm = T), 2), "%\n",
    "Games w/in 5 Points of Observed Score Differential: ",
    round(100 * mean(abs(y$pred_score_diff - y$score_diff) <= 5, na.rm = T), 2), "%\n",
    "Predicted Totals Score <= Total Score: ",
    round(100 * mean(y$pred_total_score <= y$total_score, na.rm = T), 1), "%\n",
    "Predicted Totals Score > Total Score: ",
    round(100 * mean(y$pred_total_score > y$total_score, na.rm = T), 1), "%\n",
    "Mean Absolute Error in Total Score Predictions: ",
    round(mean(abs(y$pred_total_score - y$total_score), na.rm = T),  2), "\n",
    "Games w/in 2 Points of Observed Total Score: ",
    round(100 * mean(abs(y$pred_total_score - y$total_score) <= 2, na.rm = T), 2), "%\n",
    "Games w/in 5 Points of Observed Total Score: ",
    round(100 * mean(abs(y$pred_total_score - y$total_score) <= 5, na.rm = T), 2), "%\n",
    sep = "")
