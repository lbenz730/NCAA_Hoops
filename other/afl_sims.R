# library(tidyverse)
# library(purrr)
# #############################  Read CSVs #######################################
# library(dplyr) 
# library(readr)
# library(lubridate)
# x <- read_csv(paste("3.0_Files/Results/2019-20/NCAA_Hoops_Results_3_9_2020.csv"))
# train <- read_csv("3.0_Files/Results/2017-18/training.csv")
# confs <- read_csv("3.0_Files/Info/conferences.csv")
# deadlines <- read_csv("3.0_Files/Info/deadlines.csv") %>%
#   mutate(deadline = as.Date(deadline, "%m/%d/%y"))
# priors <- read_csv("3.0_Files/Info/prior.csv")
# source("3.0_Files/powerrankings.R")
# source("3.0_Files/Ivy_Sims.R")
# source("3.0_Files/rpi.R")
# source("3.0_Files/record_evaluator.R")
# source("3.0_Files/bracketology.R")
# source("3.0_Files/helpers.R")
# source("3.0_Files/tourney_sim.R")
# source("3.0_Files/ncaa_sims.R")
# ########################  Data Cleaning ########################################
# x <- x %>%
#   rename(team_score = teamscore, opp_score = oppscore) %>%
#   mutate(date = as.Date(paste(year, month, day, sep = "-")),
#          score_diff = team_score - opp_score,
#          total_score = team_score + opp_score,
#          season_id = "2019-20", game_id = NA, opp_game_id = NA, 
#          team_conf = NA, opp_conf = NA, conf_game = NA) %>%
#   select(date, team, opponent, location, team_score, opp_score,
#          score_diff, total_score, game_id, opp_game_id, team_conf, opp_conf,
#          year, month, day, season_id, D1, OT) %>% 
#   filter(D1 == 2)
# 
# teams <- sort(unique(x$team))
# 
# ### Game IDs
# for(i in 1:length(teams)) {
#   x[x$team == teams[i],] <- x %>%
#     filter(team == teams[i]) %>%
#     mutate(game_id = seq(1, sum(team == teams[i]), 1))
# }
# 
# ### Opp Game IDs
# x <- 
#   inner_join(x, select(x, date, team, opponent, game_id), 
#              by = c("team" = "opponent",
#                     "opponent" = "team", 
#                     "date" = "date")) %>%
#   filter(!duplicated(paste(team, game_id.x))) %>%
#   mutate(opp_game_id = game_id.y) %>%
#   rename(game_id = game_id.x) %>%
#   select(-game_id.y)
# 
# ### Confs
# x <- mutate(x, team_conf = sapply(x$team, get_conf),
#             opp_conf = sapply(x$opponent, get_conf),
#             conf_game = team_conf == opp_conf)
# x$conf_game[x$team %in%  c("Arizona St.", "Colorado") & x$date == "2019-11-08"] <- F
# 
# ### Reg Season
# x <- inner_join(x, deadlines, by = c("team_conf" = "conf")) %>%
#   mutate(reg_season = date < deadline) %>%
#   select(-deadline)
# 
# ### Eliminate Teams from Auto Bid contention
# confs <- eliminate(filter(x, score_diff < 0, !reg_season) %>% pull(team), confs)
# 
# ################################# Set Weights ##################################
# x$weights <- 0
# for(i in 1:nrow(x)) {
#   w_team <- 1 - (max(c(0, x$game_id[x$team == x$team[i] & !is.na(x$score_diff)])) - x$game_id[i])/
#     max(c(0, x$game_id[x$team == x$team[i] & !is.na(x$score_diff)]))
#   w_opponent <- 1 - (max(c(0, x$game_id[x$team == x$opponent[i] & !is.na(x$score_diff)])) - x$opp_game_id[i])/
#     max(c(1, x$game_id[x$team == x$opponent[i] & !is.na(x$score_diff)]))
#   rr <- mean(c(w_team, w_opponent))
#   x$weights[i] <- 1/(1 + (0.5^(5 * rr)) * exp(-rr))
# }   
# 
# ############################### Create Models ##################################
# ### Current Season
# lm.hoops <- lm(score_diff ~ team + opponent + location, weights = weights, data = x) 
# lm.off <- lm(team_score ~ team + opponent + location, weights = weights, data = x) 
# lm.def <- lm(opp_score ~ team + opponent + location, weights = weights, data = x) 
# 
# ### Update With Pre-Season Priors
# priors <- mutate(priors, 
#                  rel_yusag_coeff = yusag_coeff - yusag_coeff[1],
#                  rel_off_coeff = off_coeff - off_coeff[1],
#                  rel_def_coeff = def_coeff - def_coeff[1]) %>%
#   arrange(team)
# 
# w <- sapply(teams, prior_weight)
# 
# lm.hoops$coefficients[2:353] <- 
#   lm.hoops$coefficients[2:353] * w[-1] + priors$rel_yusag_coeff[-1] * (1-w[-1])
# lm.hoops$coefficients[354:705] <- 
#   lm.hoops$coefficients[354:705] * w[-1] - priors$rel_yusag_coeff[-1] * (1-w[-1])
# lm.off$coefficients[2:353] <- 
#   lm.off$coefficients[2:353] * w[-1] + priors$rel_off_coeff[-1] * (1-w[-1])
# lm.off$coefficients[354:705] <- 
#   lm.off$coefficients[354:705] * w[-1] - priors$rel_off_coeff[-1] * (1-w[-1])
# lm.def$coefficients[2:353] <- 
#   lm.def$coefficients[2:353] * w[-1] - priors$rel_def_coeff[-1] * (1-w[-1])
# lm.def$coefficients[354:705] <- 
#   lm.def$coefficients[354:705] * w[-1] + priors$rel_def_coeff[-1] * (1-w[-1])
# 
# lm.hoops$coefficients[c(1, 706:707)] <- 
#   w[1] * lm.hoops$coefficients[c(1, 706:707)] + 
#   (1-w[1]) * c(3.34957772, -3.34957772, -6.69915544)
# lm.off$coefficients[c(1, 706:707)] <- 
#   w[1] * lm.off$coefficients[c(1, 706:707)] + 
#   (1-w[1]) * c(68.19705698, -2.93450334,  -3.34957772)
# lm.def$coefficients[c(1, 706:707)] <- 
#   w[1] * lm.def$coefficients[c(1, 706:707)] + 
#   (1-w[1]) * c(64.84747926, 0.41507438,  3.34957772)
# 
# ################################ Power Rankings ################################
# power_rankings <- pr_compute(by_conf = F)
# history <- read.csv("3.0_Files/History/history.csv", as.is = T)
# x <- 
#   mutate(x, pr_date = sapply(x$date, max_date, hist_dates = unique(history$date))) %>%
#   inner_join(select(history, team, yusag_coeff, rank, date, off_coeff, def_coeff), 
#              by = c("team" = "team","pr_date" = "date")) %>%
#   inner_join(select(history, team, yusag_coeff, rank, date, off_coeff, def_coeff), 
#              by = c("opponent" = "team","pr_date" = "date")) %>% 
#   rename(rank = rank.x, opp_rank = rank.y, 
#          yusag_coeff = yusag_coeff.x, opp_yusag_coeff = yusag_coeff.y,
#          off_coeff = off_coeff.x, opp_off_coeff = off_coeff.y,
#          def_coeff = def_coeff.x, opp_def_coeff = def_coeff.y) %>%
#   select(-pr_date)
# 
# ############################### Predictions ####################################
# x <- 
#   mutate(x, "hca" = case_when(location == "H" ~ lm.hoops$coefficients[1],
#                               location == "V" ~ lm.hoops$coefficients[706],
#                               location == "N" ~ 0),
#          "hca_off" = case_when(location == "H" ~ abs(lm.off$coefficients[706]),
#                                location == "V" ~ lm.off$coefficients[707] - lm.off$coefficients[706],
#                                location == "N" ~ 0),
#          "hca_def" = case_when(location == "H" ~ -lm.def$coefficients[706],
#                                location == "V" ~ lm.def$coefficients[707] - lm.def$coefficients[706],
#                                location == "N" ~ 0)) %>%
#   mutate("pred_score_diff" = round(yusag_coeff - opp_yusag_coeff + hca, 1),
#          "pred_team_score" = round(70 + off_coeff - opp_def_coeff + hca_off, 1),
#          "pred_opp_score" = round(70 -def_coeff + opp_off_coeff + hca_def, 1),
#          "pred_total_score" = pred_team_score + pred_opp_score) %>%
#   select(-hca, -hca_off, -hca_def)
# 
# ######################## Point Spread to Win Percentage Model #################
# x$wins[x$score_diff > 0] <- 1
# x$wins[x$score_diff < 0] <- 0
# glm.pointspread <- glm(wins ~ pred_score_diff, 
#                        data = bind_rows(select(train, wins, pred_score_diff),
#                                         select(x, wins, pred_score_diff)), 
#                        family=binomial) 
# saveRDS(glm.pointspread, file = "glm_pointspread.rds")
# x$wins[is.na(x$wins)] <- 
#   round(predict(glm.pointspread, newdata = x[is.na(x$wins),], type = "response"), 3)
# by_conf <- pr_compute(by_conf = T)
# write_csv(x, "3.0_Files/Predictions/predictions.csv")
# 
# 
# ##############################################################################################
# standard_sim <- function(conf, nsims, loc_type) {
#   standings <- filter(x, team_conf == conf, conf_game) %>%
#     group_by(team) %>%
#     summarise("n_win" = sum(wins),
#               "n_loss" = n() - sum(wins)) %>%
#     inner_join(select(power_rankings, team, yusag_coeff), by = "team") %>%
#     arrange(desc(n_win), desc(yusag_coeff)) %>%
#     slice(1:8) %>%
#     mutate("seed" = 1:8)
#   standings$conference <- conf
#   standings$finals <- 0
#   standings$champ <- 0
#   standings$loc_type <- loc_type
#   
#   ### Pre-Compute Spread and Win Prob for first round
#   df_sim <- tibble("team" = c(standings$team[1:4], rep(NA, 3)),
#                    "opponent" = c(standings$team[8:5], rep(NA, 3)),
#                    "location" = loc_type)
#   df_sim$pred_score_diff <- predict(lm.hoops, newdata = df_sim)
#   df_sim$win_prob <- predict(glm.pointspread, newdata = df_sim, type = "response")
#   
#   for(i in 1:nsims) {
#     ### Simulate First Round
#     df_sim <- mutate(df_sim, "winner" = ifelse(runif(7) <= win_prob, team, opponent))
#     if(df_sim$team[1] == df_sim$winner[1]) {
#       df_sim$team[5] <- df_sim$winner[1]
#       df_sim$opponent[5] <- df_sim$winner[4]
#     } else {
#       df_sim$team[5] <- df_sim$winner[4]
#       df_sim$opponent[5] <- df_sim$winner[1]
#     }
#     
#     if(df_sim$team[2] == df_sim$winner[2]) {
#       df_sim$team[6] <- df_sim$winner[2]
#       df_sim$opponent[6] <- df_sim$winner[3]
#     } else {
#       df_sim$team[6] <- df_sim$winner[3]
#       df_sim$opponent[6] <- df_sim$winner[2]
#     }
#     
#     ### Simulate 2nd Round
#     df_sim$pred_score_diff[5:6] <- predict(lm.hoops, newdata = df_sim[5:6,])
#     df_sim$win_prob[5:6] <- predict(glm.pointspread, newdata = df_sim[5:6,], type = "response")
#     df_sim[5:6,] <- mutate(df_sim[5:6,], "winner" = ifelse(runif(2) <= win_prob, team, opponent))
#     
#     if(standings$seed[standings$team == df_sim$winner[5]] < standings$seed[standings$team == df_sim$winner[6]]) {
#       df_sim$team[7] <- df_sim$winner[5]
#       df_sim$opponent[7] <- df_sim$winner[6]
#     } else {
#       df_sim$team[7] <- df_sim$winner[6]
#       df_sim$opponent[7] <- df_sim$winner[5]
#     } 
#     
#     ### Simulate Finals
#     df_sim$pred_score_diff[7] <- predict(lm.hoops, newdata = df_sim[7,])
#     df_sim$win_prob[7] <- predict(glm.pointspread, newdata = df_sim[7,], type = "response")
#     df_sim[7,] <- mutate(df_sim[7,], "winner" = ifelse(runif(1) <= win_prob, team, opponent))
#     standings$finals[standings$team %in% c(df_sim$team[7], df_sim$opponent[7])] <- 
#       standings$finals[standings$team %in% c(df_sim$team[7], df_sim$opponent[7])] + 1/nsims
#     standings$champ[standings$team == df_sim$winner[7]] <- 
#       standings$champ[standings$team == df_sim$winner[7]] + 1/nsims
#   }
#   return(standings)
# }
# 
# 
# afl_sim <- function(conf, nsims) {
#   standings <- filter(x, team_conf == conf, conf_game) %>%
#     group_by(team) %>%
#     summarise("n_win" = sum(wins),
#               "n_loss" = n() - sum(wins)) %>%
#     inner_join(select(power_rankings, team, yusag_coeff), by = "team") %>%
#     arrange(desc(n_win), desc(yusag_coeff)) %>%
#     slice(1:8) %>%
#     mutate("seed" = 1:8)
#   standings$conference <- conf
#   standings$finals <- 0
#   standings$champ <- 0
#   standings$loc_type <- "AFL"
#   
#   ### Pre-Compute Spread and Win Prob for first round
#   df_sim <- tibble("team" = c(standings$team[c(1:2, 5,6)], rep(NA, 5)),
#                    "opponent" = c(standings$team[c(4,3,8,7)], rep(NA, 5)),
#                    "location" = c(rep("H", 8), "N"))
#   df_sim$pred_score_diff <- predict(lm.hoops, newdata = df_sim)
#   df_sim$win_prob <- predict(glm.pointspread, newdata = df_sim, type = "response")
#   
#   
#   for(i in 1:nsims) {
#     ### Simulate First Round
#     df_sim <- mutate(df_sim, "winner" = ifelse(runif(9) <= win_prob, team, opponent))
#     if(df_sim$team[1] == df_sim$winner[1]) {
#       df_sim$team[7] <- df_sim$team[1]
#       df_sim$team[5] <- df_sim$opponent[1]
#       
#     } else {
#       df_sim$team[5] <- df_sim$team[1]
#       df_sim$team[7] <- df_sim$opponent[1]
#     }
#     
#     if(df_sim$team[2] == df_sim$winner[2]) {
#       df_sim$team[8] <- df_sim$team[2]
#       df_sim$team[6] <- df_sim$opponent[2]
#       
#     } else {
#       df_sim$team[6] <- df_sim$team[2]
#       df_sim$team[8] <- df_sim$opponent[2]
#     }
#     
#     df_sim$opponent[6] <- df_sim$winner[3]
#     df_sim$opponent[5] <- df_sim$winner[4]
#     
#     df_sim$pred_score_diff[5:6] <- predict(lm.hoops, newdata = df_sim[5:6,])
#     df_sim$win_prob[5:6] <- predict(glm.pointspread, newdata = df_sim[5:6,], type = "response")
#     df_sim[5:6,] <- mutate(df_sim[5:6,], "winner" = ifelse(runif(2) <= win_prob, team, opponent))
#     
#     df_sim$opponent[7] <- df_sim$winner[6]
#     df_sim$opponent[8] <- df_sim$winner[5]
#     
#     df_sim$pred_score_diff[7:8] <- predict(lm.hoops, newdata = df_sim[7:8,])
#     df_sim$win_prob[7:8] <- predict(glm.pointspread, newdata = df_sim[7:8,], type = "response")
#     df_sim[7:8,] <- mutate(df_sim[7:8,], "winner" = ifelse(runif(2) <= win_prob, team, opponent))
#     
#     ### Simulate Finals
#     df_sim$team[9] <- df_sim$winner[7]
#     df_sim$opponent[9] <- df_sim$winner[8]
#     df_sim$pred_score_diff[9] <- predict(lm.hoops, newdata = df_sim[9,])
#     df_sim$win_prob[9] <- predict(glm.pointspread, newdata = df_sim[9,], type = "response")
#     df_sim[9,] <- mutate(df_sim[9,], "winner" = ifelse(runif(1) <= win_prob, team, opponent))
#     standings$finals[standings$team %in% c(df_sim$team[9], df_sim$opponent[9])] <- 
#       standings$finals[standings$team %in% c(df_sim$team[9], df_sim$opponent[9])] + 1/nsims
#     standings$champ[standings$team == df_sim$winner[9]] <- 
#       standings$champ[standings$team == df_sim$winner[9]] + 1/nsims
#   }
#   return(standings)
# }
# 
# 
# df <- map_dfr(unique(x$team_conf), standard_sim, nsims = 1000, loc_type = "N")
# df2 <- map_dfr(unique(x$team_conf), standard_sim, nsims = 1000, loc_type = "H")
# df3 <- map_dfr(unique(x$team_conf), afl_sim, nsims = 1000)
# 
# write_csv(bind_rows(df, df2, df3), "../../Desktop/sims.csv")
