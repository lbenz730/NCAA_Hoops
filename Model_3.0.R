#############################  Read CSVs #######################################
library(dplyr) 
library(readr)
library(lubridate)
library(purrr)
library(furrr)
plan(multicore(workers = 8))
options(future.fork.enable = T)
options(future.rng.onMisue = "ignore")
options(dplyr.summarise.inform = FALSE)
x <- read_csv(paste("3.0_Files/Results/2024-25/NCAA_Hoops_Results",
                    month(Sys.Date()), day(Sys.Date()), paste0(year(Sys.Date()), ".csv"),
                    sep = "_"))
train <- read_csv("3.0_Files/Results/2017-18/training.csv")
confs <- read_csv("3.0_Files/Info/conferences.csv")
deadlines <- read_csv("3.0_Files/Info/deadlines.csv")
conf_tournaments <- read_csv('3.0_Files/Info/conf_tournaments.csv')
priors <- 
  read_csv("3.0_Files/Info/prior.csv") %>% 
  arrange(tolower(team))
# seed_list <- read_csv('3.0_Files/ncaa_sims/seed_list.csv')
source("3.0_Files/powerrankings.R")
source("3.0_Files/Ivy_Sims.R")
source("3.0_Files/tiebreak.R")
source("3.0_Files/rpi.R")
source("3.0_Files/record_evaluator.R")
source("3.0_Files/bracketology.R")
source("3.0_Files/helpers.R")
source("3.0_Files/tourney_sim.R")
source("3.0_Files/conf_tourney_sims.R")

params <- 
  list('conf_sims' = 1000,
       # 'n_ct' = 5000,
       'ivy_sims' = 2000,
       'psf_sims' = 200,
       'pct_post' = 0.25)

########################  Data Cleaning ########################################
x <- 
  x %>%
  rename(team_score = teamscore, opp_score = oppscore) %>%
  mutate(date = as.Date(paste(year, month, day, sep = "-")),
         score_diff = team_score - opp_score,
         total_score = team_score + opp_score,
         season_id = "2024-25", game_id = NA, opp_game_id = NA, 
         team_conf = NA, opp_conf = NA, conf_game = NA) %>%
  select(date, team, opponent, location, team_score, opp_score,
         score_diff, total_score, game_id, opp_game_id, team_conf, opp_conf,
         year, month, day, season_id, D1, OT, postponed, canceled) %>% 
  filter(D1 == 2) 


teams <- priors$team
# teams <- teams[order(tolower(teams))]

### Game IDs
x <- 
  x %>% 
  group_by(team) %>% 
  mutate('game_id' = 1:n()) %>% 
  ungroup() 

### Opp Game IDs
x <- 
  inner_join(x, select(x, date, team, opponent, game_id), 
             relationship = "many-to-many",
             by = c("team" = "opponent",
                    "opponent" = "team", 
                    "date" = "date")) %>%
  filter(!duplicated(paste(team, game_id.x))) %>%
  mutate(opp_game_id = game_id.y) %>%
  rename(game_id = game_id.x) %>%
  select(-game_id.y)

### Confs
x <- 
  mutate(x, team_conf = sapply(x$team, get_conf),
         opp_conf = sapply(x$opponent, get_conf),
         conf_game = team_conf == opp_conf)

### Reg Season
x <- 
  left_join(x, deadlines, by = c("team_conf" = "conference")) %>%
  mutate('reg_season' = date <= deadline) %>%
  select(-deadline)

### Eliminate Teams from Auto Bid contention
# confs <- eliminate(filter(x, score_diff < 0, !reg_season) %>% pull(team), confs)

# ### Update NCAA Eliminations:
# round_dates <-
#   list('0' = c('2024-03-19', '2024-03-20'),
#        '1' = c('2024-03-21', '2024-03-22'),
#        '2' = c('2024-03-23', '2024-03-24'),
#        '3' = c('2024-03-28', '2024-03-29'),
#        '4' = c('2024-03-30', '2024-03-31'),
#        '5' = c('2024-04-06', '2024-04-06'),
#        '6' = c('2024-04-08', '2024-04-08'))
# 
# eliminate_ncaa_teams(seed_list, round_dates)


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
pad <- tibble('team' = teams,
              'opponent' = teams, 
              'location' = sample(c('H', 'V', 'N'), length(teams), replace = T),
              'weights' = 0.000001,
              'score_diff' = 0,
              'team_score' = 0,
              'opp_score' = 0)


lm.hoops <- lm(score_diff ~ team + opponent + location, weights = weights, data = bind_rows(pad, x)) 
lm.off <- lm(team_score ~ team + opponent + location, weights = weights, data = bind_rows(pad, x)) 
lm.def <- lm(opp_score ~ team + opponent + location, weights = weights, data = bind_rows(pad, x)) 

### Update With Pre-Season Priors
priors <- mutate(priors, 
                 rel_yusag_coeff = yusag_coeff - yusag_coeff[1],
                 rel_off_coeff = off_coeff - off_coeff[1],
                 rel_def_coeff = def_coeff - def_coeff[1]) %>%
  arrange(tolower(team))

w <- sapply(priors$team, prior_weight)

lm.hoops$coefficients[is.na(lm.hoops$coefficients)] <- 0
lm.off$coefficients[is.na(lm.off$coefficients)] <- 0
lm.def$coefficients[is.na(lm.def$coefficients)] <- 0

lm.hoops$coefficients[2:length(teams)] <- 
  lm.hoops$coefficients[2:length(teams)] * w[-1] + priors$rel_yusag_coeff[-1] * (1-w[-1])
lm.hoops$coefficients[(length(teams)+1):(2 * length(teams)-1)] <- 
  lm.hoops$coefficients[(length(teams)+1):(2 * length(teams)-1)] * w[-1] - priors$rel_yusag_coeff[-1] * (1-w[-1])
lm.off$coefficients[2:length(teams)] <- 
  lm.off$coefficients[2:length(teams)] * w[-1] + priors$rel_off_coeff[-1] * (1-w[-1])
lm.off$coefficients[(length(teams)+1):(2 * length(teams)-1)] <- 
  lm.off$coefficients[(length(teams)+1):(2 * length(teams)-1)] * w[-1] - priors$rel_off_coeff[-1] * (1-w[-1])
lm.def$coefficients[2:length(teams)] <- 
  lm.def$coefficients[2:length(teams)] * w[-1] - priors$rel_def_coeff[-1] * (1-w[-1])
lm.def$coefficients[(length(teams)+1):(2 * length(teams)-1)] <- 
  lm.def$coefficients[(length(teams)+1):(2 * length(teams)-1)] * w[-1] + priors$rel_def_coeff[-1] * (1-w[-1])

lm.hoops$coefficients[c(1, (2 * length(teams)):(2 * length(teams)+1))] <- 
  w[1] * lm.hoops$coefficients[c(1, (2 * length(teams)):(2 * length(teams)+1))] + 
  (1-w[1]) * c(3.34957772, -3.34957772, -6.69915544)
lm.off$coefficients[c(1, (2 * length(teams)):(2 * length(teams)+1))] <- 
  w[1] * lm.off$coefficients[c(1, (2 * length(teams)):(2 * length(teams)+1))] + 
  (1-w[1]) * c(68.19705698, -2.93450334,  -3.34957772)
lm.def$coefficients[c(1, (2 * length(teams)):(2 * length(teams)+1))] <- 
  w[1] * lm.def$coefficients[c(1, (2 * length(teams)):(2 * length(teams)+1))] + 
  (1-w[1]) * c(64.84747926, 0.41507438,  3.34957772)

################################ Power Rankings ################################
power_rankings <- pr_compute(by_conf = F)
history <- read_csv("3.0_Files/History/history.csv")

x <- 
  mutate(x, pr_date = as.Date(unlist(map(x$date, ~max_date(.x, hist_dates = unique(history$date)))), origin = '1970-01-01')) %>%
  inner_join(select(power_rankings, team, rank),by = c("team" = "team")) %>%
  inner_join(select(power_rankings, team, rank),by = c("opponent" = "team")) %>% 
  inner_join(select(history, team, yusag_coeff, date, off_coeff, def_coeff), 
             by = c("team" = "team","pr_date" = "date")) %>%
  inner_join(select(history, team, yusag_coeff, date, off_coeff, def_coeff), 
             by = c("opponent" = "team","pr_date" = "date")) %>% 
  rename(rank = rank.x, opp_rank = rank.y, 
         yusag_coeff = yusag_coeff.x, opp_yusag_coeff = yusag_coeff.y,
         off_coeff = off_coeff.x, opp_off_coeff = off_coeff.y,
         def_coeff = def_coeff.x, opp_def_coeff = def_coeff.y) %>%
  select(-pr_date)

############################### Predictions ####################################
x <- 
  mutate(x, "hca" = case_when(location == "H" ~ lm.hoops$coefficients[1],
                              location == "V" ~ lm.hoops$coefficients[(2 * length(teams))],
                              location == "N" ~ 0),
         "hca_off" = case_when(location == "H" ~ abs(lm.off$coefficients[(2 * length(teams))]),
                               location == "V" ~ lm.off$coefficients[(2 * length(teams)+1)] - lm.off$coefficients[(2 * length(teams))],
                               location == "N" ~ 0),
         "hca_def" = case_when(location == "H" ~ -lm.def$coefficients[(2 * length(teams))],
                               location == "V" ~ lm.def$coefficients[(2 * length(teams)+1)] - lm.def$coefficients[(2 * length(teams))],
                               location == "N" ~ 0)) %>%
  mutate("pred_score_diff" = round(yusag_coeff - opp_yusag_coeff + hca, 1),
         "pred_team_score" = round(70 + off_coeff - opp_def_coeff + hca_off, 1),
         "pred_opp_score" = round(70 -def_coeff + opp_off_coeff + hca_def, 1),
         "pred_total_score" = pred_team_score + pred_opp_score) %>%
  select(-hca, -hca_off, -hca_def)

######################## Point Spread to Win Percentage Model #################
x$wins[x$score_diff > 0] <- 1
x$wins[x$score_diff < 0] <- 0
glm.pointspread <- glm(wins ~ pred_score_diff, 
                       data = bind_rows(select(train, wins, pred_score_diff),
                                        select(x, wins, pred_score_diff)), 
                       family=binomial) 
saveRDS(glm.pointspread, file = "glm_pointspread.rds")
x$wins[is.na(x$wins)] <- 
  round(predict(glm.pointspread, newdata = x[is.na(x$wins),], type = "response"), 3)
by_conf <- pr_compute(by_conf = T)
write_csv(x, "3.0_Files/Predictions/predictions.csv")
######################### Ivy League Specific Sims #############################
playoffs <- ivy.sim(params$ivy_sims)
# ivy_psf <- psf(params$psf_sims, min_date = Sys.Date(), max_date = Sys.Date()+6)
# playoffs <- read_csv('3.0_Files/Predictions/playoffs.csv')

############################ Conference Sims (No Tie-Breaks) ##################
if(lubridate::hour(Sys.time())  <= 12) {
  # confs <- update_conf_seeds()
  for(conf in setdiff(sort(unique(confs$conference)), 'Independent')) {
    # for(conf in sort(unique(confs$conference[!is.na(confs$conf_seed)]))) {
    visualize_schedule_data(conf)
    print(conf)
    df_f <- read_csv(paste0("3.0_Files/Predictions/conf_sims/", conf, ".csv"), col_types = cols())
    f <- !all(group_by(df_f, team, place) %>% count() %>% pull(n) == params$conf_sims)

    sims <- conf_fast_sim(conf, params$conf_sims, params$pct_post, params$n_ct, force = f)
    write_csv(sims$reg_season, paste0("3.0_Files/Predictions/conf_sims/", conf, ".csv"))

    df <-
      sims$reg_season %>%
      group_by(team, place) %>%
      summarise("pct" = n()/n_distinct(.$sim)) %>%
      ungroup() %>%
      tidyr::spread(key = "place", value = "pct") %>%
      mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
      rename("Team" = team)

    df$avg_seed <- apply(df[,-1], 1, function(x) {sum(x * as.numeric(names(df)[-1]))})
    df <-
      arrange(df, avg_seed) %>%
      select(-avg_seed)

    write_csv(df, paste0("3.0_Files/Predictions/conf_sims/", conf, "_win_matrix.csv"))

    standings <-
      group_by(sims$reg_season, team) %>%
      summarise("avg_wins" = mean(n_wins)) %>%
      arrange(desc(avg_wins)) %>%
      mutate("rank" = nrow(.):1) %>%
      arrange(team) %>%
      left_join(select(ncaahoopR::ncaa_colors, ncaa_name, primary_color),
                by = c("team" = "ncaa_name"))

    write_csv(standings, paste0("3.0_Files/Predictions/conf_sims/", conf, "_standings.csv"))

    if(conf != 'Ivy ') {
      write_csv(sims$post_season, paste0("3.0_Files/Predictions/conf_sims_ncaa/", conf, ".csv"))
    } else {
      playoffs %>%
        select(team, 'freq' = auto_bid) %>%
        mutate('freq' = freq/100) %>%
        write_csv(paste0("3.0_Files/Predictions/conf_sims_ncaa/", conf, ".csv"))
    }
  }
  # conf_tourney_graphics()
}


# source('3.0_Files/ivy_graphics.R')
########################### Bracketology #######################################
resumes <- get_resumes(new = T)
bracket <- make_bracket(tourney = T)
bracket_math <- make_bracket(tourney = F)

####################### NCAA Simulations #######################################
# source('3.0_Files/ncaa_sims/ncaa_sims.R')
# source('3.0_Files/ncaa_sims/ncaa_tables.R')

############################ System Evaluation #################################
min_date <- as.Date("2024-11-04")
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
nrow(filter(x, round(pred_team_score) == team_score))
filter(x, round(pred_team_score) == team_score, round(pred_opp_score) == opp_score)

#### Copy App Specific Files
file.copy(dir('3.0_Files/Results/2024-25/', full.names = T),
          paste0('app/', dir('3.0_Files/Results/2024-25/', full.names = T)), overwrite = T)
file.copy(dir('3.0_Files/Predictions/', full.names = T, recursive = T),
          paste0('app/', dir('3.0_Files/Predictions/', full.names = T, recursive = T)), overwrite = T)
file.copy(dir('3.0_Files/Bracketology/', full.names = T),
          paste0('app/', dir('3.0_Files/Bracketology/', full.names = T)), overwrite = T)
file.copy(dir('3.0_Files/ncaa_sims/', full.names = T),
          paste0('app/', dir('3.0_Files/ncaa_sims/', full.names = T)), overwrite = T)
file.copy(dir('3.0_Files/schedule_data/', full.names = T),
          paste0('app/', dir('3.0_Files/schedule_data/', full.names = T)), overwrite = T)
file.copy('glm_pointspread.rds', 'app/glm_pointspread.rds', overwrite = T)
file.copy('3.0_Files/History/history.csv', 'app/3.0_Files/History/history.csv', overwrite = T)
file.copy('3.0_Files/Info/conferences.csv', 'app/3.0_Files/Info/conferences.csv', overwrite = T)

devtools::install_github("lbenz730/ncaahoopR", force = T)
rsconnect::deployApp(forceUpdate = T, appDir = 'app')

