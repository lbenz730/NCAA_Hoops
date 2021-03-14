library(tidyverse)
library(data.table)
library(furrr)
options(future.fork.enable = T)

seed_list <- 
  read_csv('Bracketology/bracket.csv') %>% 
  select(team, 
         'rating' = yusag_coeff, 
         'seed' = seed_line) %>% 
  group_by(seed) %>% 
  slice(1:4) %>% 
  mutate('region' = c('East', 'West', 'Midwest', 'South')) %>% 
  ungroup()


ncaa_sims <- function(n_sims, seed_list, glm_pointspread) {
  ### Compute a win-probability matrix for each possible combination of teams
  crossing('team' = seed_list$team, 
           'opponent' = seed_list$team) %>% 
    inner_join(select(seed_list, team, rating), 
               by = 'team') %>% 
    inner_join(select(seed_list, team, rating), 
               by = c('opponent' = 'team'), 
               suffix = c('_team', '_opponent')) %>% 
    mutate('pred_score_diff' = rating_team - rating_opponent) %>%  ### Score Diff of Game
    mutate('win_prob' = predict(glm.pointspread, newdata = .)) ### Win Prob for Team over Opponent


}