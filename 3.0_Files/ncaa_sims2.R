library(tidyverse)
library(data.table)
library(furrr)
options(future.fork.enable = T)

### Number of sims to run
n_sims <- 100
set.seed(13579)

### Function to sim games 
sim_game <- function(team, opponent, win_prob) {
 ifelse(runif(1) <= win_prob, team, opponent)
}

### Read in Seed list and team ratings
seed_list <- read_csv('seed_list.csv')
power_rankings <- read_csv('Power_Rankings/power_rankings.csv')
glm_pointspread <- read_rds('../glm_pointspread.rds')

seed_list <- 
  seed_list %>% 
  inner_join(select(power_rankings, team, 'rating' = yusag_coeff), by = 'team')


### Compute a win-probability matrix for each possible combination of teams
wp_matrix <- 
  crossing('team' = seed_list$team, 
           'opponent' = seed_list$team) %>% 
  inner_join(select(seed_list, team, rating), 
             by = 'team') %>% 
  inner_join(select(seed_list, team, rating), 
             by = c('opponent' = 'team'), 
             suffix = c('_team', '_opponent')) %>% 
  ### Score Diff of Game
  mutate('pred_score_diff' = rating_team - rating_opponent) %>%  
  ### Win Prob for Team over Opponent
  mutate('win_prob' = predict(glm_pointspread, newdata = ., type = 'response'))


### First Four
first_four <- 
  seed_list %>% 
  filter(first_four) %>% 
  group_by(region, seed) %>%
  group_map(~tibble('team' = .x$team[1], 
                    'opponent' = .x$team[2],
                    'seed' = .x$seed[1],
                    'region' = .x$region[1]),
            .keep = T) %>% 
  bind_rows() %>% 
  inner_join(wp_matrix, by = c('team', 'opponent'))


map_dfr(
            
            
            