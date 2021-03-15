library(tidyverse)
library(data.table)
library(furrr)
options(future.fork.enable = T)

### Number of sims to run
n_sims <- 10000
set.seed(13579)

### Function to sim games 
sim_game <- function(team, opponent, win_prob) {
  ifelse(runif(1) <= win_prob, team, opponent)
}

### Function to Build a Bracket For a Round
build_bracket <- function(seed_list) {
  n <- nrow(seed_list)
  ix <- seq(1, n, 2)
  tibble('team' = seed_list$team[ix],
         'opponent' = seed_list$team[-ix]) %>% 
    inner_join(wp_matrix, by = c('team', 'opponent'))
  
}

### Function to Simulate Games in Round
sim_round <- function(bracket) {
  pmap_chr(list('team' = bracket$team,
                'opponent' = bracket$opponent,
                'win_prob' = bracket$win_prob),
           sim_game)
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
  build_bracket()
first_four <- map(1:n_sims, ~{first_four})

first_four_winners <- future_map(first_four, sim_round)

### Brackets for Tournament Proper
ncaa_brackets <- 
  map(first_four_winners, ~{
    filter(seed_list, !first_four | team %in% .x) 
  })

### Keep Track of Winners
round_winners <- list()

### Simulate Tournament
for(i in 1:6) {
  cat('Simulating Round', i, 'of', 6, '\n')
  ### in bracket Form
  brackets <- future_map(ncaa_brackets, build_bracket)
  winners <- future_map(brackets, sim_round)
  round_winners[[i]] <- unlist(winners)
  ncaa_brackets <- map2(ncaa_brackets, winners, ~{filter(.x, team %in% .y)})
}

### Aggregate Results
sim_results <- 
  seed_list %>% 
  select(-elim_round) %>% 
  group_by(team) %>% 
  mutate('first_round' = ifelse(!first_four, 1, sum(team == unlist(first_four_winners))/n_sims),
         'second_round' = sum(team == round_winners[[1]])/n_sims,
         'sweet_sixteen' = sum(team == round_winners[[2]])/n_sims,
         'elite_eight' = sum(team == round_winners[[3]])/n_sims,
         'final_four' = sum(team == round_winners[[4]])/n_sims,
         'championship_game' = sum(team == round_winners[[5]])/n_sims,
         'champ' = sum(team == round_winners[[6]])/n_sims) %>% 
  ungroup() %>% 
  select(-first_four) %>% 
  arrange(-champ, -championship_game, 
          -final_four, -elite_eight, -sweet_sixteen,
          -second_round, -first_round) 
  
  


library(gt)
sim_results %>% 
  gt() %>% 
  data_color(
    columns = vars(rating),
    colors = scales::col_numeric(
      palette = c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"),
      domain = NULL,
    )
  ) %>% 
  fmt_number(
    columns = vars(rating),
    decimals = 1
    ) %>% 
  fmt_percent(
    columns = vars(first_round, second_round,
                     sweet_sixteen, elite_eight,
                     final_four, championship_game,
                     champ),
    # digits = 1,
    decimals = 0,
    pattern = "{x}")
  

