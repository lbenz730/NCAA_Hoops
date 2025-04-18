library(tidyverse)
library(data.table)
library(furrr)
options(future.fork.enable = T)
options(future.rng.onMisue = "ignore")

### Number of sims to run
n_sims <- 100000
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
         'opponent' = seed_list$team[-ix],
         'team_elim_round' = seed_list$elim_round[ix],
         'opp_elim_round' = seed_list$elim_round[-ix]) %>% 
    inner_join(wp_matrix, by = c('team', 'opponent')) %>% 
    mutate('win_prob' = 
             case_when(
               is.na(team_elim_round) & is.na(opp_elim_round) ~ win_prob,
               is.na(team_elim_round) & !is.na(opp_elim_round) ~ 1,
               !is.na(team_elim_round) & is.na(opp_elim_round) ~ 0,
               team_elim_round > opp_elim_round ~ 1,
               team_elim_round < opp_elim_round ~ 0)
    )
  
}

### Function to Simulate Games in Round
sim_round <- function(bracket) {
  pmap_chr(list('team' = bracket$team,
                'opponent' = bracket$opponent,
                'win_prob' = bracket$win_prob),
           sim_game)
}

### Read in Seed list and team ratings
seed_list <- read_csv('3.0_Files/ncaa_sims/seed_list.csv')
power_rankings <- read_csv('3.0_Files//Power_Rankings/power_rankings.csv')
glm_pointspread <- read_rds('glm_pointspread.rds')

seed_list <- 
  seed_list %>% 
  inner_join(select(power_rankings, team, 'rating' = yusag_coeff), by = 'team')

seed <- seed_list$seed
names(seed) <- seed_list$team


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
write_csv(wp_matrix, '3.0_Files/ncaa_sims/ncaa_wp_matrix_2025.csv')

### First Four
first_four <- 
  seed_list %>% 
  filter(first_four) %>% 
  build_bracket()

first_four <- map(1:n_sims, ~{first_four})
first_four_winners <- future_map(first_four, sim_round, .options = furrr_options(seed = 781))

### Brackets for Tournament Proper
ncaa_brackets <- 
  map(first_four_winners, ~{
    filter(seed_list, !first_four | team %in% .x) 
  })

### Keep Track of Winners
round_winners <- list()
round_points <- c(1,2,3,4,5,10)
df_upsets <- NULL

### Simulate Tournament
for(i in 1:6) {
  cat('Simulating Round', i, 'of', 6, '\n')
  ### in bracket Form
  brackets <- future_map(ncaa_brackets, build_bracket)
  winners <- future_map(brackets, sim_round,.options = furrr_options(seed = 781 + i))
  losers <- future_map2(brackets, winners, ~{setdiff(c(rbind(.x$team, .x$opponent)), .y)})
  upsets <- future_map2(winners, losers, ~{seed[.x] > seed[.y]})
  
  round_winners[[i]] <- unlist(winners)
  
  ### Points
  df_round <- 
    tibble('round' = i,
           'team' = unlist(winners),
           'upset' = unlist(upsets))
  df_upsets <- bind_rows(df_upsets, df_round)
  
  
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
  select(-first_four) 

write_csv(sim_results, '3.0_Files/ncaa_sims/ncaa_sims.csv')

