library(mRchmadness)
library(tidyverse)

### My WP Matrix
wp_matrix <- read_csv('3.0_Files/ncaa_sims/ncaa_wp_matrix_2023.csv')

### Clean up their names so I can join my probs onto their IDs
df_teams <- 
  teams.men %>% 
  mutate('name' = gsub('State', 'St.', name)) %>% 
  mutate('name' = case_when(
    name == 'Texas A&M-CC' ~ 'A&M-Corpus Christi',
    name == 'Charleston' ~ 'Col. of Charleston',
    name == 'Fair Dickinson' ~ 'Fairleigh Dickinson',
    name == 'FAU' ~ 'Fla. Atlantic',
    name == 'Kennesaw St' ~ 'Kennesaw St.',
    name == 'UL Lafayette' ~ 'Louisiana',
    name == 'Miami' ~ 'Miami (FL)',
    name == 'Miss St' ~ 'Mississippi St.',
    name == 'NC St.' ~ 'NC State',
    name == 'N Kentucky' ~ 'Northern Ky.',
    name == 'Pitt' ~ 'Pittsburgh',
    name == "Saint Mary's" ~ "Saint Mary's (CA)",
    name == 'SE Missouri St' ~ 'Southeast Mo. St.',
    name == 'USC' ~ 'Southern California',
    name == 'UCSB' ~ 'UC Santa Barbara',
    name == 'UVA' ~ 'Virginia',
    T ~ name))

wp_matrix <- 
  wp_matrix %>% 
  inner_join(select(df_teams, name, id), by = c('team' = 'name')) %>% 
  inner_join(select(df_teams, name, 'id_opp' = id), by = c('opponent' = 'name'))

### Build Matrix
prob.matrix <- matrix(nrow = 68, ncol = 68)
rownames(prob.matrix) <- unique(wp_matrix$id)
colnames(prob.matrix) <- unique(wp_matrix$id)

for(i in 1:nrow(wp_matrix)) {
  prob.matrix[ wp_matrix$id[i] , wp_matrix$id_opp[i] ]  <- 
    wp_matrix$win_prob[i]
}

### Make Bracket 
set.seed(2020)
my.bracket <- 
  find.bracket(
    bracket.empty = bracket.men.2023,
    prob.matrix = prob.matrix,
    league = "men",
    num.candidates = 2000,
    num.sims = 10000,
    criterion = "win",
    pool.size = 10,
    bonus.round = c(1, 2, 4, 8, 16, 32),
    bonus.seed = rep(0, 16),
    bonus.combine = "add"
  )
draw.bracket(bracket.empty = bracket.men.2023, bracket.filled = my.bracket)
