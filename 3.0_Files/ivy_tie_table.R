library(tidyverse)
library(gt)
library(ncaahoopR)
library(glue)
options(chromote.headless = "new")

df_standings <- read_csv('3.0_Files/Predictions/all_standings.csv')


df_ties <- 
  df_standings %>% 
  group_by(sim_id, rank_start) %>% 
  summarise('max_rank' = max(rank_final),
            'n' = n(),
            'tied_teams' = paste(sort(team), collapse = '/'),
            'ordering' = paste(team, collapse = '/')) %>% 
  ungroup() %>% 
  filter(n > 1) %>% 
  filter(rank_start <= 4) %>% 
  group_by(tied_teams, ordering) %>% 
  count() %>% 
  group_by(tied_teams) %>% 
  mutate('n_tie' = sum(n),
         'n_teams' = length(unlist(strsplit(tied_teams[1], '/'))),
         'pct' = n/n_tie,
         'tie_freq' = n_tie/max(df_standings$sim_id)) %>% 
  ungroup()

x <- 
  df_ties %>% 
  group_by(tied_teams, tie_freq) %>% 
  arrange(-tie_freq, -pct) %>% 
  select(-n, -n_tie) 

gt2 <- 
  x %>% 
  filter(n_teams == 2, tie_freq > 0.01) %>%
  select(-n_teams) %>% 
  gt() %>% 
  cols_align('center') %>% 
  fmt_percent('pct', decimals = 1) %>% 
  text_transform(
    locations = cells_body(ordering),
    fn = function(z) {
      tms <- map(z, ~unlist(strsplit(.x, '/')))
      urls <- map(tms, function(w) map_chr(w, ~ncaa_colors$logo_url[ncaa_colors$ncaa_name == .x]))
      lapply(urls, function(x) {
        html(paste(map_chr(x, function(t) {
          web_image(
            url = t,
            height = 30
          )
        }), collapse = ' '))
      })
    }) %>% 
  text_transform(
    locations = cells_row_groups(),
    fn = function(z) {
      pct <- as.numeric(str_extract(z, '\\d+.\\d+'))
      z <- gsub('\\s+-.*', '', z)
      tms <- map(z, ~unlist(strsplit(.x, '/')))
      urls <- map(tms, function(w) map_chr(w, ~ncaa_colors$logo_url[ncaa_colors$ncaa_name == .x]))
      code <- 
        lapply(urls, function(x) {
          html(paste('<b>Tied Teams:</b>', 
                     paste(map_chr(x, function(t) {
                       web_image(
                         url = t,
                         height = 30
                       )
                     }), collapse = ' ')),
               
               '<b>Tie Probability:</b>',
               paste0(sprintf('%0.1f', 100 * pct[1]), '%')
               
          )
        })
      code
    }) %>% 
  cols_label(ordering = '', 
             pct = '') %>% 
  tab_header(title =  md("<img src='https://content.sportslogos.net/logos/153/4824/full/ivy_league_logo_primary_2019_sportslogosnet-9024.png' style='height: 50px; width: auto; vertical-align: middle;'> "),
             subtitle = md(glue('**Most Common Ties for<br>Any Place in Top 4 (2-way Ties)**'))) %>% 
  tab_options(column_labels.font.size = 16,
              row_group.font.size = 20, 
              # row_group.padding.horizontal = 100,
              heading.title.font.size = 30,
              heading.subtitle.font.size = 40,
              heading.title.font.weight = 'bold',
              heading.subtitle.font.weight = 'bold'
  )

gtsave(gt2, 'graphics/ivy/ties_2.png', zoom = 2)


gt3 <- 
  x %>% 
  filter(n_teams == 3, tie_freq > 0.01) %>%
  select(-n_teams) %>% 
  gt() %>% 
  cols_align('center') %>% 
  fmt_percent('pct', decimals = 1) %>% 
  text_transform(
    locations = cells_body(ordering),
    fn = function(z) {
      tms <- map(z, ~unlist(strsplit(.x, '/')))
      urls <- map(tms, function(w) map_chr(w, ~ncaa_colors$logo_url[ncaa_colors$ncaa_name == .x]))
      lapply(urls, function(x) {
        html(paste(map_chr(x, function(t) {
          web_image(
            url = t,
            height = 30
          )
        }), collapse = ' '))
      })
    }) %>% 
  text_transform(
    locations = cells_row_groups(),
    fn = function(z) {
      pct <- as.numeric(str_extract(z, '\\d+.\\d+'))
      z <- gsub('\\s+-.*', '', z)
      tms <- map(z, ~unlist(strsplit(.x, '/')))
      urls <- map(tms, function(w) map_chr(w, ~ncaa_colors$logo_url[ncaa_colors$ncaa_name == .x]))
      code <- 
        lapply(urls, function(x) {
          html(paste('<b>Tied Teams:</b>', 
                     paste(map_chr(x, function(t) {
                       web_image(
                         url = t,
                         height = 30
                       )
                     }), collapse = ' ')),
               
               '<b>Tie Probability:</b>',
               paste0(sprintf('%0.1f', 100 * pct[1]), '%')
               
          )
        })
      code
    }) %>% 
  cols_label(ordering = '', 
             pct = '') %>% 
  tab_header(title =  md("<img src='https://content.sportslogos.net/logos/153/4824/full/ivy_league_logo_primary_2019_sportslogosnet-9024.png' style='height: 50px; width: auto; vertical-align: middle;'> "),
             subtitle = md(glue('**Most Common Ties for<br>Any Place in Top 4 (3-way Ties)**'))) %>% 
  tab_options(column_labels.font.size = 16,
              row_group.font.size = 20, 
              # row_group.padding.horizontal = 100,
              heading.title.font.size = 30,
              heading.subtitle.font.size = 40,
              heading.title.font.weight = 'bold',
              heading.subtitle.font.weight = 'bold'
  )

gtsave(gt3, 'graphics/ivy/ties_3.png')



df_brown <- 
  df_standings %>% 
  filter(rank_start <= 4) %>% 
  group_by(sim_id, rank_start) %>% 
  filter(n() == 2, any(team == 'Brown'), any(team == 'Dartmouth'))


df_brown %>% 
  group_by(sim_id) %>% 
  mutate('winner' = team[which.min(rank_final)]) %>% 
  group_by(relevant_teams, metric, winner) %>% 
  count() %>% 
  View()

df_brown$metric



df_standings %>% 
  filter(team == 'Harvard', n_win == 7) %>% 
  filter(rank_final <= 4)

predictions <- read_csv('3.0_Files/Predictions/predictions.csv') 
predictions %>% 
  filter(team == 'Harvard', is.na(score_diff)) %>% 
  select(team, opponent, date, location, wins) 
