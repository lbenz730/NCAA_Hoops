library(tidyverse)
library(gt)
library(ncaahoopR)
library(gtExtras)

x <- read_csv('3.0_Files/Predictions/predictions.csv')
seed_list <- read_csv('3.0_Files/ncaa_sims/seed_list.csv')

df_games <- 
  x %>% 
  inner_join(seed_list, by = c('team')) %>% 
  inner_join(seed_list, by = c('opponent' = 'team')) %>% 
  filter(date == Sys.Date()) %>% 
  filter(seed.x < seed.y) %>% 
  inner_join(ncaa_colors %>% select(ncaa_name, logo_url), by = c('team' = 'ncaa_name')) %>% 
  inner_join(ncaa_colors %>% select(ncaa_name, logo_url), by = c('opponent' = 'ncaa_name')) %>% 
  select('region' = region.x, team, 'logo' = logo_url.x, 'seed_team' = seed.x,
         opponent, 'logo_opp' = logo_url.y,  'seed_opponent' = seed.y, 
         pred_team_score, pred_opp_score, wins) %>%
  mutate('team' = paste0(team, ' (', seed_team, ')'),
         'opponent' = paste0(opponent, ' (', seed_opponent, ')')) %>% 
  select(-contains('seed'))


table <- 
  gt(df_games) %>% 
  ### Align Columns
  cols_align(align = "center", columns = everything()) %>% 
  
  ### Formatting
  fmt_number(columns = contains('score'), decimals = 1) %>% 
  fmt_percent(columns = wins, decimals = 1) %>% 
  
  ### Colors
  data_color(columns = contains('score'),
             colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100),
                                          domain = c(40, 100)
             )) %>% 
  
  
  ### Tournament Odds 
  data_color(columns = wins,
             colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100),
                                          domain = c(0,1)
             )) %>% 
  
  
  tab_spanner(label = 'Team 1', columns = c('team', 'logo')) %>% 
  tab_spanner(label = 'Team 2', columns = c('opponent', 'logo_opp')) %>% 
  
  ### Borders
  tab_style(
    style = list(
      cell_borders(
        sides = "bottom",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_column_labels(
        columns = gt::everything()
      )
    )
  ) %>% 
  tab_style(
    style = list(
      cell_borders(
        sides = "right",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_body(
        columns = c(logo_opp)
      )
    )
  ) %>% 
  text_transform(
    locations = cells_body(c(logo, logo_opp)),
    fn = function(x) {
      web_image(
        url = x,
        height = 30
      )
    }
  ) %>% 
  cols_label(
    region = 'Region',
    team = '',
    logo = '',
    opponent = '',
    logo_opp = '',
    pred_team_score = 'Team 1 Score',
    pred_opp_score = 'Team 2 Score',
    wins = 'Win %'
  ) %>% 
  tab_source_note("Table: Luke Benz (@recspecs730) | https://lbenz730.shinyapps.io/recspecs_basketball_central/") %>% 
  tab_header(
    title = md("**2023 NCAA Men's Basketball Tournament Odds**"),
    subtitle = md(paste0('**Game Predictions: ', Sys.Date(), '**'))) %>% 
  tab_options(column_labels.font.size = 20,
              column_labels.font.weight = 'bold',
              heading.title.font.size = 40,
              heading.subtitle.font.size = 30,
              heading.title.font.weight = 'bold',
              heading.subtitle.font.weight = 'bold'
  )

gtsave_extra(table, 'graphics/ncaa_tourney/predictions.png')
