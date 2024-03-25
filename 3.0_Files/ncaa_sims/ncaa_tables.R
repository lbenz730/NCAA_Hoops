library(gt)
library(paletteer)
library(ncaahoopR)
library(tidyverse)
library(here)
library(gtExtras)



sim_results <- read_csv(here('3.0_Files/ncaa_sims/ncaa_sims.csv'))
seed_list <-  read_csv(here('3.0_Files/ncaa_sims/seed_list.csv'))

make_table <- function(sim_results, table_region, round, message = '') {
  
  img_files <- dir('app/www/')
  teams <- read_csv(here('3.0_Files/Info/conferences.csv')) %>% pull(team)
  df_img <- 
    tibble('team' = teams,
           'logo_file' = paste0('app/www/', map_chr(teams, ~case_when(paste0(.x, '.png') %in% img_files ~ paste0(.x, '.png'), 
                                                                      paste0(.x, '.svg') %in% img_files ~ paste0(.x, '.svg'), 
                                                                      paste0(.x, '.jpg') %in% img_files ~ paste0(.x, '.jpg')))))
  
  df <- 
    df_img %>% 
    inner_join(sim_results) 
  
  round <- 'second_round'
  df <- df[df[[round]] != 0,]
  
  elim <- 
    seed_list %>% 
    filter(!is.na(elim_round)) %>% 
    pull(team)
  
  df <- 
    df %>% 
    mutate('expected_elim_round' = 
             0 * (1 - first_round) +
             -1 * (second_round - first_round) +
             -2 * (sweet_sixteen - second_round) +
             -3 * (elite_eight - sweet_sixteen) +
             -4 * (final_four - elite_eight) +
             -5 * (championship_game - final_four) + 
             -6 * (champ - championship_game) + 
             7 * champ) %>% 
    arrange(-expected_elim_round, -rating) %>% 
    select(-expected_elim_round)
  
  
  if(table_region != 'all') {
    df <- filter(df, region == table_region)
    
  }
  
  
  
  df %>% 
    gt() %>% 
    
    
    ### Ratings 
    data_color(
      columns = c(rating),
      fn = scales::col_numeric(
        palette = 'RdYlGn',
        domain = c(min(sim_results$rating), max(sim_results$rating)),
      )
    ) %>% 
    fmt_number(
      columns = c(rating),
      decimals = 1
    ) %>% 
    
    ### Tournament Odds 
    data_color(
      columns = any_of(c('first_round', 'second_round',
                         'sweet_sixteen', 'elite_eight',
                         'final_four', 'championship_game',
                         'champ')),
      fn = scales::col_numeric(
        palette = ggsci::rgb_material('amber', n = 68),
        domain = c(0,1),
      )
    ) %>% 
    
    fmt_percent(
      columns = any_of(c('first_round', 'second_round',
                         'sweet_sixteen', 'elite_eight',
                         'final_four', 'championship_game',
                         'champ')),
      decimals = 1) %>% 
    
    ### Align Columns
    cols_align(
      align = "center",
      columns = everything()
    ) %>% 
    
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
          columns = c(rating)
        )
      )
    ) %>% 
    text_transform(
      locations = cells_body(c(logo_file)),
      fn = function(x) {
        local_image(
          filename = gsub('amp;', '', x),
          height = 30
        )
        
      }
    ) %>% 
    cols_label(
      team = '',
      logo_file = '',
      region = 'Region',
      seed = 'Seed',
      rating = 'Rating',
      first_round = '1st Round',
      second_round = '2nd Round',
      sweet_sixteen = 'Sweet 16',
      elite_eight = 'Elite 8',
      final_four = 'Final 4',
      championship_game = 'NCG',
      champ = 'Champion'
    ) %>% 
    tab_source_note("Based on 100,000 Simulations of NCAA Tournament") %>%
    tab_source_note("Rating: Points relative to baseline NCAA team on neutral floor") %>% 
    tab_source_note("Table: Luke Benz (@recspecs730) | https://lbenz730.shinyapps.io/recspecs_basketball_central/") %>% 
    tab_header(
      title = md("**2024 NCAA Men's Basketball Tournament Odds**"),
      subtitle = md(paste0('**', ifelse(table_region != 'all', paste0(table_region, " Region**"), paste0(message, '**'))))
    ) %>% 
    tab_options(column_labels.font.size = 20,
                column_labels.font.weight = 'bold',
                heading.title.font.size = 40,
                heading.subtitle.font.size = 30,
                heading.title.font.weight = 'bold',
                heading.subtitle.font.weight = 'bold'
    )
}

east <- make_table(sim_results, 'East')
mw <- make_table(sim_results, 'Midwest')
south <- make_table(sim_results, 'South')
west <- make_table(sim_results, 'West')
all <- make_table(sim_results, 'all')

gtsave_extra(east, filename = here('graphics/ncaa_tourney/east.png'), expand = 10)
gtsave_extra(mw, filename = here('graphics/ncaa_tourney/mw.png'), expand = 10)
gtsave_extra(south, filename = here('graphics/ncaa_tourney/south.png'), expand = 10)
gtsave_extra(west, filename = here('graphics/ncaa_tourney/west.png'), expand = 10)
gtsave_extra(all, filename = here('graphics/ncaa_tourney/ncaa_sims.png'), expand = 10)

