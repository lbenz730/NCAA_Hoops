library(gt)
library(paletteer)
library(ncaahoopR)
library(tidyverse)
library(here)

sim_results <- read_csv(here('3.0_Files/ncaa_sims/ncaa_sims.csv'))
make_table <- function(sim_results, table_region, round, message = '') {
  
  df <- 
    select(ncaa_colors, 'team' = ncaa_name, logo_url) %>% 
    inner_join(sim_results) 
  
  df <- df[df[[round]] != 0,]
    
  if(table_region != 'all') {
    df <- filter(df, region == table_region)
    df <- 
      df %>% 
      mutate('expected_elim_round' = 
               1 * (1 - first_round) + 
               2 * (1 - second_round - first_round) +
               3 * (1 - sweet_sixteen - second_round - first_round) + 
               4 * (1 - elite_eight - sweet_sixteen - second_round - first_round) +
               5 * (1 - final_four - elite_eight - sweet_sixteen - second_round - first_round) +
               6 * (1 -  championship_game - final_four - elite_eight - sweet_sixteen - second_round - first_round)) %>% 
      arrange(expected_elim_round) %>% 
      select(-expected_elim_round)
  }
  else {
   df <- arrange(df, -champ)
  }
  

  
  df %>% 
    gt() %>% 
    
    ### Ratings 
    data_color(
      columns = vars(rating),
      colors = scales::col_numeric(
        palette = ggsci::rgb_material('amber', n = 68),
        domain = c(min(sim_results$rating), max(sim_results$rating)),
      )
    ) %>% 
    fmt_number(
      columns = vars(rating),
      decimals = 1
    ) %>% 
    
    ### Tournament Odds 
    data_color(
      columns = vars(first_round, second_round,
                     sweet_sixteen, elite_eight,
                     final_four, championship_game,
                     champ),
      colors = scales::col_numeric(
        palette = ggsci::rgb_material('amber', n = 68),
        domain = c(0,1),
      )
    ) %>% 
    
    fmt_percent(
      columns = vars(first_round, second_round,
                     sweet_sixteen, elite_eight,
                     final_four, championship_game,
                     champ),
      decimals = 1) %>% 
    
    ### Align Columns
    cols_align(
      align = "center",
      columns = T
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
          columns = vars(rating)
        )
      )
    ) %>% 
    text_transform(
      locations = cells_body(vars(logo_url)),
      fn = function(x) {
        web_image(
          url = x,
          height = 30
        )
      }
    ) %>% 
    cols_label(
      team = '',
      logo_url = '',
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
    tab_source_note("Based on 10,000 Simulations of NCAA Tournament") %>%
    tab_source_note("Table: Luke Benz (@recspecs730) | https://lbenz730.shinyapps.io/recspecs_basketball_central/") %>% 
    tab_header(
      title = md("**2021 NCAA Men's Basketball Tournament Odds**"),
      # subtitle = md(paste0('**', ifelse(table_region != 'all', paste0(table_region, " Region**"), paste0(message, '**'))))
      subtitle = html(web_image("https://upload.wikimedia.org/wikipedia/en/0/04/2021_NCAA_Men%27s_Final_Four_logo.svg"))
    ) %>% 
    tab_options(column_labels.font.size = 20,
                heading.title.font.size = 40,
                heading.subtitle.font.size = 30,
                heading.title.font.weight = 'bold',
                heading.subtitle.font.weight = 'bold'
                )
}

east <- make_table(sim_results, 'East', round = 'final_four')
mw <- make_table(sim_results, 'Midwest', round = 'final_four')
south <- make_table(sim_results, 'South', round = 'final_four')
west <- make_table(sim_results, 'West', round = 'final_four')
all <- make_table(sim_results, 'all', round = 'final_four', 'Final Four')

gtsave(east, filename = here('3.0_Files/ncaa_sims/figures/east.png'), expand = 10)
gtsave(mw, filename = here('3.0_Files/ncaa_sims/figures/mw.png'), expand = 10)
gtsave(south, filename = here('3.0_Files/ncaa_sims/figures/south.png'), expand = 10)
gtsave(west, filename = here('3.0_Files/ncaa_sims/figures/west.png'), expand = 10)
gtsave(all, filename = here('3.0_Files/ncaa_sims/figures/ncaa_sims.png'), expand = 10)

