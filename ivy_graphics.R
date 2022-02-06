library(dplyr) 
library(readr)
library(ggplot2)
library(ggrepel)
library(gt)
library(paletteer)
library(ncaahoopR)
library(purrr)
library(RColorBrewer)
library(ggridges)


### Ivy League
ivy_history <- read_csv('3.0_Files/Predictions/playoff_history.csv')
ivy_playoffs <- read_csv('3.0_Files/Predictions/playoffs.csv')

ivy_gt <- 
  ivy_playoffs %>% 
  arrange(desc(playoff_prob)) %>% 
  mutate_if(is.numeric, ~.x/100) %>% 
  inner_join(select(ncaa_colors, 'team' = ncaa_name, logo_url)) %>% 
  select(team, logo_url, everything()) %>% 
  
  
  gt() %>% 
  cols_label('team' = '', 
             'logo_url' = '',
             'auto_bid' = 'Auto Bid',
             'playoff_prob' = 'Make Playoffs',
             'seed1_prob' = '1-Seed',
             'seed2_prob' = '2-Seed',
             'seed3_prob' = '3-Seed',
             'seed4_prob' = '4-Seed') %>% 
  
  ### Hightlight Columns 
  data_color(
    columns = c(auto_bid, playoff_prob, contains('seed')),
    colors = scales::col_numeric(
      palette = ggsci::rgb_material('amber', n = 68),
      domain = c(0,1),
    )
  ) %>% 
  
  ### Percent
  fmt_percent(
    columns = c(auto_bid, playoff_prob, contains('seed')),
    decimals = 1) %>% 
  
  ### Align Columns
  cols_align(
    align = "center",
    columns = any_of(names(ivy_history))
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
        columns = c(logo_url)
      )
    )
  ) %>% 
  text_transform(
    locations = cells_body(c(logo_url)),
    fn = function(x) {
      web_image(
        url = x,
        height = 30
      )
    }
  ) %>% 
  tab_source_note("2022 Tournament hosted by Harvard University") %>%
  tab_source_note("Based on 5,000 Simulations. Ties broken according to official Ivy League tiebreaking rules.") %>%
  
  # tab_source_note("Table: Luke Benz (@recspecs730) | https://lbenz730.shinyapps.io/recspecs_basketball_central/") %>% 
  tab_header(
    title = md("**2022 Ivy League Men's Basketball Tournament Odds**"),
    # subtitle = md(paste0('**', table_region, " Region**"))
  ) %>% 
  tab_options(column_labels.font.size = 16,
              heading.title.font.size = 30,
              heading.subtitle.font.size = 20,
              heading.title.font.weight = 'bold',
              heading.subtitle.font.weight = 'bold'
  )


cols <- filter(ncaa_colors, conference == "Ivy") %>% 
  pull(primary_color)
ivy_history_plot <- 
  ggplot(ivy_history, aes(x = as.Date(date), y = playoff_prob)) + 
  geom_line(aes(group = team, col = team), size = 1.5) +
  facet_wrap(~team, ncol = 4) +
  theme_bw() + 
  labs(x = "Date", 
       y = "Playoff Probability",
       title = "Ivy League Playoff Odds",
       subtitle = "2021-2022") +
  theme(legend.position = "none",
        axis.title = element_text(size = 18),
        plot.title = element_text(size = 24, hjust = 0.5),
        plot.subtitle = element_text(size = 18, hjust = 0.5)) + 
  scale_y_continuous(labels = function(x) {paste0(x, "%")}, limits = c(0,100)) +
  scale_color_manual(values = cols)

ivy_sim <- read_csv(paste0("3.0_Files/Predictions/conf_sims/Ivy.csv"))

ivy_bar <- 
  ivy_sim %>% 
  group_by(n_wins, place) %>%
  count() %>% 
  group_by(n_wins) %>% 
  mutate('pct' = n/sum(n)) %>% 
  ungroup() %>% 
  ggplot(aes(x = n_wins, y = pct)) + 
  geom_col(aes(fill = as.factor(place))) + 
  scale_fill_manual(values = c(brewer.pal(5, 'Greens')[5:2], brewer.pal(5, 'Reds')[2:5])) + 
  scale_x_continuous(breaks = min(ivy_sim$n_wins):max(ivy_sim$n_wins)) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = '# of Conference Wins',
       y = 'Frequency',
       fill = 'Place',
       title = 'Distribution of Conference Finish By # of Wins',
       subtitle = 'Ivy League: Tiebreakers Applied')



ivy_psf <- read_rds('3.0_Files/Predictions/ivy_psf_full.rds') 

ivy_psf_gt <-
  ivy_psf %>% 
  inner_join(select(ncaa_colors, ncaa_name, logo_url), by = c('home' = 'ncaa_name')) %>% 
  inner_join(select(ncaa_colors, ncaa_name, logo_url), by = c('away' = 'ncaa_name'), suffix = c('_home', '_away')) %>% 
  mutate_if(is.numeric, ~{.x/100}) %>% 
  inner_join(x, by = c('home' = 'team', 
                       'away' = 'opponent',
                       'date' = 'date')) %>% 
  mutate('home_bar' = paste0('3.0_Files/Predictions/psf_figures/home_', 1:nrow(.), '.png'),
         'away_bar' = paste0('3.0_Files/Predictions/psf_figures/away_', 1:nrow(.), '.png'),
         'delta_bar' = paste0('3.0_Files/Predictions/psf_figures/delta_', 1:nrow(.), '.png')) %>% 
  mutate('favored' = ifelse(pred_score_diff > 0, logo_url_home, logo_url_away),
         'win_prob' = ifelse(pred_score_diff > 0, wins, 1-wins),
         'pred_score' = ifelse(pred_score_diff > 0, 
                               paste(sprintf('%0.1f', pred_team_score), sprintf('%0.1f', pred_opp_score), sep = '-'),
                               paste(sprintf('%0.1f', pred_opp_score), sprintf('%0.1f', pred_team_score), sep = '-'))) %>% 
  select(date, logo_url_away, logo_url_home, favored, pred_score, win_prob,
         psf, auto_bid_sf, away_bar, home_bar, delta_bar) %>% 
  arrange(date) %>% 
  filter(date == Sys.Date()) %>%
  gt() %>% 
  cols_label('date' = 'Date',
             'logo_url_home' = 'Home',
             'logo_url_away' = 'Away',
             'favored' = 'Winner', 
             'pred_score' = 'Score',
             'win_prob' = 'Win Probability',
             'psf' = 'Playoffs',
             'auto_bid_sf' = 'Auto Bid',
             'home_bar' = 'If Home Wins',
             'away_bar' = 'If Away Wins',
             'delta_bar' = 'Difference') %>% 
  
  tab_spanner(label = 'Matchup', columns = c('date', 'logo_url_away', 'logo_url_home')) %>% 
  tab_spanner(label = 'Game Prediction', columns = c('favored', 'pred_score', 'win_prob')) %>% 
  tab_spanner(label = 'Leverage', columns = c('psf', 'auto_bid_sf')) %>% 
  tab_spanner(label = 'Playoff Odds', columns = c('away_bar', 'home_bar', 'delta_bar')) %>% 
  
  
  ### Hightlight Columns 
  data_color(
    columns = c(auto_bid_sf, psf, win_prob),
    colors = scales::col_numeric(
      palette = ggsci::rgb_material('amber', n = 100),
      domain = c(0,1),
    )
  ) %>% 
  
  ### Percent
  fmt_percent(
    columns = c(auto_bid_sf, psf, win_prob),
    decimals = 1) %>% 
  
  ### Align Columns
  cols_align(
    align = "center",
    columns = any_of(c('home_bar', 'away_bar', 'delta_bar', 'favored', names(x), 'pred_score', 'win_prob', names(ivy_psf)))
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
        columns = c(logo_url_home, auto_bid_sf, home_bar, win_prob, away_bar, delta_bar)
      )
    )
  ) %>% 
  text_transform(
    locations = cells_body(c(logo_url_home, logo_url_away, favored)),
    fn = function(x) {
      web_image(
        url = x,
        height = 50
      )
    }
  ) %>% 
  text_transform(
    locations = cells_body(c(home_bar, away_bar, delta_bar)),
    fn = function(x) {
      local_image(
        filename  = x,
        height = 200
      )
    }
  ) %>% 
  tab_source_note('@recspecs730') %>% 
  tab_source_note('Leverage = total swing in all teams\' playoff/auto-bid odds between the two possible outcomes.') %>% 
  tab_source_note('Difference = delta in playoff odds per team if Home wins (right) vs Away Wins (left)') %>% 
  tab_source_note("2022 Tournament hosted by Harvard University") %>%
  tab_source_note("Based on 1,000 Simulations of each outcome. Ties broken according to official Ivy League tiebreaking rules.") %>% 
  tab_header(title = paste0(
    'Ivy League Playoff Leverage: ', 
    min(ivy_psf$date), 
    # ifelse(n_distinct(ivy_psf$date) > 1, paste(' -', max(ivy_psf$date)), ''),
    ''
  )) %>% 
  tab_options(column_labels.font.size = 20,
              heading.title.font.size = 40,
              heading.subtitle.font.size = 30,
              heading.title.font.weight = 'bold',
              heading.subtitle.font.weight = 'bold'
  )


make_standings_plot <- function(conf) {
  sims <- read_csv(paste0("3.0_Files/Predictions/conf_sims/", conf, ".csv"))
  if(nrow(sims) < 10) {
    p <- NULL
  } else{
    
    standings <- 
      group_by(sims, team) %>%
      summarise("avg_wins" = mean(n_wins)) %>%
      arrange(desc(avg_wins)) %>%
      mutate("rank" = nrow(.):1) %>%
      arrange(team) %>%
      left_join(select(ncaahoopR::ncaa_colors, ncaa_name, primary_color), 
                by = c("team" = "ncaa_name"))
    
    
    sims$team <- as.factor(sims$team)
    sims$team <- reorder(sims$team, rep(standings$rank, 10000))
    standings <- arrange(standings, avg_wins)
    
    p <- 
      ggplot(sims, aes(x = n_wins, y = team, fill = team)) + 
      geom_density_ridges(stat = "binline", scale = 0.7, binwidth = 1) + 
      labs(x ="# of Wins", 
           y = "Team",
           title = "Distribution of Conference Wins",
           subtitle = conf) +
      theme(legend.position = "none") +
      scale_fill_manual(values = c(standings$primary_color)) +
      scale_x_continuous(breaks = c(0:max(sims$n_wins)))
  }
  return(p)
}

df_wins <- 
  read_csv('3.0_Files/Predictions/win_place_results.csv')

df_summary <- 
  df_wins %>% 
  group_by(team, n_wins) %>% 
  summarise('freq' = n()/nrow(.) * 8,
            'playoff_prob' = mean(playoff),
            'tiebreak_prob' = mean(tiebreak),
            'win_tiebreak' = mean(playoff[tiebreak])) %>% 
  ungroup() %>% 
  pivot_longer(-c(team, n_wins),
               names_to = 'condition',
               values_to = 'prob') %>% 
  mutate('condition_prime' = 
           case_when(condition == 'freq' ~ 'Finish with # of Wins',
                     condition == 'playoff_prob' ~ 'Make Playoffs',
                     condition == 'tiebreak_prob' ~ 'Tiebreaker for Final Playoff Spot',
                     condition == 'win_tiebreak' ~ 'Win Tiebreaker for Final Playoff Spot'))

ivy_snapsnot <- 
  ggplot(df_summary, aes(x = n_wins, y = prob)) + 
  facet_grid(team~forcats::fct_reorder(condition_prime, condition)) + 
  geom_point(aes(col = team), size = 3) +
  geom_line(aes(col = team)) +
  scale_x_continuous(breaks = 1:max(df_wins$n_wins)) +
  scale_y_continuous(labels = scales::percent) + 
  scale_color_manual(values = cols) + 
  theme(legend.position = 'none',
        strip.text = element_text(size = 12)) + 
  labs(x = '# of Conference Wins',
       y = 'Probability of Event w/ # of Wins',
       title = 'Ivy Madness Playoff Snapshot',
       subtitle = 'Tiebreaking Scenarios Applied',
       color = '') 


ggsave(plot = ivy_snapsnot, '..graphics/ivy_snapshot.png', height = 9, width = 16)
make_standings_plot('Ivy') 
ggsave('graphics/ivy_standings.png', height = 9/1.5, width = 16/1.5)
ivy_bar
ggsave('graphics/ivy_bar.png', height = 9/1.5, width = 16/1.5)
gtsave(ivy_psf_gt, 'graphics/ivy_psf.png')
gtsave(ivy_gt, 'graphics/ivy_playoffs.png')