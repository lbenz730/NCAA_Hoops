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
library(tidyr)

replace_na <- function(x, r) {
  x[is.na(x)] <- r 
  return(x)
}

glm.pointspread <- readRDS("glm_pointspread.rds")
ncaa_sims <- read_csv('3.0_Files/ncaa_sims/ncaa_sims.csv')
t_confs <- gsub('\\.csv', '', dir('3.0_Files/Predictions/conf_tourney_sims/2023-24', full.names = F))



theme_set(theme_bw() +
            theme(axis.title = element_text(size = 20),
                  axis.text = element_text(size = 12),
                  plot.title = element_text(size = 24, hjust  = 0.5),
                  plot.subtitle = element_text(size = 18, hjust = 0.5),
                  legend.title = element_text(size = 12, hjust = 0.5),
                  legend.position = "bottom"))

x <- read_csv("3.0_Files/Predictions/predictions.csv")
teams <- sort(unique(x$team))

img_files <- dir('www/')
df_img <- 
  tibble('team' = teams,
         'logo_file' = paste0('www/', map_chr(teams, ~ifelse(paste0(.x, '.png') %in% img_files, paste0(.x, '.png'), paste0(.x, '.svg')))))

history <- read_csv("3.0_Files/History/history.csv") %>%
  mutate("date" = as.Date(date)) 

rankings <- filter(history, date == max(date)) %>%
  select(-date) %>%
  select(rank, everything())
rankings_clean <- filter(history, date == max(date)) %>%
  select(-date) %>%
  select(rank, everything())

names(rankings) <- c("Rank", "Team", "Conference", "Net Rating", "Off. Rating", 
                     "Def. Rating", "Off. Rank", "Def. Rank")


confs <- read_csv("3.0_Files/Info/conferences.csv")


records <- 
  x %>% 
  filter(!canceled, !postponed | (postponed & team_conf == 'Ivy' & conf_game)) %>% 
  group_by(team, team_conf) %>%
  summarise("n_win" = sum(wins),
            "n_loss" = sum(1-wins),
            "conf_wins" = sum(wins[conf_game & reg_season]),
            "conf_losses" = sum(1 - wins[conf_game & reg_season])) %>%
  ungroup()

records_actual <- 
  x %>% 
  filter(!canceled, !postponed) %>% 
  mutate('tier' = 
           case_when(
             opp_rank <= 50 & location == "N" ~ 'A',
             opp_rank <= 30 & location == "H" ~ 'A',
             opp_rank <=  75 & location == "V" ~ 'A',
             
             opp_rank >= 51 & opp_rank <= 100 & location == "N" ~ 'B',
             opp_rank >= 31 & opp_rank <= 75 & location == "H" ~ 'B', 
             opp_rank >= 76 & opp_rank <= 135 & location == "V" ~ 'B',
             
             opp_rank >= 101 & opp_rank <= 200 & location == "N" ~ 'C',
             opp_rank >= 76 & opp_rank <= 160 & location == "H" ~ 'C', 
             opp_rank >= 136 & opp_rank <= 240 & location == "V" ~ 'C',
             
             opp_rank >= 201 & location == "N" ~ 'D',
             opp_rank >= 161 & location == "H" ~ 'D', 
             opp_rank >= 241 & location == "V" ~ 'D'
             
           )) %>% 
  group_by(team, team_conf) %>%
  summarise("n_win" = sum(wins[!is.na(team_score)]),
            "n_loss" = sum(1-wins[!is.na(team_score)]),
            'n_win_a' = sum(wins[tier == 'A' & !is.na(team_score)]),
            'n_loss_a' = sum(1-wins[tier == 'A'& !is.na(team_score)]),
            'n_win_b' = sum(wins[tier == 'B'& !is.na(team_score)]),
            'n_loss_b' = sum(1-wins[tier == 'B' & !is.na(team_score)]),
            'n_win_c' = sum(wins[tier == 'C' & !is.na(team_score)]),
            'n_loss_c' = sum(1-wins[tier == 'C' & !is.na(team_score)]),
            'n_win_d' = sum(wins[tier == 'D' & !is.na(team_score)]),
            'n_loss_d' = sum(1-wins[tier == 'D' & !is.na(team_score)]),
            "conf_wins" = sum(wins[conf_game & !is.na(team_score)]),
            "conf_losses" = sum(1 - wins[conf_game& !is.na(team_score) ])) %>%
  ungroup()

non_d1 <- read_csv(paste0("3.0_Files/Results/2023-24/NCAA_Hoops_Results_",
                          paste(gsub("^0", "", unlist(strsplit(as.character(max(history$date)), "-"))[c(2,3,1)]), collapse = "_"),
                          ".csv")) %>% 
  filter(D1 == 1) %>%
  filter(!canceled, !postponed) %>% 
  group_by(team) %>%
  summarise("n_win" = sum(teamscore > oppscore, na.rm = T) + sum(is.na(teamscore)),
            "n_loss" = sum(teamscore < oppscore, na.rm = T)) %>%
  mutate("conf_wins" = 0,
         "conf_losses" = 0) %>%
  inner_join(select(confs, team, conference)) %>%
  rename("team_conf" = conference) %>%
  ungroup()

non_d1_actual <- read_csv(paste0("3.0_Files/Results/2023-24/NCAA_Hoops_Results_",
                                 paste(gsub("^0", "", unlist(strsplit(as.character(max(history$date)), "-"))[c(2,3,1)]), collapse = "_"),
                                 ".csv")) %>% 
  filter(D1 == 1) %>%
  filter(!is.na(teamscore)) %>% 
  filter(!canceled, !postponed) %>% 
  group_by(team) %>%
  summarise("n_win" = sum(teamscore > oppscore, na.rm = T) ,
            "n_loss" = sum(teamscore < oppscore, na.rm = T)) %>%
  mutate("conf_wins" = 0,
         "conf_losses" = 0) %>%
  inner_join(select(confs, team, conference)) %>%
  rename("team_conf" = conference) %>%
  ungroup()


conf_projections <- 
  bind_rows(records, non_d1) %>%
  group_by(team, team_conf) %>%
  summarise("n_win" = sum(n_win),
            "n_loss" = sum(n_loss),
            "conf_wins" = sum(conf_wins),
            "conf_losses" = sum(conf_losses)) %>%
  ungroup()

win_totals <- 
  bind_rows(records_actual, non_d1_actual) %>%
  group_by(team, team_conf) %>%
  summarise("n_win" = sum(n_win),
            "n_loss" = sum(n_loss),
            "conf_wins" = sum(conf_wins),
            "conf_losses" = sum(conf_losses)) %>%
  ungroup()



bracket <- read_csv("3.0_Files/Bracketology/bracket.csv")
bids <- read_csv("3.0_Files/Bracketology/bids.csv")    
bubble <- read_csv("3.0_Files/Bracketology/bubble.csv")
bubble$seed_overall <- 68 + 1:nrow(bubble)
bracket_math <- read_csv("3.0_Files/Bracketology/bracket_math.csv") 
bracket_math[is.na(bracket_math)] <- 0


visualize_schedule <- function(conf) {
  conf_data <- read_csv(paste0('3.0_Files/schedule_data/', conf, '.csv'))
  conf_data <- 
    conf_data %>% 
    inner_join(df_img, by = c('opponent' = 'team'))
  
  if(nrow(conf_data) == 0) {
    NULL 
  } else {
    
    colors <- gg_color_hue(5)
    conf_data <- 
      mutate(conf_data, "result"  = case_when(
        date == Sys.Date() & is.na(team_score) ~ "Today's Game",
        team_score > opp_score ~ "Win",
        postponed  ~ 'Postponed',
        canceled ~ 'Canceled',
        is.na(team_score) ~ "Future Game",
        T ~ "Loss"
      )) %>%
      mutate("result" = forcats::fct_relevel(result, "Win", "Loss", "Today's Game", 'Future Game',
                                             'Postponed', 'Canceled'))
    
    gc <- c()
    lwdc <- c()
    if(any(conf_data$result == "Win")) {
      gc <- c(gc, colors[3], colors[1])
      lwdc <- c(lwdc, rep(1.4, 2))
    }
    if(any(conf_data$result == "Today's Game")) {
      gc <- c(gc, colors[5]) 
      lwdc <- c(lwdc, 1.4)
    }
    if(any(conf_data$result == "Future Game")) {
      gc <- c(gc, "grey") 
      lwdc <- c(lwdc, 0.5)
    }
    if(any(conf_data$result == "Postponed")) {
      gc <- c(gc, "Blue") 
      lwdc <- c(lwdc, 1.4)
    }
    if(any(conf_data$result == "Canceled")) {
      gc <- c(gc, "black") 
      lwdc <- c(lwdc, 1.4)
    }
    
    print('Making Plot')
    p <- ggplot(conf_data, aes(x = game_id, y = forcats::fct_reorder(team, desc(team)))) +
      geom_tile(aes(fill = paste(location, team), 
                    col = result, linewidth = result),
                alpha = 0.7, width = 0.9, height = 0.9) +
      geom_image(aes(image = logo_file)) +
      scale_fill_manual(values = replace_na(c(ncaahoopR::ncaa_colors$primary_color[ncaahoopR::ncaa_colors$conference == conf],
                                              rep("grey", sum(conf_data$location == "N")),
                                              rep("white", length(unique(conf_data$team)))), 
                                            'white')) +
      scale_color_manual(values = gc) +
      scale_linewidth_manual(values = lwdc) +
      theme_minimal() +
      theme(axis.title = element_text(size = 20),
            axis.text.y = element_text(size = 16),
            axis.text.x = element_blank(),
            panel.grid = element_blank(),
            plot.title = element_text(size = 24, hjust  = 0.5),
            plot.subtitle = element_text(size = 18, hjust = 0.5),
            strip.text = element_text(size = 14),
            plot.caption = element_text(size = 12, hjust = 0.5),
            legend.position = "none"
      ) +
      labs(x = "", 
           y = "",
           color = "",
           size = "",
           title = "Conference Schedule",
           subtitle = conf,
           caption = "Home = Colored Background | Away = White Background
         Green = Win | Red = Loss | Purple = Game Today |\nBlue = Postponed | Black = Cancelled")
    print('Done Plot')
    return(p)
  }
}


### Recreate ggoplot2 colors
### Copied from https://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}


make_table <- function(sim_results, table_region) {
  
  df <- 
    select(ncaa_colors, 'team' = ncaa_name, logo_url) %>% 
    inner_join(sim_results) 
  
  if(table_region != 'all') {
    df <- filter(df, region == table_region) 
  }
  
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
  
  df %>% 
    gt() %>% 
    
    ### Ratings 
    data_color(
      columns = c(rating),
      fn = scales::col_numeric(
        palette = ggsci::rgb_material('amber', n = 68),
        domain = c(min(sim_results$rating), max(sim_results$rating)),
      )
    ) %>% 
    fmt_number(
      columns = c(rating),
      decimals = 1
    ) %>% 
    
    ### Tournament Odds 
    data_color(
      columns = c(first_round, second_round,
                  sweet_sixteen, elite_eight,
                  final_four, championship_game,
                  champ),
      fn = scales::col_numeric(
        palette = ggsci::rgb_material('amber', n = 68),
        domain = c(0,1),
      )
    ) %>% 
    
    fmt_percent(
      columns = c(first_round, second_round,
                  sweet_sixteen, elite_eight,
                  final_four, championship_game,
                  champ),
      decimals = 1) %>% 
    
    ### Align Columns
    cols_align(
      align = "center",
      columns = any_of(names(df))
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
      locations = cells_body(c(logo_url)),
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
    tab_source_note("Rating: Points relative to baseline NCAA team on neutral floor") %>% 
    tab_source_note("Table: Luke Benz (@recspecs730) | https://lbenz730.shinyapps.io/recspecs_basketball_central/") %>%
    tab_header(
      title = md("**2023 NCAA Men's Basketball Tournament Odds**"),
      subtitle = md(ifelse(table_region == 'all', '', paste0('**', table_region, " Region**")))
    ) %>% 
    tab_options(column_labels.font.size = 20,
                heading.title.font.size = 40,
                heading.subtitle.font.size = 30,
                heading.title.font.weight = 'bold',
                heading.subtitle.font.weight = 'bold'
    )
}

### Ivy League
ivy_history <- 
  read_csv('3.0_Files/Predictions/playoff_history.csv') %>% 
  filter(date >= '2023-11-01')
ivy_playoffs <- read_csv('3.0_Files/Predictions/playoffs.csv')

ivy_gt <- 
  ivy_playoffs %>% 
  
  mutate_if(is.numeric, ~.x/100) %>% 
  inner_join(df_img, by = 'team') %>% 
  inner_join(select(records_actual, team, conf_wins, conf_losses)) %>% 
  mutate('record' = paste0(' (', conf_wins, '-', conf_losses, ')')) %>% 
  select(-conf_wins, -conf_losses) %>% 
  arrange(desc(playoff_prob), desc(seed1_prob), desc(seed2_prob), desc(seed3_prob), desc(seed4_prob)) %>% 
  select(team, logo_file, record, everything()) %>% 
  
  gt() %>% 
  cols_label('team' = '', 
             'logo_file' = '',
             'record' = '',
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
        columns = c(record)
      )
    )
  ) %>% 
  text_transform(
    locations = cells_body(c(logo_file)),
    fn = function(x) {
      local_image(
        filename  = x,
        height = 30
      )
    }
  ) %>% 
  tab_source_note("2024 Tournament hosted by Columbia University") %>%
  tab_source_note("Based on 5,000 Simulations. Ties broken according to official Ivy League tiebreaking rules.") %>%
  
  tab_source_note("Table: Luke Benz (@recspecs730) | https://lbenz730.shinyapps.io/recspecs_basketball_central/") %>%
  tab_header(
    title = md("**2024 Ivy League Men's Basketball Tournament Odds**"),
    subtitle = md("<img src='https://content.sportslogos.net/logos/153/4824/full/ivy_league_logo_primary_2019_sportslogosnet-9024.png' style='height: 50px; width: auto; vertical-align: middle;'> ")
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
  ggplot(ivy_history %>% filter(date >= as.Date('2023-11-22')), aes(x = as.Date(date), y = playoff_prob)) + 
  geom_line(aes(group = team, col = team), size = 1.5) +
  facet_wrap(~team, ncol = 4) +
  theme_bw() + 
  labs(x = "Date", 
       y = "Playoff Probability",
       title = "Ivy League Playoff Odds",
       subtitle = "2023-2024") +
  theme(legend.position = "none",
        axis.title = element_text(size = 18),
        plot.title = element_text(size = 24, hjust = 0.5),
        plot.subtitle = element_text(size = 18, hjust = 0.5)) + 
  scale_y_continuous(labels = function(x) {paste0(x, "%")}, limits = c(0,100)) +
  scale_color_manual(values = cols)

ivy_sim <- 
  read_csv(paste0("3.0_Files/Predictions/conf_sims/Ivy.csv")) %>% 
  select(sim, team, n_wins, place)

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
  inner_join(df_img, by = c('home' = 'team')) %>%
  inner_join(df_img, by = c('away' = 'team'), suffix = c('_home', '_away')) %>%
  mutate_if(is.numeric, ~{.x/100}) %>%
  inner_join(x, by = c('home' = 'team',
                       'away' = 'opponent',
                       'date' = 'date')) %>%
  mutate('home_bar' = paste0('3.0_Files/Predictions/psf_figures/home_', 1:nrow(.), '.png'),
         'away_bar' = paste0('3.0_Files/Predictions/psf_figures/away_', 1:nrow(.), '.png'),
         'delta_bar' = paste0('3.0_Files/Predictions/psf_figures/delta_', 1:nrow(.), '.png')) %>%
  mutate('favored' = ifelse(pred_score_diff > 0, logo_file_home, logo_file_away),
         'win_prob' = ifelse(pred_score_diff > 0, wins, 1-wins),
         'pred_score' = ifelse(pred_score_diff > 0,
                               paste(sprintf('%0.1f', pred_team_score), sprintf('%0.1f', pred_opp_score), sep = '-'),
                               paste(sprintf('%0.1f', pred_opp_score), sprintf('%0.1f', pred_team_score), sep = '-'))) %>%
  select(date, logo_file_away, logo_file_home, favored, pred_score, win_prob,
         psf, auto_bid_sf, away_bar, home_bar, delta_bar) %>%
  arrange(date) %>%
  # filter(date == Sys.Date()) %>%
  gt() %>%
  cols_label('date' = 'Date',
             'logo_file_home' = 'Home',
             'logo_file_away' = 'Away',
             'favored' = 'Winner',
             'pred_score' = 'Score',
             'win_prob' = 'Win Probability',
             'psf' = 'Playoffs',
             'auto_bid_sf' = 'Auto Bid',
             'home_bar' = 'If Home Wins',
             'away_bar' = 'If Away Wins',
             'delta_bar' = 'Difference') %>%
  
  tab_spanner(label = 'Matchup', columns = c('date', 'logo_file_away', 'logo_file_home')) %>%
  tab_spanner(label = 'Game Prediction', columns = c('favored', 'pred_score', 'win_prob')) %>%
  tab_spanner(label = 'Leverage', columns = c('psf', 'auto_bid_sf')) %>%
  tab_spanner(label = 'Playoff Odds', columns = c('away_bar', 'home_bar', 'delta_bar')) %>%
  
  
  ### Hightlight Columns
  data_color(
    columns = c(auto_bid_sf, psf, win_prob),
    colors = scales::col_numeric(
      palette = ggsci::rgb_material('amber', n = 100),
      domain = c(0,1.25),
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
        columns = c(logo_file_home, auto_bid_sf, home_bar, win_prob, away_bar, delta_bar)
      )
    )
  ) %>%
  text_transform(
    locations = cells_body(c(logo_file_home, logo_file_away, favored)),
    fn = function(x) {
      local_image(
        filename  = x,
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
  tab_source_note("2024 Tournament hosted by Columbia University") %>%
  tab_source_note("Based on 1,000 Simulations of each outcome. Ties broken according to official Ivy League tiebreaking rules.") %>%
  tab_header(title = paste0(
    'Ivy League Playoff Leverage: ',
    min(ivy_psf$date),
    ifelse(n_distinct(ivy_psf$date) > 1, paste(' -', max(ivy_psf$date)), ''),
    ''
  )) %>%
  tab_options(column_labels.font.size = 20,
              heading.title.font.size = 40,
              heading.subtitle.font.size = 30,
              heading.title.font.weight = 'bold',
              heading.subtitle.font.weight = 'bold'
  )


### Bracketology
bracket_odds <-
  map_dfr(dir('3.0_Files/Predictions/conf_sims_ncaa/', full.names = T), read_csv) %>%
  filter(!is.na(team)) %>%
  right_join(bracket_math, by = 'team') %>%
  select(team, 'auto_bid' = freq, 'at_large' = odds) %>%
  mutate('at_large' = at_large/100) %>%
  mutate('overall' = auto_bid + (1-auto_bid) * at_large)


make_standings_plot <- function(conf) {
  standings <- read_csv(paste0("3.0_Files/Predictions/conf_sims/", conf, "_standings.csv"))
  sims <- read_csv(paste0("3.0_Files/Predictions/conf_sims/", conf, ".csv"))
  
  if(nrow(sims) < 10) {
    p <- NULL
  } else{
    
    
    
    sims$team <- as.factor(sims$team)
    sims$team <- reorder(sims$team, rep(standings$rank, 1000))
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
  print('Done Plot')
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
  scale_x_continuous(breaks = 0:max(df_wins$n_wins)) +
  scale_y_continuous(labels = scales::percent) + 
  scale_color_manual(values = cols) + 
  theme(legend.position = 'none',
        strip.text = element_text(size = 12)) + 
  labs(x = '# of Conference Wins',
       y = 'Probability of Event w/ # of Wins',
       title = 'Ivy Madness Playoff Snapshot',
       subtitle = 'Tiebreaking Scenarios Applied',
       color = '') 

x <- 
  x %>% 
  select(date, team, conf_game, rank, 
         team_conf, reg_season, opponent, 
         opp_rank, location, team_score, 
         opp_score, pred_team_score, 
         pred_score_diff,
         pred_opp_score, wins, canceled, postponed)

rm(ivy_history)
gc()


ivy_4th <- 
  df_wins %>% 
  filter(place == 4) %>% 
  group_by(n_wins) %>% 
  summarise('n' = n(), 
            tiebreak = mean(tiebreak)) %>% 
  ungroup() %>%
  mutate('pct' = n/sum(n)) %>% 
  select(n_wins, pct,  tiebreak) %>% 
  gt() %>% 
  cols_align('center') %>% 
  fmt_percent(c(pct, tiebreak), decimals = 1) %>% 
  cols_label(n_wins = '# of Wins for 4th Seed', 
             pct = 'Frequency', 
             tiebreak = 'Tiebreak for 4th Seed') %>% 
  tab_header(subtitle = md('**Distribution of Wins Need for Last Ivy Playoff Spot**'),
             title = md("<img src='https://content.sportslogos.net/logos/153/4824/full/ivy_league_logo_primary_2019_sportslogosnet-9024.png' style='height: 100px; width: auto; vertical-align: middle;'> ")) %>% 
  tab_options(column_labels.font.size = 16,
              column_labels.font.weight = 'bold',
              heading.title.font.size = 30,
              heading.subtitle.font.size = 20,
              heading.title.font.weight = 'bold',
              heading.subtitle.font.weight = 'bold'
  ) 

df <- 
  x %>% 
  filter(team_conf == 'Ivy') %>% 
  filter(conf_game) %>% 
  group_by(team, opponent) %>% 
  summarise('n_win' = sum(team_score > opp_score, na.rm = T),
            'n_loss' = sum(team_score < opp_score, na.rm = T)) %>% 
  mutate('record' = paste0(n_win, '-', n_loss)) %>% 
  select(team, opponent, record) %>% 
  ungroup() %>% 
  gt_cbb_teams(team, team, logo_height = 50, include_name = F) %>% 
  gt_cbb_teams(opponent, opponent, logo_height = 50, include_name = F) %>% 
  pivot_wider(names_from = opponent,
              values_from = record) %>% 
  select(1, 9, everything()) 

tab <- 
  gt(df) %>% 
  cols_align('center') %>% 
  fmt_markdown('team') %>% 
  fmt_missing(missing_text = '---') %>% 
  cols_label(team = '') %>% 
  cols_label_with(columns = 2:9, fn = md) %>% 
  tab_header(subtitle = md('**Head-to-Head Win Matrix**'),
             title = md("<img src='https://content.sportslogos.net/logos/153/4824/full/ivy_league_logo_primary_2019_sportslogosnet-9024.png' style='height: 100px; width: auto; vertical-align: middle;'> ")) %>% 
  tab_options(column_labels.font.size = 16,
              column_labels.font.weight = 'bold',
              heading.title.font.size = 30,
              heading.subtitle.font.size = 40,
              heading.title.font.weight = 'bold',
              heading.subtitle.font.weight = 'bold') 


for(j in 2:9) {
  for(i in 1:8) {
    if(!is.na(df[i,j]) & df[i,j] == '0-1') {
      tab <- 
        tab %>% 
        tab_style(style = cell_fill(color = 'pink'),
                  locations = cells_body(columns = j, 
                                         rows = i)
        )
    } else if(!is.na(df[i,j]) & df[i,j] == '0-2') {
      tab <- 
        tab %>% 
        tab_style(style = cell_fill(color = 'darkred'),
                  locations = cells_body(columns = j, 
                                         rows = i)
        )
    } else if(!is.na(df[i,j]) & df[i,j] == '1-0') {
      tab <- 
        tab %>% 
        tab_style(style = cell_fill(color = 'lightgreen'),
                  locations = cells_body(columns = j, 
                                         rows = i)
        )
    } else if(!is.na(df[i,j]) & df[i,j] == '2-0') {
      tab <- 
        tab %>% 
        tab_style(style = cell_fill(color = 'darkgreen'),
                  locations = cells_body(columns = j, 
                                         rows = i)
        )
    } else if(!is.na(df[i,j]) & df[i,j] == '1-1') {
      tab <- 
        tab %>% 
        tab_style(style = cell_fill(color = 'lightyellow'),
                  locations = cells_body(columns = j, 
                                         rows = i)
        )
    }
    
    
    
    
    
  }
}

