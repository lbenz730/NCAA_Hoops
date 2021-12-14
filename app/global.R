library(dplyr) 
library(readr)
library(ggplot2)
library(ggrepel)
library(gt)
library(paletteer)
library(ncaahoopR)

glm.pointspread <- readRDS("glm_pointspread.rds")
ncaa_sims <- read_csv('3.0_Files/ncaa_sims/ncaa_sims.csv')

theme_set(theme_bw() +
            theme(axis.title = element_text(size = 20),
                  axis.text = element_text(size = 12),
                  plot.title = element_text(size = 24, hjust  = 0.5),
                  plot.subtitle = element_text(size = 18, hjust = 0.5),
                  legend.title = element_text(size = 12, hjust = 0.5),
                  legend.position = "bottom"))

x <- read_csv("3.0_Files/Predictions/predictions.csv")
teams <- sort(unique(x$team))

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
  filter(!canceled, !postponed) %>% 
  group_by(team, team_conf) %>%
  summarise("n_win" = sum(wins),
            "n_loss" = sum(1-wins),
            "conf_wins" = sum(wins[conf_game]),
            "conf_losses" = sum(1 - wins[conf_game])) %>%
  ungroup()




non_d1 <- read_csv(paste0("3.0_Files/Results/2021-22/NCAA_Hoops_Results_",
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


conf_projections <- 
  bind_rows(records, non_d1) %>%
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
  conf_data <- filter(x, conf_game, team_conf == conf)
  if(nrow(conf_data) == 0) {
    NULL 
  } else {
    
    
    conf_data <- 
      conf_data %>% 
      inner_join(select(ncaahoopR::ncaa_colors, ncaa_name, logo_url), 
                 by = c("opponent" = "ncaa_name"))
    conf_data <- 
      conf_data %>% 
      group_by(team) %>% 
      mutate('game_id' = 1:n()) %>% 
      ungroup()
    
    colors <- gg_color_hue(5)
    conf_data <- mutate(conf_data, "result"  = case_when(
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
    
    ggplot(conf_data, aes(x = game_id, y = forcats::fct_reorder(team, desc(team)))) +
      geom_tile(aes(fill = paste(location, team), 
                    col = result, lwd = result),
                alpha = 0.7, width = 0.9, height = 0.9) +
      geom_image(aes(image = logo_url), size = 0.04) +
      scale_fill_manual(values = c(ncaahoopR::ncaa_colors$primary_color[ncaahoopR::ncaa_colors$conference == conf],
                                   rep("grey", sum(conf_data$location == "N")),
                                   rep("white", length(unique(conf_data$team))))) +
      scale_color_manual(values = gc) +
      scale_size_manual(values = lwdc) +
      theme_minimal() +
      theme(axis.title = element_text(size = 20),
            axis.text.y = element_text(size = 16),
            axis.text.x = element_blank(),
            plot.title = element_text(size = 24, hjust  = 0.5),
            plot.subtitle = element_text(size = 18, hjust = 0.5),
            strip.text = element_text(size = 14),
            plot.caption = element_text(size = 12, hjust = 0.5),
            legend.position = "none") +
      labs(x = "", 
           y = "",
           color = "",
           size = "",
           title = "Conference Schedule",
           subtitle = conf,
           caption = "Home = Colored Background | Away = White Background
         Green = Win | Red = Loss | Purple = Game Today |\nBlue = Postponed | Black = Cancelled")
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
             1 * (1 - first_round) + 
             2 * (1 - second_round - first_round) +
             3 * (1 - sweet_sixteen - second_round - first_round) + 
             4 * (1 - elite_eight - sweet_sixteen - second_round - first_round) +
             5 * (1 - final_four - elite_eight - sweet_sixteen - second_round - first_round) +
             6 * (1 -  championship_game - final_four - elite_eight - sweet_sixteen - second_round - first_round)) %>% 
    arrange(expected_elim_round) %>% 
    select(-expected_elim_round)
  
  df %>% 
    gt() %>% 
    
    ### Ratings 
    data_color(
      columns = c(rating),
      colors = scales::col_numeric(
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
      colors = scales::col_numeric(
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
    # tab_source_note("Table: Luke Benz (@recspecs730) | https://lbenz730.shinyapps.io/recspecs_basketball_central/") %>% 
    tab_header(
      title = md("**2021 NCAA Men's Basketball Tournament Odds**"),
      # subtitle = md(paste0('**', table_region, " Region**"))
    ) %>% 
    tab_options(column_labels.font.size = 20,
                heading.title.font.size = 40,
                heading.subtitle.font.size = 30,
                heading.title.font.weight = 'bold',
                heading.subtitle.font.weight = 'bold'
    )
}

ncaa_gt <- make_table(ncaa_sims, 'all')


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
  scale_x_continuous(breaks = min(ivy_sim$n_wins):max(ivy_sim$n_wins)) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = '# of Conference Wins',
       y = 'Frequency',
       fill = 'Place',
       title = 'Distribution of Conference Finish By # of Wins',
       subtitle = 'Ivy League: Tiebreakers Applied')


