library(ggplot2)
library(ggridges)
library(viridis)
library(knitr)
library(kableExtra)
library(tidyr)
library(ncaahoopR)


### get team's conference
get_conf <- function(team) {
  return(confs$conference[confs$team == team])
}

### Visualizations
yusag_plot <- function(data){
  data %>% mutate(group = reorder(conference, yusag_coeff, median)) %>%
    ggplot(aes(x = yusag_coeff, y = group, fill = ..x..)) + 
    geom_density_ridges_gradient(scale = 1.5) + theme_ridges() +
    scale_y_discrete(expand = c(0.01, 0)) + 
    scale_x_continuous(expand = c(0.01, 0)) +
    scale_fill_viridis(name = "group", option = "C") +
    labs(y = "Conference", 
         x = "Points Above Average Team",
         title = "NCAA Men's Basketball Power Rankings") + 
    theme(plot.title = element_text(size = 16, hjust = 0.5),
          axis.title.y = element_text(size = 14, hjust = 0.5),
          axis.title.x = element_text(size = 14, hjust = 0.5),
          legend.position="none")
  
}

box_plot <- function(data) {
  data %>% mutate(group = reorder(conference, yusag_coeff, median)) %>%
    ggplot(aes(y = yusag_coeff, x = group)) + 
    geom_point(alpha = 0.2) + 
    geom_boxplot(fill  = "orange", alpha = 0.2) +
    geom_hline(yintercept = 0, lty = 2, size = 1.2, col = "orange") + 
    labs(x = "Conference", 
         y = "Points Above Average Team",
         title = "NCAA Men's Basketball Power Rankings") + 
    theme_bw() + 
    theme(plot.title = element_text(size = 16, hjust = 0.5),
          axis.title.y = element_text(size = 14, hjust = 0.5),
          axis.title.x = element_text(size = 14, hjust = 0.5),
          axis.text.x = element_text(angle = 90),
          legend.position="none")
}

#### Conference Sims
conf_sim <- function(conf, nsims) {
  conf_teams <- filter(confs, conference == conf) %>% 
    pull(team) %>% 
    unique()
  results <- data.frame("team" = conf_teams,
                        "shared_title" = rep(0, length(conf_teams)),
                        "sole_title" = rep(0, length(conf_teams)),
                        "avg_wins" = rep(0, length(conf_teams)),
                        "avg_losses" = rep(0, length(conf_teams)),
                        stringsAsFactors = F)
  
  ### Sim Schedule
  schedule <- filter(x, conf_game, team_conf == conf, location != "V") %>%
    mutate(simwins = 0, opp_simwins = 0)
  schedule$tmp <- case_when(
    schedule$team < schedule$opponent ~ paste(schedule$team, schedule$opponent, schedule$date),
    T ~ paste(schedule$opponent, schedule$team, schedule$date)
  )
  schedule <- filter(schedule, !duplicated(tmp))
  
  
  
  sim_season <- rep(0, nrow(results))
  max_wins <- nrow(schedule) * 2 / nrow(results)
  
  for(i in 1:nsims) {
    if(i %% 100 == 0) {
      cat("Sim:", i, "\n")
    }
    rands <- runif(nrow(schedule))
    schedule$simwins <- ifelse(rands <= schedule$wins, 1, 0)
    schedule$opp_simwins <- abs(1 - schedule$simwins)
    for(j in 1:nrow(results)) {
      sim_season[j] <- sum(schedule$simwins[schedule$team == conf_teams[j]]) +
        sum(schedule$opp_simwins[schedule$opponent == conf_teams[j]])
    }
    
    results$avg_wins <- results$avg_wins + sim_season/nsims
    results$avg_losses <- results$avg_losses + (max_wins - sim_season)/nsims
    winners <- grep(max(sim_season), sim_season)
    results$shared_title[winners] <- results$shared_title[winners] + 1/nsims
    if(length(winners) == 1) {
      results$sole_title[winners] <- results$sole_title[winners] + 1/nsims
    }
  }
  return(results)
}

#### Conference Sims
conf_fast_sim <- function(conf, nsims) {
  ### Sim Schedule
  schedule <- 
    x %>% 
    filter(conf_game, team_conf == conf, location != "V") %>%
    mutate(simwins = 0, opp_simwins = 0)
  

  schedule$tmp <- 
    case_when(
      schedule$team < schedule$opponent ~ paste(schedule$team, schedule$opponent, schedule$date),
      T ~ paste(schedule$opponent, schedule$team, schedule$date)
    )
  schedule <- filter(schedule, !duplicated(tmp))
  
  schedule <- bind_rows(replicate(nsims, schedule, simplify = F))
  
  if(nrow(schedule) == 0) {
    return(tibble('team' =  '',
                  'sim' = '',
                  'n_wins' = '',
                  'place' = ''))
  }
  
  results <- 
    schedule %>% 
    group_by(team, opponent, date) %>% 
    mutate('sim' = 1:n()) %>% 
    ungroup() %>% 
    mutate('rand' = runif(nrow(.))) %>% 
    mutate('winner' = case_when(rand <= wins ~ team,
                                T ~ opponent)) %>% 
    select(sim, team, opponent, winner) %>% 
    pivot_longer(cols = c('team', 'opponent'),
                 values_to = 'team') %>% 
    group_by(sim, team) %>% 
    summarise('n_wins' = sum(winner == team)) %>% 
    group_by(sim) %>% 
    mutate('place' = rank(-n_wins, ties = "min"))
  
  
  return(results)
}

### Jerome EP Calcultor
jerome <- function(t_sim) {
  v <- 3 * t_sim$finals + 2 * (1 + c(0, 0, rep(1, nrow(t_sim) - 2))) * t_sim$champ/t_sim$finals
  names(v) <- t_sim$team
  return(v)
}

### eliminate teams from AutoBid Contention
eliminate <- function(teams, confs) {
  for(team in teams) {
    confs$eliminated[confs$team == team] <- T
  }
  write.csv(confs, "3.0_Files/Info/conferences.csv", row.names = F)
  return(confs)
}

### find regular season
reg_season <- function(date, conf) {
  
}

### Compute Weights for Pre-season prior
prior_weight <- function(school) {
  
  num <- max(c(0, 
               filter(x, team == school, !is.na(score_diff)) %>% 
                 pull(game_id) %>%
                 max()), na.rm = T)
  denom <- 
    max(c(1, 
          filter(x, team == school) %>% 
            pull(game_id) %>%
            max()), na.rm = T)
  w <- 0.25 * num/32
  
  
  w <- min(c(w, 1))
  return(w)
}



rank_plot <- function() {
  ggplot(history, aes(x = as.Date(date), y = rank, group = team, col = team)) + 
    facet_wrap(~conference, ncol = 8) +
    geom_line() + 
    theme_bw() + 
    theme(plot.title = element_text(size = 16, hjust = 0.5),
          axis.title.y = element_text(size = 14, hjust = 0.5),
          axis.title.x = element_text(size = 14, hjust = 0.5),
          axis.text.x = element_text(angle = 90, size = 6),
          legend.position = "none") + 
    labs(x = "Date", 
         y = "D1 Rank",
         title = "Evolution of the NCAA Basketball Universe") + 
    scale_y_reverse()
}


evo_plot <- function() {
  ggplot(history, aes(x = as.Date(date), y = yusag_coeff)) + 
    facet_wrap(~conference, ncol = 8) +
    geom_line(aes(group = team, col = team)) + 
    theme_bw() + 
    theme(plot.title = element_text(size = 16, hjust = 0.5),
          axis.title.y = element_text(size = 14, hjust = 0.5),
          axis.title.x = element_text(size = 14, hjust = 0.5),
          axis.text.x = element_text(angle = 90, size = 6),
          legend.position = "none") + 
    labs(x = "Date", 
         y = "Points Above Average Team",
         title = "Evolution of the NCAA Basketball Universe")
}


max_date <- function(date, hist_dates) {
  return(max(hist_dates[hist_dates <= date]))
}


ivy_preds <- function() {
  filter(x, conf_game, 
         team_conf == "Ivy", 
         date > Sys.Date(), date <= Sys.Date() + 2, 
         location == "H") %>% 
    select(date, team, opponent, location, pred_team_score, 
           pred_opp_score, pred_score_diff, pred_total_score) %>%
    rbind(read.csv("3.0_Files/Predictions/ivy_predictions.csv", as.is = T) %>%
            mutate("date" = as.Date(date)) %>%
            filter(date <= Sys.Date())) %>%
    filter(!duplicated(paste(date, team, opponent))) %>%
    write.csv("3.0_Files/Predictions/ivy_predictions.csv", row.names = F)
}

####### Ivy League Graphics
playoff_graphic <- function() {
  background_colors <- arrange(playoffs, desc(playoff_prob), desc(seed1_prob)) %>%
    pull(team) %>%
    sapply(., function(x) { ncaa_colors$primary_color[ncaa_colors$ncaa_name == x] })
  text_colors <- arrange(playoffs, desc(playoff_prob), desc(seed1_prob)) %>%
    pull(team) %>%
    sapply(., function(x) { ncaa_colors$secondary_color[ncaa_colors$ncaa_name == x] })
  text_colors[c("Brown", "Dartmouth", "Cornell", "Harvard")] <- "#FFFFFF"
  tmp <- text_colors["Penn"]
  text_colors["Penn"] <- "red"
  background_colors["Penn"] <- tmp
  
  mutate(playoffs, auto_bid = case_when(
    auto_bid > 0.05 ~ round(auto_bid, 1),
    playoff_prob > 0 ~ 0.1,
    T ~ 0),
    seed1_prob = 
      case_when(
        seed1_prob > 0.05 ~ round(seed1_prob, 1),
        seed1_prob > 0 ~ 0.1,
        T ~ 0),
    seed2_prob = 
      case_when(
        seed2_prob > 0.05 ~ round(seed2_prob, 1),
        seed2_prob > 0 ~ 0.1,
        T ~ 0),
    seed3_prob = 
      case_when(
        seed3_prob > 0.05 ~ round(seed3_prob, 1),
        seed3_prob > 0 ~ 0.1,
        T ~ 0),
    seed4_prob = 
      case_when(
        seed4_prob > 0.05 ~ round(seed4_prob, 1),
        seed4_prob > 0 ~ 0.1,
        T ~ 0)
  ) %>%
    arrange(desc(playoff_prob), desc(seed1_prob)) %>%
    mutate(playoff_prob = 
             case_when(
               playoff_prob > 99.9 & playoff_prob < 100 ~ 99.9,
               playoff_prob > 0.05 ~ round(playoff_prob, 1),
               playoff_prob > 0 ~ 0.1,
               T ~ 0)) %>%
    rename("Team" = team,
           "Auto Bid" = auto_bid,
           "Playoff Probability" = playoff_prob,
           "1st Seed" = seed1_prob,
           "2nd Seed" = seed2_prob,
           "3rd Seed" = seed3_prob,
           "4th Seed" = seed4_prob) %>%
    kable(., align = "ccccccc") %>%
    kable_styling("striped", full_width = F, position = "center") %>%
    row_spec(1, color = text_colors[1], background = background_colors[1], bold = T) %>%
    row_spec(2, color = text_colors[2], background = background_colors[2], bold = T) %>%
    row_spec(3, color = text_colors[3], background = background_colors[3], bold = T) %>%
    row_spec(4, color = text_colors[4], background = background_colors[4], bold = T) %>%
    row_spec(5, color = text_colors[5], background = background_colors[5], bold = T) %>%
    row_spec(6, color = text_colors[6], background = background_colors[6], bold = T) %>%
    row_spec(7, color = text_colors[7], background = background_colors[7], bold = T) %>%
    row_spec(8, color = text_colors[8], background = background_colors[8], bold = T) %>%
    row_spec(0, bold = T, font_size = 16) %>%
    add_header_above(c("Ivy League Men's Basketball Playoff Odds" = 7), bold = T)
}

psf_graphic <- function() {
  background_colors <- arrange(playoffs, desc(auto_bid), desc(playoff_prob), desc(seed1_prob)) %>%
    pull(team) %>%
    sapply(., function(x) { ncaa_colors$primary_color[ncaa_colors$ncaa_name == x] })
  text_colors <- arrange(playoffs, desc(auto_bid), desc(playoff_prob), desc(seed1_prob)) %>%
    pull(team) %>%
    sapply(., function(x) { ncaa_colors$secondary_color[ncaa_colors$ncaa_name == x] })
  text_colors[c("Brown", "Dartmouth", "Cornell", "Harvard")] <- "#FFFFFF"
  tmp <- text_colors["Penn"]
  text_colors["Penn"] <- "red"
  background_colors["Penn"] <- tmp
  
  psf_results <- arrange(psf_results, desc(psf)) %>% 
    mutate(auto_bid_sf = sprintf("%.1f", auto_bid_sf),
           psf = sprintf("%.1f", psf))
  
  inner_join(psf_results, select(x, team, opponent, pred_team_score, date,
                                 pred_opp_score, wins), by = c("home" = "team",
                                                               "away" = "opponent",
                                                               "date" = "date")) %>%
    mutate("winner" = ifelse(pred_team_score > pred_opp_score, home, away),
           "result" = case_when(
             pred_team_score > pred_opp_score ~ paste0(home, ": ", sprintf("%.1f", pred_team_score), " - ",
                                                       sprintf("%.1f", pred_opp_score), " (", sprintf("%.1f", 100 * wins), "%)"),
             pred_opp_score > pred_team_score ~ paste0(away, ": ", sprintf("%.1f", pred_opp_score), " - ",
                                                       sprintf("%.1f", pred_team_score), " (", sprintf("%.1f", 100 * (1 - wins)), "%)")
           )
    ) %>%
    select(home, away, result, psf, auto_bid_sf, winner) %>%
    mutate(home = cell_spec(home, 
                            color = text_colors[home], 
                            background = background_colors[home],
                            bold = T),
           away = cell_spec(away, 
                            color = text_colors[away], 
                            background = background_colors[away],
                            bold = T),
           result = 
             cell_spec(result, 
                       color = text_colors[winner], 
                       background = background_colors[winner],
                       bold = T)
    ) %>%
    mutate(psf = cell_spec(
      psf, color = "white", bold = T,
      background = spec_color(as.numeric(psf), end = 0.9, option = "C", direction = 1)
    )) %>%
    mutate(auto_bid_sf = cell_spec(
      auto_bid_sf, color = "white", bold = T,
      background = spec_color(as.numeric(auto_bid_sf), end = 0.9, option = "C", direction = 1)
    )) %>%
    select(-winner) %>%
    rename("Home" = home,
           "Away" = away,
           "Predicted Result" = result,
           "Playoff Swing Factor" = psf,
           "Auto Bid Swing Factor" = auto_bid_sf) %>%
    kable(., escape = F, align = "ccccccc") %>%
    kable_styling("striped", full_width = F, position = "center") %>%
    row_spec(0, bold = T, font_size = 16) %>%
    add_header_above(c("Ivy League Men's Basketball Predictions" = 5), bold = T)
}





