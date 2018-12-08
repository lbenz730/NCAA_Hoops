library(ggplot2)
library(ggridges)
library(viridis)

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
                        "avg_losses" = rep(0, length(conf_teams)))
  
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

### Jerome EP Calcultor
jerome <- function(t_sim) {
  return(3 * t_sim$finals + 2 * (1 + c(0, 0, rep(1, nrow(t_sim) - 2))) * t_sim$champ/t_sim$finals)
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
  w <- 1.85 * max(c(0, filter(x, team == school, !is.na(score_diff)) %>% 
                      pull(game_id) %>%
                      max()), 
                  na.rm = T)/(
                    max(c(1, filter(x, team == school) %>% 
                            pull(game_id) %>%
                            max()), na.rm = T)
                  )
  
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
  ggplot(history, aes(x = as.Date(date), y = yusag_coeff, group = team, col = team)) + 
    facet_wrap(~conference, ncol = 8) +
    geom_line() + 
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
