library(dplyr) 
library(readr)
library(ggplot2)
library(ggrepel)

glm.pointspread <- readRDS("glm_pointspread.rds")

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




non_d1 <- read_csv(paste0("3.0_Files/Results/2020-21/NCAA_Hoops_Results_",
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
