library(dplyr) 
library(readr)
library(ggplot2)
library(ggrepel)

glm.pointspread <- readRDS("glm_pointspread.rds")

theme_set(theme_bw() +
            theme(axis.title = element_text(size = 14),
                  plot.title = element_text(size = 16, hjust  = 0.5),
                  plot.subtitle = element_text(size = 12, hjust = 0.5),
                  legend.title = element_text(size = 8, hjust = 0.5),
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

records <- group_by(x, team, team_conf) %>%
  summarise("n_win" = sum(wins),
            "n_loss" = sum(1-wins),
            "conf_wins" = sum(wins[conf_game]),
            "conf_losses" = sum(1 - wins[conf_game])) %>%
  ungroup()




non_d1 <- read_csv(paste0("3.0_Files/Results/2019-20/NCAA_Hoops_Results_",
                          paste(gsub("^0", "", unlist(strsplit(as.character(max(history$date)), "-"))[c(2,3,1)]), collapse = "_"),
                          ".csv")) %>% 
  filter(D1 == 1) %>%
  group_by(team) %>%
  summarise("n_win" = sum(teamscore > oppscore, na.rm = T) + sum(is.na(teamscore)),
            "n_loss" = sum(teamscore < oppscore, na.rm = T)) %>%
  mutate("conf_wins" = 0,
         "conf_losses" = 0) %>%
  inner_join(select(confs, team, conference)) %>%
  rename("team_conf" = conference) %>%
  ungroup()


conf_projections <- bind_rows(records, non_d1) %>%
  group_by(team, team_conf) %>%
  summarise("n_win" = sum(n_win),
            "n_loss" = sum(n_loss),
            "conf_wins" = sum(conf_wins),
            "conf_losses" = sum(conf_losses)) %>%
  ungroup()








    
    
    
