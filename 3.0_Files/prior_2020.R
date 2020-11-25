library(tidyverse)
library(XML)
library(knitr)
library(kableExtra)

### Compute Stats 2020
luke <- read_csv("Info/luke2021.csv")
sports247 <- read_csv("Info/247_rankings_2020.csv")
yusag_2020 <- read_csv("Info/2020_yusag_coefficients.csv")
mins_2020 <- read_csv("Info/mins_2021.csv")
dict <- ncaahoopR::dict

stats_2016 <- read_csv("Info/stats_2016.csv") %>%
  select(-transfer_ppg, -net_ppg)
stats_2017 <- read_csv("Info/stats_2017.csv") %>%
  select(-transfer_ppg, -net_ppg)
stats_2018 <- read_csv("Info/stats_2018.csv") %>%
  select(-transfer_ppg, -net_ppg)
stats_2019 <- read_csv("Info/stats_2019.csv")

stats_2020 <- 
  select(dict, NCAA, Trank, name_247) %>%
  inner_join(luke, by = c("Trank" = "Team")) %>%
  left_join(sports247, by = c("name_247" = "team")) %>%
  inner_join(yusag_2020, by = c("NCAA" = "team")) %>%
  left_join(mins_2020, by = c("Trank" = "team")) %>%
  select(-Trank, -name_247, -conf, -conf_rank, -conference) %>%
  rename("team" = NCAA,
         "returning_ppg" = Returning,
         "departing_ppg" = Departing) %>%
  select(names(stats_2016))

write_csv(stats_2020, "Info/stats_2020.csv")

train <- bind_rows(
  inner_join(stats_2016, select(stats_2017, team, yusag_coeff, off_coeff, def_coeff),
             by = "team", suffix = c("", "_next")),
  inner_join(stats_2017, select(stats_2018, team, yusag_coeff, off_coeff, def_coeff),
             by = "team", suffix = c("", "_next")),
  inner_join(stats_2018, select(stats_2019, team, yusag_coeff, off_coeff, def_coeff),
             by = "team", suffix = c("", "_next")),
  inner_join(stats_2019, select(stats_2020, team, yusag_coeff, off_coeff, def_coeff),
             by = "team", suffix = c("", "_next"))
)


lm.prior.o <- lm(off_coeff_next ~ off_coeff + composite_score +
                   departing_ppg + returning_ppg  + 
                   stars_5,
                 data = train)

lm.prior.d <- lm(def_coeff_next ~ def_coeff  + composite_score +
                   departing_ppg + returning_ppg,
                 data = train)

stats_2020 <- mutate_all(stats_2020, ~replace(., is.na(.), 0))
stats_2020$off_coeff_next <- predict(lm.prior.o, newdata = stats_2020)
stats_2020$def_coeff_next <- predict(lm.prior.d, newdata = stats_2020)
stats_2020$yusag_coeff_next <- stats_2020$off_coeff_next + stats_2020$def_coeff_next

trank <- jsonlite::fromJSON('https://barttorvik.com/2021_team_results.json') %>% 
  as_tibble() %>% 
  select(V2, V5, V7) %>%
  set_names('team', 'adj_oe', 'adj_de') %>% 
  mutate("adj_oe" = as.numeric(as.character(adj_oe)),
         "adj_de" = as.numeric(as.character(adj_de)),
         "team" = as.character(team))

df <- 
  inner_join(trank, dict, by = c("team" = "Trank")) %>%
  right_join(select(stats_2020, team, yusag_coeff_next, off_coeff_next, def_coeff_next),
             by = c("NCAA" = "team"))

lm_o <- lm(off_coeff_next ~ adj_oe, data = df)
lm_d <- lm(def_coeff_next ~ adj_de, data = df)



new <- c("Bellarmine", "Tarleton St.",
         "Dixie St.", "UC San Diego")
off_coeff_next <- predict(lm_o, filter(trank, team %in% new))
def_coeff_next <- predict(lm_d, filter(trank, team %in% new))
yusag_coeff_next <- off_coeff_next + def_coeff_next

stats_2020 <- bind_rows(stats_2020, 
                        tibble('team' = new,
                               'off_coeff_next' = off_coeff_next,
                               'def_coeff_next' = def_coeff_next,
                               'yusag_coeff_next' = yusag_coeff_next))

select(stats_2020, team, yusag_coeff_next, off_coeff_next, def_coeff_next) %>%
  rename_all(function(x) gsub("_next", "", x)) %>%
  write_csv("Info/prior.csv")

select(stats_2020, team, yusag_coeff_next, off_coeff_next, def_coeff_next) %>%
  rename_all(function(x) gsub("_next", "", x)) %>%
  mutate(off_coeff = round(off_coeff, 1),
         def_coeff = round(def_coeff, 1),
         yusag_coeff = off_coeff + def_coeff) %>%
  arrange(desc(yusag_coeff)) %>%
  head(25) %>%
  mutate("Rank" = 1:nrow(.)) %>%
  select(Rank, everything()) %>%
  rename("Team" = team,
         "Net Rating" = yusag_coeff,
         "Off. Rating" = off_coeff,
         "Def. Rating" = def_coeff) %>%
  kable(align = "c") %>%
  kable_styling(bootstrap_options = "striped", full_width = F, position = "center") %>%
  add_header_above(c("2019 NCAA Men's Basketball Preseason Ratings" = 5), bold = T, font_size = 24)

y2y <- select(train, off_coeff, off_coeff_next, def_coeff, def_coeff_next, stars_5, 
              stars_4, stars_3, num_star_players, mins, returning_ppg, departing_ppg,
              composite_score, avg_recruit_score) %>%
  mutate("row" = 1:nrow(.)) %>%
  gather(coefficient, value, -row, - stars_5, -stars_4, -stars_3,
         -num_star_players, -mins, -returning_ppg, -departing_ppg,
         -composite_score, -avg_recruit_score) %>%
  mutate("type" = ifelse(grepl("off_", coefficient), "Offense", "Defense"),
         "year" = ifelse(grepl("_next", coefficient), "year", "year_next")) %>%
  select(-coefficient) %>%
  pivot_wider(names_from = year, values_from = value) %>%
  mutate("delta" = year - year_next)

ggplot(y2y, aes(x = year, y = year_next)) +
  geom_point(alpha = 0.2) +
  facet_wrap(~type) + 
  geom_hline(yintercept = 0, lty = 2, col = "red") +
  geom_vline(xintercept = 0, lty = 2, col = "red") +
  geom_density_2d()

ggplot(y2y, aes(x = delta)) +
  geom_density(aes(fill = type), alpha = 0.3) +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 14)) +
  labs(x = "Change in Rating from Previous Year",
       y = "Density",
       fill = "Rating Type",
       title = "Distributions of Year to Year Change in Rating",
       subtitle = "NCAA Men's Basketball")

ggplot(y2y, aes(x = departing_ppg, y = delta)) +
  facet_wrap(~type) +
  geom_point(aes(col = mins), alpha = 0.4) +
  geom_smooth() +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 14)) +
  labs(y = "Change in Rating from Previous Year",
       x = "Departing PPG!",
       title = "Where Does Experience Matter in NCAA Men's Basketball",
       color = "Returning Possession Minutes",
       subtitle = "2015-16 through 2018-19",
       caption = "PPG! Courtsey of Bart Torvik (@totally_t_bomb)") +
  scale_color_viridis_c(labels = function(x) paste(100*x, "%"))

ggplot(train, aes(x = returning_ppg - departing_ppg, y =  yusag_coeff_next - yusag_coeff)) +
  geom_point(aes(color = mins)) +
  geom_smooth() +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 14)) +
  labs(y = "Change in Net Rating from Previous Year",
       x = "Departing PPG!",
       title = "Losing Good Players is Not Good",
       color = "Returning Possession Minutes",
       subtitle = "2015-16 through 2018-19",
       caption = "PPG! Courtsey of Bart Torvik (@totally_t_bomb)") +
  scale_color_viridis_c(labels = function(x) paste(100*x, "%"))

ggplot(filter(train, avg_recruit_score > 0), aes(x = avg_recruit_score, y =  yusag_coeff_next - yusag_coeff)) +
  geom_point(aes(color = mins)) +
  geom_smooth() +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 14)) +
  labs(y = "Change in Net Rating from Previous Year",
       x = "Avgerage Recruit Score",
       title = "Does a Good Recruiting Class Matter?",
       color = "Returning Possession Minutes",
       subtitle = "2015-16 through 2018-19",
       caption = "Recruiting Courtsey of 247 Sports") +
  scale_color_viridis_c(labels = function(x) paste(100*x, "%"))

