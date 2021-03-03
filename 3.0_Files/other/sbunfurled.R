library(tidyverse)

x <- read_csv('https://www.masseyratings.com/cb/compare.csv') 
x <- read_csv('https://www.masseyratings.com/cb/compare.csv',
              skip = which(x$`College Basketball Ranking Comparison` == 'Team')+2) %>% 
  select('team' = Team,
         'win_loss' = WL,
         'trank' = TRK,
         'sagarin' = SAG,
         'bpi' = EBP,
         'kenpom' = POM) %>% 
  separate(win_loss, c('win', 'loss'), sep = '-') %>% 
  mutate_at(vars(-team), as.numeric) %>% 
  mutate('win_pct' = 100 * win/(win+loss),
         'avg' = (trank + sagarin + bpi + kenpom)/4)


trank <- XML::readHTMLTable(RCurl::getURL('https://www.barttorvik.com/tranketology.php'))
trank_cln <- 
  bind_rows(trank[[1]], trank[[2]] %>% set_names(names(trank[[1]]))) %>%
  filter(!is.na(Team)) %>% 
  select('team' = Team, 
         'bid_pct' = `Bid%`) %>% 
  mutate('team' = gsub('\\s+[0-9]+', '', gsub('\\**', '', team))) %>% 
  mutate('team' = gsub('\\s+F4O', '', team)) %>% 
  mutate('team' = gsub('St.$', 'St', team)) %>% 
  filter(!is.na(bid_pct)) %>% 
  filter(bid_pct > 1) %>% 
  distinct() %>% 
  mutate('team' = case_when(
    team == 'Saint Louis' ~ 'St Louis',
    team == 'Loyola Chicago' ~ 'Loyola-Chicago',
    team == 'St. Bonaventure' ~ 'St Bonaventure',
    team == 'North Carolina St' ~ 'NC State',
    team == 'Western Kentucky' ~ 'WKU',
    T ~ team
  ))

x <- inner_join(trank_cln, x)



bracket <- XML::readHTMLTable('http://www.bracketmatrix.com/')
bracket_cln <- 
  bracket[[1]] %>% 
  select('team' = V2,
         'seed_avg' = V4) %>% 
  mutate('seed_avg' = as.numeric(seed_avg),
         'team' = gsub('State', 'St', as.character(team))) %>% 
  filter(!is.na(seed_avg)) %>% 
  mutate('team' = case_when(
    team == 'Saint Louis' ~ 'St Louis',
    team == 'Loyola Chicago' ~ 'Loyola-Chicago',
    team == 'St. Bonaventure' ~ 'St Bonaventure',
    team == 'North Carolina St' ~ 'NC State',
    team == 'Western Kentucky' ~ 'WKU',
    T ~ team
  ))
  

sbu <- inner_join(x, bracket_cln)
write_csv(sbu, '3.0_Files/other/sbunfurled.csv')
  