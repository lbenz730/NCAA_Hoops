### Enter Seeds 

update_conf_seeds <- function() {
  # Sunbelt
  seed_teams <- c('Texas St.', 'Appalachian St.',  'Georgia St.', 'Troy',
                  'South Alabama', 'Arkansas St.', 'Coastal Carolina', 'Louisiana', 
                  'UT Arlington', 'Ga. Southern', 'ULM', 'Little Rock')
  
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # A-Sun
  seed_teams <- c('Jacksonville St.', 'Liberty', 'Jacksonville', 'Bellarmine',
                  'FGCU', 'Central Ark.', 'Lipscomb', 'Kennesaw St.',
                  'Eastern Ky.', 'North Florida', 'Stetson', 'North Ala.')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # Patriot
  seed_teams <- c('Colgate', 'Navy', 'Boston U.', 'Lehigh',
                  'Army West Point', 'Loyola Maryland', 'Holy Cross',
                  'Lafayette', 'Bucknell', 'American')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # NEC
  seed_teams <- c('Bryant', 'Wagner', 'LIU', "Mount St. Mary's", 
                  'St. Francis Brooklyn', 'Saint Francis (PA)',
                  'Sacred Heart','Fairleigh Dickinson', 'Central Conn. St.')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # OVC
  seed_teams <- c('Murray St.', 'Belmont', 'Morehead St.', 'Southeast Mo. St.',
                  'Tennessee St.', 'Austin Peay', 'Tennessee Tech', 'SIUE')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # Big South
  seed_teams <- c('Longwood', 'Winthrop', 'Gardner-Webb', 'USC Upstate',
                  'UNC Asheville', 'Campbell', 'High Point', 'Radford',
                  'N.C. A&T', 'Hampton', 'Presbyterian', 'Charleston So.')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # MVC
  seed_teams <- c('UNI', 'Missouri St.', 'Drake', 'Loyola Chicago',
                  'Bradley', 'Southern Ill.', 'Illinois St.', 'Valparaiso',
                  'Indiana St.', 'Evansville')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # Horizon
  seed_teams <- c('Cleveland St.', 'Purdue Fort Wayne', 'Northern Ky.', 'Wright St.',
                  'Oakland', 'Detroit Mercy', 'Youngstown St.', 'UIC', 
                  'Milwaukee', 'Robert Morris', 'Green Bay', 'IUPUI')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # Summit
  seed_teams <- c('South Dakota St.', 'North Dakota St.', 'Oral Roberts',
                  'Kansas City', 'South Dakota', 'Western Ill.', 'Denver', 'Omaha')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # WCC
  seed_teams <- c('Gonzaga', "Saint Mary's (CA)", 'Santa Clara', 'San Francisco', 
                  'BYU', 'Portland', 'San Diego', 'Pacific', 'LMU (CA)', 'Pepperdine')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # Southern
  seed_teams <- c('Chattanooga', 'Furman', 'Samford', 'Wofford', 
                  'VMI', 'UNC Greensboro', 'Mercer', 'ETSU',
                  'The Citadel', 'Western Caro.')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # CAA 
  seed_teams <- c('Towson', 'UNCW', 'Hofstra', 'Drexel',
                  'Delaware', 'Col. of Charleston', 'Elon', 
                  'William & Mary', 'Northeastern')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  
  write_csv(confs, '3.0_Files/Info/conferences.csv')
  return(confs)
}


conf_tourney_graphics <- function() {
  for(conf in sort(unique(confs$conference))) {
    if(file.exists(paste0('3.0_Files/Predictions/conf_tourney_sims/', conf, '.csv'))) {
      df_sim <- read_csv(paste0('3.0_Files/Predictions/conf_tourney_sims/', conf, '.csv'))
      
      
      df <- 
        df_sim %>% 
        inner_join(select(ncaa_colors, 'team' = ncaa_name, logo_url)) %>% 
        inner_join(select(power_rankings, team, yusag_coeff)) %>% 
        inner_join(select(confs, team, eliminated)) %>% 
        filter(!eliminated) %>% 
        arrange(desc(champ), desc(finals)) %>% 
        select(team, logo_url, seed, yusag_coeff, finals, champ)
      
      library(gt)
      table <- 
        gt(df) %>% 
        cols_label('team' = '', 
                   'logo_url' = '',
                   'seed' = 'Seed',
                   'yusag_coeff' = 'Rating',
                   'finals' = 'Finals',
                   'champ' = 'Champ') %>% 
        
        ### Hightlight Columns 
        data_color(
          columns = c(finals, champ),
          colors = scales::col_numeric(
            palette = ggsci::rgb_material('amber', n = 68),
            domain = c(0,1),
          )
        ) %>% 
        
        data_color(
          columns = c(yusag_coeff),
          colors = scales::col_numeric(
            palette = ggsci::rgb_material('amber', n = 358),
            domain = range(df$yusag_coeff)
          ),
        ) %>% 
        
        ### Percent
        fmt_percent(
          columns = c(finals, champ),
          decimals = 1) %>% 
        
        fmt_number(
          columns = c(yusag_coeff),
          decimals = 1) %>% 
        
        
        ### Align Columns
        cols_align(
          align = "center",
          columns = gt::everything()
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
              columns = c(yusag_coeff)
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
        # tab_source_note("2022 Tournament hosted by Harvard University") %>%
        # tab_source_note("Based on 5,000 Simulations. Ties broken according to official Ivy League tiebreaking rules.") %>%
        
        tab_source_note("Table: Luke Benz (@recspecs730) | https://lbenz730.shinyapps.io/recspecs_basketball_central/") %>%
        tab_header(
          title = md(paste("**2022", conf, "Men's Basketball Tournament Odds**")),
          # subtitle = md(paste0('**', table_region, " Region**"))
        ) %>% 
        tab_options(column_labels.font.size = 16,
                    heading.title.font.size = 30,
                    heading.subtitle.font.size = 20,
                    heading.title.font.weight = 'bold',
                    heading.subtitle.font.weight = 'bold'
        )
      gtsave(table, paste0('graphics/conf_tourneys/', conf, '.png'))
    }
  }
}

