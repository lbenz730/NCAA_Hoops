### Enter Seeds 

update_conf_seeds <- function() {
  # Sunbelt
  seed_teams <- c('Southern Miss.', 'Louisiana', 'Marshall', 
                  'James Madison',  'Troy', 'Old Dominion',  'Ga. Southern', 'South Alabama', 
                  'App State','ULM', 'Texas St.', 'Coastal Carolina', 'Arkansas St.', 'Georgia St.')
  
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # A-Sun
  seed_teams <- c('Kennesaw St.', 'Liberty', 'Eastern Ky.', 'Stetson', 
                  'Lipscomb', 'North Ala.', 'North Florida', 'Bellarmine',
                  'Queens (NC)', 'FGCU')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # Patriot
  seed_teams <- c('Colgate', 'Navy', 'Lehigh', 'Army West Point', 'Boston U.', 
                  'Lafayette', 'American', 'Loyola Maryland', 'Holy Cross', 'Bucknell')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # NEC
  seed_teams <- c('Merrimack', 'Fairleigh Dickinson', 'Saint Francis (PA)', 'Sacred Heart', 
                  'Wagner', 'Central Conn. St.', 'St. Francis Brooklyn', 'LIU')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # OVC
  seed_teams <- c('Morehead St.', 'Tennessee Tech', 'Tennessee St.', 'UT Martin', 
                  'Southeast Mo. St.', 'SIUE', 'Southern Ind.', 'Lindenwood')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # Big South
  seed_teams <-c('UNC Asheville', 'Longwood', 'Radford', 
                 'USC Upstate', 'Gardner-Webb', 'Winthrop', 
                 'Campbell', 'High Point', 'Charleston So.', 
                 'Presbyterian')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # MVC
  seed_teams <- c('Bradley', 'Drake', 'Southern Ill.', 'Belmont', 'Indiana St.', 
                  'Missouri St.', 'Murray St.', 'UNI', 'Illinois St.', 
                  'Valparaiso', 'UIC', 'Evansville')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # Horizon
  seed_teams <- c('Youngstown St.', 'Milwaukee', 'Cleveland St.',
                  'Northern Ky.', 'Oakland', 'Robert Morris', 'Wright St.', 
                  'Detroit Mercy', 'Purdue Fort Wayne', 'Green Bay', 'IUPUI')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # Summit
  seed_teams <- c('Oral Roberts', 'South Dakota St.', 'North Dakota St.',
                  'Western Ill.', 'St. Thomas (MN)', 'South Dakota',
                  'Kansas City', 'Denver', 'North Dakota', 'Omaha')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # WCC
  seed_teams <- c( "Saint Mary's (CA)", 'Gonzaga', 'Santa Clara', 'LMU (CA)', 
                   'BYU', 'San Francisco', 'Pacific', 'Portland', 'San Diego',  'Pepperdine')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # Southern
  seed_teams <- c('Furman', 'Samford', 'UNC Greensboro', 'Western Caro.', 'ETSU',
                  'Wofford', 'Chattanooga', 'Mercer', 'The Citadel', 'VMI')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # CAA
  seed_teams <- c('Hofstra', 'Col. of Charleston', 'Towson', 
                  'UNCW',  'Drexel', 'Delaware', 'N.C. A&T', 
                  'William & Mary', 'Elon', 'Stony Brook', 
                  'Northeastern', 'Hampton', 'Monmouth')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # Am. East
  seed_teams <- c('Vermont', 'UMass Lowell', 'New Hampshire', 'UMBC', 
                  'Binghamton','Bryant', 'Maine',  'NJIT')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # MEAC
  seed_teams <- c('Howard', 'N.C. Central', 'Norfolk St.', 'UMES', 'Morgan St.',
                  'Coppin St.', 'South Carolina St.','Delaware St.')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # MAC
  seed_teams <- c('Toledo', 'Kent St.', 'Akron', 'Ball St.', 'Ohio',
                  'Buffalo','NIU','Miami (OH)')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # MWC
  seed_teams <- c('San Diego St.', 'Boise St.',  'Utah St.', 'Nevada', 'San Jose St.', 
                  'New Mexico', 'UNLV', 'Colorado St.', 'Fresno St.', 'Air Force', 'Wyoming')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # WAC
  seed_teams <-
    c('Sam Houston', 'Utah Valley',  'Southern Utah', 'Seattle U', 'Grand Canyon',  'SFA', 
      'Tarleton St.', 'California Baptist', 'Abilene Christian', 'UTRGV', 'Utah Tech','UT Arlington')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # Ivy
  seed_teams <- c('Yale', 'Princeton', 'Penn', 'Cornell')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # Big Sky
  seed_teams <- c('Eastern Wash.', 'Montana St.', 'Weber St.', 'Montana',  
                  'Idaho St.','Sacramento St.','Portland St.', 'Northern Colo.',
                  'Northern Ariz.', 'Idaho')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # Big East
  seed_teams <- c('Marquette', 'Xavier', 'Creighton', 'UConn', 'Providence', 
                  'Villanova', 'Seton Hall', "St. John's (NY)", 'Butler', 'DePaul', 'Georgetown')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # Southkand
  seed_teams <- c('A&M-Corpus Christi', 'Northwestern St.',  'Southeastern La.','Nicholls',
                  'Tex. A&M-Commerce', 'Houston Christian','New Orleans',  'McNeese')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # SWAC
  seed_teams <- c('Alcorn', 'Grambling',  'Jackson St.', 'Southern U.', 'Alabama A&M', 
                  'Prairie View', 'Bethune-Cookman', 'Texas Southern')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # Pac 12
  seed_teams <- c('UCLA', 'Arizona', 'Southern California', 'Oregon', 
                  'Washington St.', 'Arizona St.', 'Utah',  'Washington', 'Colorado',
                  'Stanford', 'Oregon St.', 'California')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)

  # SEC
  seed_teams <- c('Alabama', 'Texas A&M',  'Kentucky', 'Missouri', 
                  'Tennessee', 'Vanderbilt', 'Auburn', 'Florida', 'Mississippi St.', 'Arkansas', 
                  'Georgia', 'South Carolina', 'Ole Miss', 'LSU')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # ACC
  seed_teams <- c('Miami (FL)', 'Virginia', 'Clemson', 'Duke',
                  'Pittsburgh', 'NC State', 'North Carolina', 'Syracuse',
                  'Wake Forest', 'Boston College',  'Virginia Tech', 'Florida St.', 
                  'Georgia Tech', 'Notre Dame', 'Louisville')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # C-USA
  seed_teams <- c('Fla. Atlantic', 'North Texas', 'UAB', 'Middle Tenn.', 
                  'Charlotte', 'Rice', 'FIU', 'Western Ky.', 'UTEP', 'Louisiana Tech', 'UTSA')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # MAAC
  seed_teams <- c('Iona', 'Rider','Quinnipiac', 'Siena', 'Niagara', 'Manhattan', 
                  'Fairfield', 'Mount St. Mary\'s', 'Canisius', 'Saint Peter\'s', 'Marist')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # A-10
  seed_teams <- c('VCU', 'Dayton', 'Fordham', 'Saint Louis', 
                  'George Mason',  'Duquesne', 'George Washington', 'Davidson', 
                  'St. Bonaventure', 'Saint Joseph\'s', 'La Salle',  'Richmond', 
                  'Massachusetts', 'Rhode Island', 'Loyola Chicago')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # Big 12
  seed_teams <- c('Kansas', 'Texas', 'Kansas St.', 'Baylor', 'Iowa St.', 'TCU', 
                  'Oklahoma St.', 'West Virginia', 'Texas Tech', 'Oklahoma')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # Big West
  seed_teams <- c('UC Irvine', 'UC Santa Barbara', 'UC Riverside',
                  'Cal St. Fullerton', 'Hawaii', 'UC Davis', 'Long Beach St.',
                  'CSU Bakersfield', 'CSUN', 'Cal Poly')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)

  # AAC
  seed_teams <- c('Houston', 'Memphis',  'Tulane', 'Cincinnati', 'Temple',
                  'Wichita St.', 'UCF', 'South Fla.', 'East Carolina', 'SMU', 
                  'Tulsa')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)

  # Big 10
  seed_teams <- c('Purdue', 'Northwestern', 'Indiana', 'Michigan St.', 
                  'Iowa', 'Maryland', 'Illinois', 'Michigan',
                  'Rutgers', 'Penn St.', 'Nebraska', 'Wisconsin',
                  'Ohio St.', 'Minnesota')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  
    
  
  ### Elimiate teams that didn't even make their conf tournament
  confs <- 
    confs %>% 
    group_by(conference) %>% 
    mutate('conf_seeded' = any(!is.na(conf_seed))) %>% 
    ungroup() %>% 
    mutate('eliminated' = ifelse(conf_seeded & is.na(conf_seed), T, eliminated)) %>% 
    select(-conf_seeded)
  
  write_csv(confs, '3.0_Files/Info/conferences.csv')
  return(confs)
}


conf_tourney_graphics <- function(year = '2022-23') {
  for(conf in sort(unique(confs$conference))) {
    print(conf)
    if(file.exists(paste0('3.0_Files/Predictions/conf_tourney_sims/', year, '/', conf, '.csv'))) {
      df_sim <- read_csv(paste0('3.0_Files/Predictions/conf_tourney_sims/', year, '/', conf, '.csv'), col_types = cols())
      
      
      df <- 
        df_sim %>% 
        inner_join(select(ncaa_colors, 'team' = ncaa_name, logo_url), by = 'team') %>% 
        inner_join(select(power_rankings, team, yusag_coeff), by = 'team') %>% 
        inner_join(select(confs, team, eliminated), by = 'team') %>% 
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
          title = md(paste("**2023", conf, "Men's Basketball Tournament Odds**")),
          # subtitle = md(paste0('**', table_region, " Region**"))
        ) %>% 
        tab_options(column_labels.font.size = 16,
                    heading.title.font.size = 30,
                    heading.subtitle.font.size = 20,
                    heading.title.font.weight = 'bold',
                    heading.subtitle.font.weight = 'bold'
        )
      gtsave(table, paste0('graphics/conf_tourneys/', year, '/', conf, '.png'))
    }
  }
}



conf_seeding <- function(conf_) {
  teams <- 
    x %>% 
    filter(conf_game) %>% 
    filter(reg_season) %>% 
    filter(team_conf == conf_) %>% 
    group_by(team) %>% 
    summarise('n_wins' = sum(wins),
              'n_loss' = sum(1-wins)) %>% 
    arrange(-n_wins) %>% 
    pull(team)  
  
  cat('c(',  paste(paste0("'", teams, "'"), collapse = ', '), ')', sep = '')
}
