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
  seed_teams <-c('Merrimack', 'Fairleigh Dickinson', 'Saint Francis (PA)', 'Sacred Heart', 
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
  
  # # MVC
  # seed_teams <- c('UNI', 'Missouri St.', 'Drake', 'Loyola Chicago',
  #                 'Bradley', 'Southern Ill.', 'Illinois St.', 'Valparaiso',
  #                 'Indiana St.', 'Evansville')
  # confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  # 
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
  
  # # Am. East 
  # seed_teams <- c('Vermont', 'UMBC', 'New Hampshire', 'Hartford',
  #                 'Albany (NY)', 'Binghamton', 'UMass Lowell', 'NJIT')
  # confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  # 
  # # MEAC
  # seed_teams <- c('Norfolk St.', 'Howard', 'N.C. Central', 'Morgan St.',
  #                 'South Carolina St.', 'UMES', 'Coppin St.', 'Delaware St.')
  # confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  # 
  # # MAC
  # seed_teams <- c('Toledo', 'Kent St.', 'Ohio', 'Akron', 
  #                 'Buffalo', 'Ball St.', 'Miami (OH)', 'Central Mich.')
  # confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  # 
  # # MWC
  # seed_teams <- c('Boise St.', 'Colorado St.', 'San Diego St.', 'Wyoming',
  #                 'UNLV', 'Fresno St.', 'Utah St.', 'Nevada', 'New Mexico',
  #                 'Air Force', 'San Jose St.')
  # confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  # 
  # # WAC
  # seed_teams <- c('New Mexico St.', 'Seattle U', 'SFA', 'Grand Canyon',
  #                 'Sam Houston St.', 'Abilene Christian', 'Utah Valley',
  #                 'California Baptist', 'UTRGV', 'Chicago St.')
  # confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  # 
  # # Ivy
  # seed_teams <- c('Princeton', 'Yale', 'Penn', 'Cornell')
  # confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  # 
  # # Big Sky
  # seed_teams <- c('Montana St.', 'Southern Utah', 'Northern Colo.', 'Weber St.',
  #                 'Montana', 'Eastern Wash.', 'Portland St.', 'Sacramento St.',
  #                 'Idaho', 'Idaho St.', 'Northern Ariz.')
  # confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  # 
  # # Big East
  # seed_teams <- c('Providence', 'Villanova', 'UConn', 'Creighton', 'Marquette',
  #                 'Seton Hall', 'St. John\'s (NY)', 'Xavier', 'Butler', 'DePaul', 'Georgetown')
  # confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  # 
  # # Southkand
  # seed_teams <- c('Nicholls St.', 'Southeastern La.', 'New Orleans', 'A&M-Corpus Christi',
  #                 'Houston Baptist', 'Northwestern St.', 'McNeese', 'UIW')
  # confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  # 
  # # SWAC
  # seed_teams <- c('Alcorn', 'Southern U.', 'Texas Southern', 'Florida A&M', 
  #                 'Alabama A&M', 'Grambling', 'Jackson St.', 'Prairie View')
  # confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  # 
  # # Pac 12
  # seed_teams <- c('Arizona', 'UCLA', 'Southern California', 'Colorado', 'Oregon',
  #                 'Washington', 'Washington St.', 'Arizona St.', 'Stanford',
  #                 'California', 'Utah', 'Oregon St.')
  # confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  # 
  # # SEC
  # seed_teams <- c('Auburn', 'Tennessee', 'Kentucky', 'Arkansas', 
  #                 'LSU', 'Alabama', 'South Carolina', 'Texas A&M',
  #                 'Florida', 'Mississippi St.', 'Vanderbilt', 'Missouri', 
  #                 'Ole Miss', 'Georgia')
  # confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  # 
  # # ACC 
  # seed_teams <- c('Duke', 'Notre Dame', 'North Carolina', 'Miami (FL)',
  #                 'Wake Forest', 'Virginia', 'Virginia Tech', 'Florida St.',
  #                 'Syracuse', 'Clemson', 'Louisville', 'Pittsburgh',
  #                 'Boston College', 'Georgia Tech', 'NC State')
  # confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  # 
  # # C-USA
  # seed_teams <- c('North Texas', 'UAB', 'Middle Tenn.', 'Western Ky.', 
  #                 'Louisiana Tech', 'UTEP', 'Fla. Atlantic', 'Charlotte',
  #                 'Rice', 'Old Dominion', 'UTSA', 'FIU', 'Marshall',
  #                 'Southern Miss.')
  # confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  #                 
  # 
  # # MAAC
  # seed_teams <- c('Iona', 'Saint Peter\'s', 'Siena', 'Monmouth',
  #                 'Niagara', 'Marist', 'Fairfield', 'Manhattan',
  #                 'Rider', 'Canisius', 'Quinnipiac')
  # confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  # 
  # # A-10
  # seed_teams <- c('Davidson', 'Dayton', 'VCU', 'St. Bonaventure',
  #                 'Saint Louis', 'Richmond', 'George Washington', 'Fordham',
  #                 'George Mason', 'Massachusetts', 'Rhode Island',
  #                 'La Salle', 'Saint Joseph\'s',
  #                 'Duquesne')
  # confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  # 
  # 
  # # Big 12
  # seed_teams <- c('Kansas', 'Baylor', 'Texas Tech', 'Texas', 'TCU', 'Iowa St.',
  #                 'Oklahoma', 'Kansas St.', 'West Virginia')
  # confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  # 
  # # Big West
  # seed_teams <- c('Long Beach St.', 'Cal St. Fullerton', "Hawaii", 'UC Irvine',
  #                 'UC Santa Barbara', 'UC Riverside', 'UC Davis', 'CSUN', 
  #                 'CSU Bakersfield', 'Cal Poly')
  # confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  # 
  # # AAC
  # seed_teams <- c('Houston', 'SMU', 'Memphis', 'Temple', 'Tulane',
  #                 'UCF', 'Wichita St.', 'Cincinnati', 'East Carolina',
  #                 'Tulsa', 'South Fla.')
  # confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  # 
  # # Big 10
  # seed_teams <- c('Illinois', 'Wisconsin', 'Purdue', 'Rutgers',
  #                 'Iowa', 'Ohio St.', 'Michigan St.', 'Michigan', 
  #                 'Indiana', 'Maryland', 'Penn St.', 'Northwestern',
  #                 'Nebraska', 'Minnesota')
  # confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
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
