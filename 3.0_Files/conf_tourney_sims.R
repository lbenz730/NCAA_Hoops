### Enter Seeds 

update_conf_seeds <- function() {
  # Sunbelt
  seed_teams <- 
    c('App State',
      'James Madison',
      'Troy',
      'Arkansas St.',
      'Louisiana',
      'Southern Miss.',
      'Georgia St.',
      'South Alabama',
      'Ga. Southern',
      'Marshall',
      'Texas St.',
      'ULM',
      'Coastal Carolina',
      'Old Dominion')
  
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # A-Sun
  seed_teams <- 
    c('Eastern Ky.',
      'Stetson',
      'Lipscomb',
      'Austin Peay',
      'North Florida',
      'North Ala.',
      'FGCU',
      'Queens (NC)',
      'Kennesaw St.',
      'Jacksonville')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # CAA
  seed_teams <- 
    c('Col. of Charleston',
      'Drexel',
      'Hofstra',
      'UNCW',
      'Towson',
      'Delaware',
      'Stony Brook',
      'Monmouth',
      'Campbell',
      'Northeastern',
      'Elon',
      'N.C. A&T',
      'William & Mary',
      'Hampton')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # Southern
  seed_teams <-
    c('Samford',
      'UNC Greensboro',
      'Chattanooga',
      'Western Caro.',
      'Furman',
      'Wofford',
      'ETSU',
      'Mercer',
      'The Citadel',
      'VMI')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # NEC
  seed_teams <- 
    c('Central Conn. St.',
      'Merrimack',
      'Sacred Heart',
      'Le Moyne',
      'Fairleigh Dickinson',
      'Wagner',
      'LIU',
      'Saint Francis (PA)')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # Big South
  seed_teams <-
    c('High Point',
      'UNC Asheville',
      'Gardner-Webb',
      'Winthrop',
      'Longwood',
      'Presbyterian',
      'Charleston So.',
      'USC Upstate',
      'Radford')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # Patriot
  seed_teams <-
    c('Colgate', 
      'Boston U.',
      'Lafayette',
      'American',
      'Bucknell',
      'Lehigh',
      'Navy', 
      'Army West Point', 
      'Holy Cross',
      'Loyola Maryland')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  
  
  # OVC
  seed_teams <-
    c('Little Rock',
      'UT Martin',
      'Morehead St.',
      'Western Ill.',
      'Tennessee St.',
      'SIUE',
      'Eastern Ill.',
      'Southern Ind.')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # Horizon
  seed_teams <- 
    c('Oakland',
      'Youngstown St.',
      'Green Bay',
      'Wright St.',
      'Northern Ky.',
      'Milwaukee',
      'Cleveland St.',
      'Purdue Fort Wayne',
      'Robert Morris',
      'IUPUI',
      'Detroit Mercy')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # WCC
  seed_teams <- 
    c('Saint Mary\'s (CA)',
      'Gonzaga',
      'San Francisco',
      'Santa Clara',
      'San Diego',
      'Portland',
      'LMU (CA)',
      'Pepperdine',
      'Pacific')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # Summit
  seed_teams <- 
    c('South Dakota St.', 
      'Kansas City',
      'North Dakota',
      'St. Thomas (MN)',
      'North Dakota St.',
      'Omaha',
      'Denver',
      'Oral Roberts',
      'South Dakota')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # MVC
  seed_teams <- 
    c('Indiana St.',
      'Drake', 
      'Bradley',
      'UNI', 
      'Belmont', 
      'Southern Ill.', 
      'Illinois St.',
      'Murray St.', 
      'Missouri St.', 
      'Evansville',
      'UIC',
      'Valparaiso')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # Big Sky
  seed_teams <- 
    c('Eastern Wash.',
      'Northern Colo.',
      'Montana', 
      'Weber St.', 
      'Montana St.', 
      'Portland St.', 
      'Northern Ariz.',
      'Idaho',
      'Idaho St.',
      'Sacramento St.')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  
  # Am. East
  seed_teams <- 
    c('Vermont', 
      'UMass Lowell',
      'Bryant',
      'New Hampshire', 
      'Binghamton',
      'Maine',
      'UMBC',
      'UAlbany')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # # MEAC
  # seed_teams <- c('Howard', 'N.C. Central', 'Norfolk St.', 'UMES', 'Morgan St.',
  #                 'Coppin St.', 'South Carolina St.','Delaware St.')
  # confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  # 
  # # MAC
  # seed_teams <- c('Toledo', 'Kent St.', 'Akron', 'Ball St.', 'Ohio',
  #                 'Buffalo','NIU','Miami (OH)')
  # confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  # 
  # # MWC
  # seed_teams <- c('San Diego St.', 'Boise St.',  'Utah St.', 'Nevada', 'San Jose St.', 
  #                 'New Mexico', 'UNLV', 'Colorado St.', 'Fresno St.', 'Air Force', 'Wyoming')
  # confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  # 
  # # WAC
  # seed_teams <-
  #   c('Sam Houston', 'Utah Valley',  'Southern Utah', 'Seattle U', 'Grand Canyon',  'SFA', 
  #     'Tarleton St.', 'California Baptist', 'Abilene Christian', 'UTRGV', 'Utah Tech','UT Arlington')
  # confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  # 
  # # Ivy
  # seed_teams <- c('Yale', 'Princeton', 'Penn', 'Cornell')
  # confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  # 

  # 
  # # Big East
  # seed_teams <- c('Marquette', 'Xavier', 'Creighton', 'UConn', 'Providence', 
  #                 'Villanova', 'Seton Hall', "St. John's (NY)", 'Butler', 'DePaul', 'Georgetown')
  # confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  # 
  # # Southkand
  # seed_teams <- c('A&M-Corpus Christi', 'Northwestern St.',  'Southeastern La.','Nicholls',
  #                 'Tex. A&M-Commerce', 'Houston Christian','New Orleans',  'McNeese')
  # confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  # 
  # # SWAC
  # seed_teams <- c('Alcorn', 'Grambling',  'Jackson St.', 'Southern U.', 'Alabama A&M', 
  #                 'Prairie View', 'Bethune-Cookman', 'Texas Southern')
  # confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  # 
  # # Pac 12
  # seed_teams <- c('UCLA', 'Arizona', 'Southern California', 'Oregon', 
  #                 'Washington St.', 'Arizona St.', 'Utah',  'Washington', 'Colorado',
  #                 'Stanford', 'Oregon St.', 'California')
  # confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  # 
  # # SEC
  # seed_teams <- c('Alabama', 'Texas A&M',  'Kentucky', 'Missouri', 
  #                 'Tennessee', 'Vanderbilt', 'Auburn', 'Florida', 'Mississippi St.', 'Arkansas', 
  #                 'Georgia', 'South Carolina', 'Ole Miss', 'LSU')
  # confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  # 
  # # ACC
  # seed_teams <- c('Miami (FL)', 'Virginia', 'Clemson', 'Duke',
  #                 'Pittsburgh', 'NC State', 'North Carolina', 'Syracuse',
  #                 'Wake Forest', 'Boston College',  'Virginia Tech', 'Florida St.', 
  #                 'Georgia Tech', 'Notre Dame', 'Louisville')
  # confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  # 
  # # C-USA
  # seed_teams <- c('Fla. Atlantic', 'North Texas', 'UAB', 'Middle Tenn.', 
  #                 'Charlotte', 'Rice', 'FIU', 'Western Ky.', 'UTEP', 'Louisiana Tech', 'UTSA')
  # confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  # 
  # # MAAC
  # seed_teams <- c('Iona', 'Rider','Quinnipiac', 'Siena', 'Niagara', 'Manhattan', 
  #                 'Fairfield', 'Mount St. Mary\'s', 'Canisius', 'Saint Peter\'s', 'Marist')
  # confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  # 
  # # A-10
  # seed_teams <- c('VCU', 'Dayton', 'Fordham', 'Saint Louis', 
  #                 'George Mason',  'Duquesne', 'George Washington', 'Davidson', 
  #                 'St. Bonaventure', 'Saint Joseph\'s', 'La Salle',  'Richmond', 
  #                 'Massachusetts', 'Rhode Island', 'Loyola Chicago')
  # confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  # 
  # # Big 12
  # seed_teams <- c('Kansas', 'Texas', 'Kansas St.', 'Baylor', 'Iowa St.', 'TCU', 
  #                 'Oklahoma St.', 'West Virginia', 'Texas Tech', 'Oklahoma')
  # confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  # 
  # # Big West
  # seed_teams <- c('UC Irvine', 'UC Santa Barbara', 'UC Riverside',
  #                 'Cal St. Fullerton', 'Hawaii', 'UC Davis', 'Long Beach St.',
  #                 'CSU Bakersfield', 'CSUN', 'Cal Poly')
  # confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  # 
  # # AAC
  # seed_teams <- c('Houston', 'Memphis',  'Tulane', 'Cincinnati', 'Temple',
  #                 'Wichita St.', 'UCF', 'South Fla.', 'East Carolina', 'SMU', 
  #                 'Tulsa')
  # confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  # 
  # # Big 10
  # seed_teams <- c('Purdue', 'Northwestern', 'Indiana', 'Michigan St.', 
  #                 'Iowa', 'Maryland', 'Illinois', 'Michigan',
  #                 'Rutgers', 'Penn St.', 'Nebraska', 'Wisconsin',
  #                 'Ohio St.', 'Minnesota')
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


conf_tourney_graphics <- function(year = '2023-24') {
  if(!dir.exists(paste0('graphics/conf_tourneys/', year))) {
    dir.create(paste0('graphics/conf_tourneys/', year))
  }
  
  img_files <- dir('app/www/')
  df_img <- 
    tibble('team' = teams,
           'logo_file' = paste0('app/www/', map_chr(teams, ~case_when(paste0(.x, '.png') %in% img_files ~ paste0(.x, '.png'), 
                                                                      paste0(.x, '.svg') %in% img_files ~ paste0(.x, '.svg'), 
                                                                      paste0(.x, '.jpg') %in% img_files ~ paste0(.x, '.jpg')))))
  
  
  
  for(conf in sort(unique(confs$conference))) {
    print(conf)
    if(file.exists(paste0('3.0_Files/Predictions/conf_tourney_sims/', year, '/', conf, '.csv'))) {
      df_sim <- read_csv(paste0('3.0_Files/Predictions/conf_tourney_sims/', year, '/', conf, '.csv'), col_types = cols()) 
      
      
      df <- 
        df_sim %>% 
        inner_join(df_img, by = 'team') %>% 
        inner_join(select(power_rankings, team, yusag_coeff), by = 'team') %>% 
        inner_join(select(confs, team, eliminated), by = 'team') %>% 
        filter(!eliminated) %>% 
        arrange(desc(champ), desc(finals)) %>% 
        select(team, logo_file, seed, yusag_coeff, finals, champ)
      
      library(gt)
      table <-
        gt(df) %>% 
        cols_label('team' = '', 
                   'logo_file' = '',
                   'seed' = 'Seed',
                   'yusag_coeff' = 'Rating',
                   'finals' = 'Finals',
                   'champ' = 'Champ') %>% 
        
        ### Hightlight Columns 
        data_color(
          columns = c(finals, champ),
          fn = scales::col_numeric(
            palette = ggsci::rgb_material('amber', n = 68),
            domain = c(0,1),
          )
        ) %>% 
        
        data_color(
          columns = c(yusag_coeff),
          fn = scales::col_numeric(
            palette = 'RdYlGn',
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
          locations = cells_body(c(logo_file)),
          fn = function(x) {
            local_image(
              filename = gsub('amp;', '', x),
              height = 30
            )
          }
        ) %>% 
        tab_source_note("Table: Luke Benz (@recspecs730) | https://lbenz730.shinyapps.io/recspecs_basketball_central/") %>%
        tab_source_note("Rating = Points relative to average NCAA team on neutral court") %>%
        tab_header(
          title = md(paste("**2024", conf, "Men's Basketball Tournament Odds**")),
        ) %>% 
        tab_options(column_labels.font.size = 16,
                    column_labels.font.weight = 'bold',
                    heading.title.font.size = 30,
                    heading.subtitle.font.size = 20,
                    heading.title.font.weight = 'bold',
                    heading.subtitle.font.weight = 'bold'
        )
      gtExtras::gtsave_extra(table, paste0('graphics/conf_tourneys/', year, '/', conf, '.png'))
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
    summarise('n_wins' = sum(wins, na.rm = T),
              'n_loss' = sum(1-wins, na.rm = T)) %>% 
    arrange(-n_wins) %>% 
    pull(team)  
  
  cat('c(',  paste(paste0("'", teams, "'"), collapse = ',\n'), ')', sep = '')
}
