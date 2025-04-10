### Enter Seeds 


update_conf_seeds <- function() {
  # Sunbelt To to Edit Sunbelt
  seed_teams <-
    c('South Alabama',
      'James Madison',
      'Troy',
      'Arkansas St.',
      'Marshall',
      'App State',
      'Texas St.',
      'Georgia St.',
      'Ga. Southern',
      'Old Dominion',
      'Louisiana',
      'Southern Miss.',
      'Coastal Carolina',
      'ULM')
  
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # A-Sun
  seed_teams <-
    c('Lipscomb',
      'North Ala.',
      'FGCU',
      'Jacksonville',
      'Eastern Ky.',
      'Queens (NC)',
      'Austin Peay',
      'North Florida',
      'Stetson',
      'Central Ark.')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # CAA
  seed_teams <-
    c('Towson',
      'UNCW',
      'Col. of Charleston',
      'William & Mary',
      'Campbell',
      'Monmouth',
      'Northeastern',
      'Drexel',
      'Elon',
      'Hampton',
      'Hofstra',
      'Delaware',
      'Stony Brook',
      'N.C. A&T')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # Southern
  seed_teams <-
    c('Chattanooga',
      'UNC Greensboro',
      'ETSU',
      'Samford',
      'Furman',
      'Wofford',
      'VMI',
      'Mercer',
      'Western Caro.',
      'The Citadel')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # NEC
  seed_teams <-
    c('Central Conn. St.',
      'LIU',
      'Saint Francis (PA)',
      'Fairleigh Dickinson',
      'Stonehill',
      'Wagner',
      'Chicago St.',
      'Le Moyne')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # # Big South
  seed_teams <-
    c('High Point',
      'UNC Asheville',
      'Winthrop',
      'Radford',
      'Presbyterian',
      'Longwood',
      'Charleston So.',
      'Gardner-Webb',
      'USC Upstate')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # Patriot
  seed_teams <-
    c('Bucknell',
      'American',
      'Colgate',
      'Boston U.',
      'Navy',
      'Army West Point',
      'Lafayette',
      'Loyola Maryland',
      'Lehigh',
      'Holy Cross')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # OVC
  seed_teams <-
    c('Southeast Mo. St.',
      'SIUE',
      'Tennessee St.',
      'Little Rock',
      'Tennessee Tech',
      'Lindenwood',
      'Morehead St.',
      'UT Martin')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # Horizon
  seed_teams <-
    c('Robert Morris',
      'Cleveland St.',
      'Milwaukee',
      'Youngstown St.',
      'Purdue Fort Wayne',
      'Oakland',
      'Northern Ky.',
      'Wright St.',
      'IU Indy',
      'Detroit Mercy',
      'Green Bay')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # WCC
  seed_teams <-
    c('Saint Mary\'s (CA)',
      'Gonzaga',
      'San Francisco',
      'Santa Clara',
      'Oregon St.',
      'Washington St.',
      'LMU (CA)',
      'Portland',
      'Pepperdine',
      'Pacific',
      'San Diego')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # Summit
  seed_teams <-
    c('Omaha',
      'St. Thomas (MN)',
      'South Dakota St.',
      'North Dakota St.',
      'South Dakota',
      'North Dakota',
      'Denver',
      'Kansas City',
      'Oral Roberts')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # MVC
  seed_teams <-
    c('Drake',
      'Bradley',
      'UNI',
      'Belmont',
      'Illinois St.',
      'UIC',
      'Murray St.',
      'Southern Ill.',
      'Indiana St.',
      'Evansville',
      'Valparaiso',
      'Missouri St.')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # Big Sky
  seed_teams <-
    c('Northern Colo.',
      'Montana',
      'Portland St.',
      'Idaho St.',
      'Montana St.',
      'Idaho',
      'Northern Ariz.',
      'Eastern Wash.',
      'Weber St.',
      'Sacramento St.')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  
  # Am. East
  seed_teams <-
    c('Bryant',
      'Vermont',
      'Maine',
      'UAlbany',
      'Binghamton',
      'UMass Lowell',
      'New Hampshire',
      'UMBC')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # MEAC
  seed_teams <-
    c('Norfolk St.',
      'South Carolina St.',
      'Delaware St.',
      'Howard',
      'Morgan St.',
      'N.C. Central',
      'Coppin St.',
      'UMES')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # Southland
  seed_teams <-
    c('McNeese',
      'Lamar University',
      'Nicholls',
      'Northwestern St.',
      'A&M-Corpus Christi',
      'Southeastern La.',
      'UIW',
      'Houston Christian')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # MAC
  seed_teams <-
    c('Akron',
      'Miami (OH)',
      'Kent St.',
      'Toledo',
      'Ohio',
      'Western Mich.',
      'Eastern Mich.',
      'Bowling Green')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # Ivy
  seed_teams <-
    c('Yale',
      'Cornell',
      'Dartmouth',
      'Princeton')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # C-USA
  seed_teams <-
    c('Liberty',
      'Jacksonville St.',
      'Middle Tenn.',
      'New Mexico St.',
      'Kennesaw St.',
      'Louisiana Tech',
      'Western Ky.',
      'Sam Houston',
      'UTEP',
      'FIU')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # A-10
  seed_teams <-
    c('VCU',
      'George Mason',
      'Dayton',
      'Loyola Chicago',
      'Saint Louis',
      'Saint Joseph\'s',
      'George Washington',
      'St. Bonaventure',
      'Duquesne',
      'Rhode Island',
      'Massachusetts',
      'Davidson',
      'Richmond',
      'La Salle',
      'Fordham')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)

  # MAAC
  seed_teams <-
    c('Quinnipiac',
      'Merrimack',
      'Marist',
      'Iona',
      'Manhattan',
      'Mount St. Mary\'s',
      'Sacred Heart',
      'Rider',
      'Siena',
      'Fairfield')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # SEC
  seed_teams <-
    c('Auburn',
      'Florida',
      'Alabama',
      'Tennessee',
      'Texas A&M',
      'Kentucky',
      'Missouri',
      'Ole Miss',
      'Arkansas',
      'Mississippi St.',
      'Georgia',
      'Vanderbilt',
      'Texas',
      'Oklahoma',
      'LSU',
      'South Carolina')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # Big East
  seed_teams <-
    c('St. John\'s (NY)',
      'Creighton',
      'UConn',
      'Xavier',
      'Marquette',
      'Villanova',
      'Georgetown',
      'Providence',
      'Butler',
      'DePaul',
      'Seton Hall')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # ACC
  seed_teams <-
    c('Duke',
      'Louisville',
      'Clemson',
      'Wake Forest',
      'North Carolina',
      'SMU',
      'Stanford',
      'Georgia Tech',
      'Virginia',
      'Virginia Tech',
      'Florida St.',
      'Notre Dame',
      'Pittsburgh',
      'Syracuse',
      'California')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  
  # SWAC
  seed_teams <-
    c('Southern U.',
      'Jackson St.',
      'Bethune-Cookman',
      'Texas Southern',
      'Alabama St.',
      'Alcorn',
      'Florida A&M',
      'Grambling',
      'Alabama A&M',
      'Prairie View')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)

  # Big 12
  seed_teams <-
    c('Houston',
      'Texas Tech',
      'Arizona',
      'BYU',
      'Iowa St.',
      'Kansas',
      'Baylor',
      'West Virginia',
      'TCU',
      'Kansas St.',
      'Utah',
      'Oklahoma St.',
      'Cincinnati',
      'UCF',
      'Arizona St.',
      'Colorado')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  # 
  # # Pac 12
  # seed_teams <-
  #   c('Arizona',
  #     'Washington St.',
  #     'Colorado',
  #     'Oregon',
  #     'UCLA',
  #     'Utah',
  #     'California',
  #     'Washington',
  #     'Southern California',
  #     'Stanford',
  #     'Arizona St.',
  #     'Oregon St.')
  # confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)
  # 
  # 
  # MWC
  seed_teams <-
    c(
      'New Mexico',
      'Colorado St.',
      'Utah St.',
      'San Diego St.',
      'Boise St.',
      'UNLV',
      'Nevada',
      'San Jose St.',
      'Wyoming',
      'Fresno St.',
      'Air Force')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)

  # WAC
  seed_teams <-
    c('Utah Valley',
      'Grand Canyon',
      'California Baptist',
      'Abilene Christian',
      'Seattle U',
      'Tarleton St.',
      'UT Arlington',
      'Southern Utah',
      'Utah Tech')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)

  # Big West
  seed_teams <-
    c('UC San Diego',
      'UC Irvine',
      'UC Riverside',
      'CSUN',
      'UC Santa Barbara',
      'UC Davis',
      'Cal Poly',
      'CSU Bakersfield')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)

  # AAC
  seed_teams <-
    c('Memphis',
      'North Texas',
      'UAB',
      'Tulane',
      'Fla. Atlantic',
      'East Carolina',
      'Temple',
      'Wichita St.',
      'South Fla.',
      'Tulsa',
      'UTSA',
      'Rice',
      'Charlotte')
  confs$conf_seed[map_dbl(seed_teams, ~which(confs$team == .x))] <- 1:length(seed_teams)

  # Big 10
  seed_teams <-
    c('Michigan St.',
      'Maryland',
      'Michigan',
      'UCLA',
      'Wisconsin',
      'Purdue',
      'Illinois',
      'Oregon',
      'Indiana',
      'Ohio St.',
      'Rutgers',
      'Minnesota',
      'Northwestern',
      'Southern California',
      'Iowa')
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


conf_tourney_graphics <- function(year = '2024-25') {
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
        arrange(desc(champ), desc(finals)) %>% 
        select(team, logo_file, seed, yusag_coeff, finals, champ, eliminated)
      
      library(gt)
      table <-
        gt(df %>% filter(!eliminated) %>% select(-eliminated)) %>% 
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
          title = md(paste("**2025", conf, "Men's Basketball Tournament Odds**")),
        ) %>% 
        tab_options(column_labels.font.size = 16,
                    column_labels.font.weight = 'bold',
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
    summarise('n_wins' = sum(wins, na.rm = T),
              'n_loss' = sum(1-wins, na.rm = T)) %>% 
    arrange(-n_wins) %>% 
    pull(team)  
  
  cat('c(',  paste(paste0("'", teams, "'"), collapse = ',\n'), ')', sep = '')
}
