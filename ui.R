library(shiny)
library(DT)

# Define UI 
shinyUI(navbarPage("recspecs730 Basketball Central",
                   tabPanel("Rankings",
                            # Application title
                            titlePanel("NCAA Men's Basketball Power Rankings"),
                            
                            sidebarLayout(position = "left",
                                          sidebarPanel(
                                            p(align = "center", 
                                              textOutput("update"),
                                              tags$head(tags$style("#update{text-align: center; color: ; font-weight: bold;}"))
                                              
                                            ),
                                            
                                            p(tags$b("Net Rating"), "denotes the number of",
                                              "points a given team would expect to",
                                              "win/lose by against", tags$b("an average college basketball team on a neutral court.")
                                            ),
                                            
                                            p(tags$b("Offensive Rating"), "denotes the number of points more/less than average",
                                              "a given team would score against the", 
                                              tags$b("average college basketball team on a neutral floor")
                                            ),
                                            
                                            p(tags$b("Deffensive Rating"), "denotes the number of points more/less than average",
                                              "a given team would allow against the", 
                                              tags$b("average college basketball team on a neutral floor")
                                            ),
                                            
                                            p("It’s easy to want to interpret these as meaures of offensive and defensive strength",
                                              "but it’s extremely important we don’t fall for this temptation. On a points per game basis,",
                                              "a team fewer points than the average college basketball team, but this could be because their",
                                              "elite defense would often slow down the game, leaving fewer possessions for their offense to score.",
                                              "Overall team strength is well measured by,", tags$b("Net Rating"), "and",  tags$b("Offensive Rating"), "and",
                                              tags$b("Defensive Rating"), "show from which area(s) of the game teams may derive their strength."
                                            ),
                                            
                                            p("More on the mathematical methodology behind these ratings can be found", a("here", href= "https://lukebenz.com/post/hoops_methodology/methodology/")
                                            )
                                            
                                          ),
                                          
                                          ### Render Table
                                          mainPanel(
                                            DT::dataTableOutput("rankings")
                                            
                                          )
                            )
                   ),
                   
                   ### Conference Breakdowns
                   tabPanel("Conference Breakdowns",
                            
                            sidebarLayout(
                              sidebarPanel(
                                
                                selectInput("conf", 
                                            label = "Select a Conference",
                                            choices = c(""), 
                                            selected = NULL
                                            
                                )
                              ),
                              
                              
                              mainPanel(
                                ### Render Table
                                DT::dataTableOutput("conf_standings"),
                                
                                ### Render D1 Universe
                                plotOutput("uni_plot", width = "100%", height = "600px"),
                                
                                br(),
                                br(),
                                
                                
                                ### Render Conference Standings Plot
                                plotOutput("conf_standings_plot", width = "100%", height = "600px"),
                                
                                br(),
                                br(),
                                
                                ### Render Conference Box Plots
                                plotOutput("conf_box_plot", width = "100%", height = "600px")
                                
                                
                              )
                              
                            )
                   ),
                   
                   
                   ### Team Breakdowns
                   tabPanel("Team Breakdowns",
                            sidebarLayout(
                              sidebarPanel(
                                
                                selectInput("team", 
                                            label = "Select a Team",
                                            choices = c(""), 
                                            selected = NULL
                                            
                                )
                              ),
                              
                              mainPanel(
                                DT::dataTableOutput("team_schedule"),
                                plotOutput("ratings_plot", width = "100%", height = "600px"),
                                plotOutput("rankings_plot", width = "100%", height = "600px")
                                
                              )
                              
                            )
                   ),
                   
                   ### Game Projections
                   tabPanel("Game Projections",
                            sidebarLayout(
                              sidebarPanel(
                                "Games are sorted by highest rated team.",
                                "Win probability shown is from \"Team\" perspective.",
                                br(),
                                br(),
                                dateInput("proj_date", label = "Select Date", value = NULL,
                                          format = "MM d, yyyy",
                                          min = as.Date("2019-11-05")
                                )
                              ),
                              
                              mainPanel(
                                DT::dataTableOutput("game_projections") 
                              )
                              
                            )
                   ),
                   
                   ### Bracketology
                   tabPanel("Bracketology",
                            DT::dataTableOutput("bracket")
                   ),
                   
                   ### ncaahoopR Graphics
                   tabPanel("ncaahoopR Graphics",
                            "COMING SOON"
                   ),
                   
                   ### About
                   tabPanel("About",
                            
                            
                            sidebarLayout(position = "left",
                                          sidebarPanel(
                                            p(align = "left",
                                              "My name is", tags$b("Luke Benz"), "and I",
                                              "designed this site to put together",
                                              "several of the various college basketball",
                                              "modules I maintain all in on centralized place."),
                                            
                                            p(align = "left",
                                              "Many of these projects began when I was an undergrad",
                                              "participating in the", a("Yale Undergraduate Sports Analytics Group", 
                                                                        href = "http://sports.sites.yale.edu")
                                            ),
                                            
                                            p(align = "left",
                                              "Integrating all of the various modules will take some time",
                                              "but I'm hoping to have everything up and running before too long.",
                                              "Thanks for your patientice in the mean time"),
                                            
                                            p(aligin = "left", 
                                              tags$b(tags$u("Useful Links")),
                                              tags$ul(
                                                tags$li(a("Rankings Methodology", href = "https://lukebenz.com/post/hoops_methodology/methodology/")),
                                                tags$li(a("GitHub Repository", href = "https://github.com/lbenz730/NCAA_Hoops")),
                                                tags$li(a("ncaahoopR Package", href = "https://github.com/lbenz730/ncaahoopR/"))
                                                
                                                
                                              )
                                            ),
                                            
                                            p(align = "left",
                                              "Follow me on twitter", a("@recspecs730", href = "https://twitter.com/recspecs730"),
                                              "for more hoops analysis")
                                            
                                          ),
                                          mainPanel()
                            )
                   )
                   
))
