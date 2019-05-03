# Spring 2019 Gov Final Project
# We load the necessary packages that will be used in the application.

library(shiny)
library(tidyverse)
library(shinydashboard)
library(DT)
library(leaflet)
library(ggthemes)
library(plotly)
library(shinythemes)

# Loading the data for the laliga section of our app

laliga_stats <- read_csv("LaLiga_dataset.csv") %>%
    
    # Separating the season column into start and ends
    
    separate(season, c("season_start","season_end"), sep = "-") %>%
    
    # Making sure that season end column has appropriate years for each cases
    
    mutate(season_start = as.numeric(season_start), season_end = as.numeric(season_end) + 1900) %>%
    
    # The case where season started in the 21st century
    
    mutate(season_end = ifelse(season_start >= 2000, season_end + 100, season_end)) %>%
    
    # The case where season started in the 20th century but ended in 21st century
    
    mutate(season_end = ifelse(season_start == 1999, 2000, season_end))

# Loading the EPL results dataset

results <- read_csv("results.csv")

# Loading the EPL stats dataset

stats <- read_csv("stats.csv") %>%
    
    # Separating the season column into start and end
    
    separate(season, c("season_start","season_end"), sep = "-") %>%
    mutate(season_start = as.numeric(season_start), season_end = as.numeric(season_end))

# Define UI for the application
# Using a red theme since Manchester United is my favorite team

ui <- dashboardPage(skin = 'red',
    
    # The overall title of the dashboard 
    
    dashboardHeader(title = "Seeam@Plaantik - Analyzing the EPL",
                    titleWidth = 400),
    
    dashboardSidebar(
        
        # Creating the dashboard navigation meny with necessary icons. 
        
        sidebarMenu(
            menuItem("Home", tabName = "dashboard", icon = icon("home")),
            menuItem("Attack Stats", tabName = "attack", icon = icon("rocket")),
            menuItem("Defence Stats", tabName = "defence", icon = icon("shield-alt")),
            menuItem("La Liga", tabName = "laliga", icon = icon("futbol")),
            menuItem("Fun Facts for Fans", tabName = "funfact", icon = icon("coffee"))
        )
    ),
    dashboardBody(
        tabItems(
            
            # First tab content
            # This provides information on the contents of the app, credits and data used

            tabItem(tabName = "dashboard",

                    h1("Let's analyze the English Premier League!"),
                    
                    # Providing a scavenger hunt for the user
                    
                    tags$div(class = "widget-user-header bg-red-active text-center",                
                        h3("Starting with a Scavenger Hunt for You!"),
                        p("In case you are wondering what my favorite team from the English Premier League is, look no further. The map below has an explicit hint (the marker) on what my favorite team is. Zoom in to check it out. You can move around in the map as well. If you are an English Premier League fan, chances are that you will recognize the name of the stadium the marker is located at to correctly identify my favorite team.")),
                    
                    # output for the map where scavenger hunt can be played
                    
                    leafletOutput("mymap", height = "500"),
                    
                    # Background of the project
                    
                    h3("About the project"),
                    p("Hi, I am Seeam Shahid Noor and I am very happy that you decided to check this project out. I was born and brought up in Dhaka, Bangladesh and Football (aka soccer) has always been a huge part of my life growing up. People outside my country might not know this but the football fan culture is huge in Bangladesh. So, when given the chance to present a data analysis project for the class", tags$a("Gov 1005 at Harvard", http = "https://www.davidkane.info/files/gov_1005_spring_2019.html")," I decided to take it to present some basic but interesting stats on the teams of ", tags$a("the English Premier League,",href = "https://en.wikipedia.org/wiki/Premier_League")," the most watched sports league in the world."),
                    
                    # Content of the project
                    
                    h3("Content"),
                    p("The interesting stats presented have been divided into few categories to make them easy to access and understand. The sections can be accessed from the navigation dashboard on the left. The title of the sections are pretty self-explanatory to indicate what the stats will be about. For instance, 'Attack Stats' will show some interesting stats teams had during attacking plays. Finally, I have also decided to add few interesting stats from the Laliga so some of my best friends, who are fans of Barcelona and Real Madrid, don't miss out."),
                    
                    # Credits to outside collaboration I did to get the app to more people once published
                    # Collaboration was also used to get better ideas
                    
                    h3("Collaboration"),
                    p("The goal was to make the project something English Premier League Fans from Bangladesh would enjoy exploring.

So, at first I took a look at the data available and made a short list of what interesting analysis I could be doing. Then, I reached out to ", tags$a("Plaantik", href = "http://plaantik.com/"),", a platform dedicated to football fans of Bangladesh with over ",   tags$a("81,500 Facebook followers", href = "https://www.facebook.com/plaantik/" ),". Plaantik found the idea of a data analysis project to be interesting and made a public Facebook post asking its followers what type of analysis they would be interested in knowing. The response was overwhelming and it can be found ,", tags$a("here", href = "https://www.facebook.com/plaantik/posts/2370059273026323"),".
The Facebook post was a good way of gauging specific fan interest in analysis that they'd enjoy and provided a very good idea of where I can start. I remain grateful to Plaantik and to every fan who contributed in idea generation. I tried to add a few fun stats in the 'Fun Facts for Fans' section."),
                    
                    # Description of the data
                    
                    h3("Description of Data"),
                    p("The Data has been taken from the",  tags$a("Kaggle profile of Zaeem Nalla", href = "https://www.kaggle.com/zaeemnalla/premier-league#stats.csv"),". The creator of the dataset has made the data publicly available and ready for use in personal projects. The data was acquired from the English Premier League Official website. It contains the match results of all football matches in the ", tags$a("English Premier League", href = "https://www.premierleague.com/"), "seasons starting from 2006/2007 through 2017/2018."),
                    p("Additionally, I will also do a mini data analysis on the Laliga (Spanish Football Leageue) as well. 
The data for that can be found ", tags$a("here.", href = "https://www.kaggle.com/spn007/la-liga-dataset#_=_")),
                    p("We used three data sets for our analysis:
", tags$br(), "1. Results.csv - Results of 4560 Premier League Matches. Dimensions of the data = 4561 X 6
", tags$br(), "2. Stats.csv - Basic statistics collected from Opta (official stats collector of the Premier League) of each team in every season (season totals) from 2006/2007 to 2017/2018.
Dimensions of the data = 204 X 42  
", tags$br(), "3. LaLiga_dataset.csv - Results of all matches of laliga from 1970/1971 to 2016/2017 season.  
Dimensions of the data = 908 X 16")
            ),
            
            # Second tab content
            # This tab contains stats on attacking plays in the EPL
            # We first describe how to use the tool
            
            tabItem(tabName = "attack",
                    
                    h2("Interesting Stats on Offensive Plays"),
                    h3("Displaying the Top Ten for each stat"),
                    tags$div(class = "widget-user-header bg-red-active text-center",
                        h4("How to use this tool:"),
                        p("1. Select a year range to display the stats for", tags$br(), "Note: As we mentioned before we only have data from 2006/2007 season to 2017/2018 season for the EPL", tags$br(), "2. Select the stats that you want to see.", tags$br(), "3. Enjoy the charts", tags$br(), tags$strong("Fascinating how Manchester City has the most goals in counter attacks in the last decade!"))
                        ),
                    
                    # Slider input which takes the range of seasons to include for output
                    
                    box(
                        sliderInput("year_1",
                                    "Premier League Seasons to include:",
                                    min = 2006,
                                    max = 2017,
                                    value = c(2006,2017))
                    
                    # Dropdown input to give user chance to select the stat they want to see
                        
                    ),
                    box(
                        selectInput("attack", "Which attack stat do you want to see:", 
                                    choices =  c("Goals" = "goals",
                                                 "Crossbar Hits" = "hit_woodwork",
                                                 "Free Kicks Scored" = "att_freekick_goal",
                                                 "Penalties Scored" = "att_pen_goal",
                                                 "Counter Attack Goals" = "goal_fastbreak"), selected = "goals"), inline = TRUE
                    ),
                    
                    # Plot output to show a top ten chart based on inputs
                    
                    plotOutput("Plot1", height = 500)
            ),
            
            # Third tab content
            # This shows stats on defence plays in the EPL
            # Providing instructions on how to use the tool
            
            tabItem(tabName = "defence",
                    h2("Interesting Stats on Defensive Plays"),
                    h3("Displaying the Top Ten for each stat"),
                    tags$div(class = "widget-user-header bg-red-active text-center",
                        h4("How to use this tool:"),
                        p("1. Select a year range to display the stats for", tags$br(), "Note: As we mentioned before we only have data from 2006/2007 season to 2017/2018 season for the EPL", tags$br(), "2. Select the stats that you want to see.", tags$br(), "3. Enjoy the charts", tags$br(), tags$strong("Chelsea sure knows how to play dirty!"))
                        ),
                    
                    # Slider input which takes the range of seasons to include for output
                    
                    box(
                        sliderInput("year_2",
                                    "Premier League Seasons to include:",
                                    min = 2006,
                                    max = 2017,
                                    value = c(2006,2017))
                    ),
                    
                    # Dropdown input to give user chance to select the stat they want to see
                    
                    box(
                        selectInput("defence", "Which defence stat do you want to see:", 
                                    choices =  c("Red Cards" = "total_red_card",
                                                 "Clean Sheets" = "clean_sheet",
                                                 "Saves" = "saves",
                                                 "Interceptions" = "interception",
                                                 "Goalline clearances" = "clearance_off_line"), selected = "total_red_card"), inline = TRUE
                    ),
                    
                    # Plot output to show a top ten chart based on inputs
                    
                    plotOutput("Plot2", height = 500)
            ),
            
            # Fourth tab content
            # This tab shows time series on Laliga (Top Spanish Football League)
            # Providing users instructions on how to use the tool
            
            tabItem(tabName = "laliga",
                    h2("Interesting Stats on La Liga"),
                    h3("Displaying an Interactive Time series for a club in Laliga for a stat of Your Choice"),
                    tags$div(class = "widget-user-header bg-red-active text-center",
                        h4("How to use this tool:"),
                        p("1. Select a LaLiga team", tags$br(), "Note: For simplicity, you can only choose a club that has won the Laliga at least once", tags$br(), "2. Select the stats that you want to see.", tags$br(), "3. Hover over the interactivegraph and enjoy the stats!", tags$br(), tags$strong("See how Barcelona and Real Madrid's goals tally had an insane surge in the Messi-Ronaldo era!"))
                        ),
                    
                    # Dropdown menu to choose a club from
                    
                    box(
                        selectInput("club_3", "Choose a club", 
                                    choices =  c("Real Madrid", "Barcelona", "Atletico de Madrid", "Athletic Club", "Deportivo", "Real Sociedad", "Valencia", "Sevilla", "Betis"), selected = "Real Madrid")
                    ),
                    
                    # Dropdown menu to choose a stat to show the time series for
                    
                    box(
                        selectInput("laliga", "Which stat do you want to see:", 
                                    choices =  c("Home wins" = "home_win",
                                                 "Away wins" = "away_win",
                                                 "Total Wins" = "matches_won",
                                                 "Total points" = "points",
                                                 "Goals Scored" = "goals_scored",
                                                 "Goals Conceded" = "goals_conceded"), selected = "goals_scored")
                    ),
                    
                    # Adding a few line breaks to make the page look better
                    
                    br(),
                    br(),
                    br(),
                    br(),
                    hr(),
                    
                    # Plotly output for our interacive time series
                    
                    plotlyOutput("Plot3", height = 500),
                    
                    # Notes to explain the anomalies one might observe on the time series
                    
                    tags$div(class = "widget-user-header bg-aqua-active text-center",
                        h4("Notes"),
                        p("a. We only have data from 1970/1971 season to 2016/2017 season", tags$br(), "b. Each team played 34 matches in the seasons from 1970 - 1985", tags$br(), "c. Each team played 44 matches in the 1986-1987 season", tags$br(), "d. Each team played 38 matches from then onwards")
            )),
            
            # The fifth tab
            # This tab is basically a fun way to explore team performances against all other teams
            # Explaining how to use the tool and about the section below
            
            tabItem(tabName = "funfact",
                    h2("Fun Facts for Fans"),
                    h3("Show a team's home/away performance  against all other teams of the English Premier League"),
                    tags$div(class = "widget-user-header bg-red-active text-center",
                        h4("How to use this tool:"),
                        p("1. Select a major English Premier League team", tags$br(), "2. Select whether you want to see the home/away performance of that team", tags$br(), "3. You can use the search function and sort the columns. Enjoy the table!", tags$br(), "Note: For simplicity, you can only choose a club that has a major fan base in Bangladesh. Furthermore, note that our data only includes Premier League seasons from 2006/2007 to 2017/2018")
                        ),
                    
                    # This tool was inspired by a question from a fan. Providing the story below
                    
                    tags$div(class = "widget-user-header bg-aqua-active text-center",
                        h4("Inspiration for this tool:"),
                        p("A fan from Plaantik asked which team can make it on a cold rainy night at Stoke and I personally thought it was funny and fascinating. Although, it's hard to get data for weather conditions, you can get a simpler answer to that question by choosing a club, choosing Away performance and then seeing how they did against Stoke on the table that appears. Inspiration for Data Science can come from anywhere, so thank you for the question!", tags$br(), tags$strong("Funny how Manchester United's worst home performance is against Manchester City!"))
                        ),
                    
                    # Providing user a dropdown to choose a team
                    
                    box(
                        selectInput("club_4", "Choose a club", 
                                    choices =  c("Manchester United", "Liverpool", "Chelsea", "Arsenal", "Manchester City", "Tottenham Hotspur"), selected = "Manchester United")
                    ),
                    
                    # Providing user a dropdown to choose away or home performance
                    
                    box(
                        selectInput("side", "Which ground performance do you want to see:", 
                                    choices =  c("Home" = "home",
                                                 "Away" = "away"), selected = "home")
                    ),
                    
                    # Output for DT table that is easier to use and has more functionality
                    
                    DT::dataTableOutput("table4")
            )
        )
    )
)

# This is the server component of the app, which takes the inputs and produces outputs

server <- function(input, output) { 
    
    # we will use this reactive data in our second tab
    
    datareact1 <- reactive({
        stats %>%

            # Filtering for seasons user included
                
            filter(season_start >= input$year_1[1] & season_start <= input$year_1[2]) %>%
            
            # Grouping by teams
            
            group_by(team) %>%
            
            # Summing user selected stat of teams across all seasons
            
            summarise(total = sum(!! rlang::sym(input$attack))) %>%
            
            # Arraning data with teams with highest wins on top
            
            arrange(desc(total)) %>%
            
            # Taking the top 10 teams
            
            head(n = 10) %>%
            
            # Turning the teams into factors ordered by wins to make graph look nicer
            
            mutate(team = fct_reorder(team, total))
            
    })
    
    # We will use this reactive data in our third tab 
    
    datareact2 <- reactive({
        stats %>%
            
            # Filtering for seasons user included
            
            filter(season_start >= input$year_2[1] & season_start <= input$year_2[2]) %>%
            
            # Grouping by teams
            
            group_by(team) %>%
            
            # Summing user selected stat of teams across all seasons
            
            summarise(total = sum(!! rlang::sym(input$defence))) %>%
            
            # Arraning data with teams with highest wins on top
            
            arrange(desc(total)) %>%
            
            # Taking the top 10 teams
            
            head(n = 10) %>%
            
            # Turning the teams into factors ordered by wins to make graph look nicer
            
            mutate(team = fct_reorder(team, total))
        
    })
    
    # We will use this reactive data to create our reactive plot for our fourth tab
    
    datareact3 <- reactive({
        laliga_stats %>%
            
            # Filtering for the club user selected
            
            filter(club == input$club_3)
    })
    
    # We will use this reactive plotly plot for our fourth tab
    
    plotreact3 <- reactive({
        
        # Using our reactive to create a plot
        
        datareact3() %>%
            
        # Using plotly to create an interactive plot using user input on y-axis
                
        plot_ly(x = ~season_start, y = ~eval(as.name(input$laliga)),
                
                # Providing descriptive info on hover activity
                
                hoverinfo = 'text',
                text = ~paste('Start of season:', season_start, "<br>",
                              'Stat selected:', eval(as.name(input$laliga)))) %>%
        
        # Making a scatterplot    
            
        add_markers() %>%
            
        # Making the graph simpler and adding labels    
            
        layout(xaxis = list(title = 'Start of Season', showgrid = FALSE),
               yaxis = list(title = 'Selected Stat', showgrid = FALSE)
                )
    
    })
    
    # Creatiing a reactive data frame for our fifth tab
    
    tablereact4 <- reactive({
        
        # We create different tables based on user input
        
        if(input$side == "home")
            {
            
            # Table for home performance
            
            tablehome <-
                results %>%
                
                # Choosing user selected club
                
                filter(home_team == input$club_4) %>%
                
                # Choosing away teams and the results to group
                
                group_by(away_team, result) %>%
                
                # Removing columns we don't need
                
                select(-home_goals, -away_goals) %>%
                
                # Counting performance against each team in each category
                
                count() %>%
                
                # Ungrouping them to be able to spread
                
                ungroup() %>%
                
                # Spreading data to get the table shape we want
                
                spread(key = result, value = n, fill = 0) %>%
                
                # Renaming labels to make them easier to understand
                
                rename(Opponents = away_team, Losses = A, Tie = D, Wins = H)
        }
        else{
            
            # Table for away performance
            
            tableaway <-
                results %>%
                
                # Choosing user selected club
                
                filter(away_team == input$club_4) %>%
                
                # Choosing away teams and the results to group
                
                group_by(home_team, result) %>%
                
                # Removing columns we don't need
                
                select(-home_goals, -away_goals) %>%
                
                # Counting performance against each team in each category
                
                count() %>%
                
                # Ungrouping them to be able to spread
                
                ungroup() %>%
                
                # Spreading data to get the table shape we want
                
                spread(key = result, value = n, fill = 0) %>%
                
                # Renaming labels to make them easier to understand
                
                rename(Opponents = home_team, Losses = H, Tie = D, Wins = A)
        }
    })

    # Plotting output for our 2nd tab
    
    output$Plot1 <- renderPlot({
        
         # Using reactive data
        
         datareact1() %>%
            
            # Plotting total stat for each team
            
            ggplot(aes(x = team, y = total)) +
            
            geom_col() + 
            
            # Flipping the graph to make it niceer
            
            coord_flip() + 
            
            # Providing title, subtitle, axes labels and data source credit
            
            labs(x = NULL,
                 y = "Selected Attack Stat",
                 title = 'Showing Top 10 teams in the English Premier League across included seasons',
                 subtitle = ifelse(datareact1()$total[1] == datareact1()$total[2], paste(datareact1()$team[1], "&",datareact1()$team[2],  "win the race"), paste(datareact1()$team[1], "wins the race")),
                 caption = 'Source: Official Website of English Premier League') +
            
            # Adding appropriate data labels to make the graph look nicer
            
            geom_text(aes(label = total, color = '#a00901', size = 3, fontface = 'bold', hjust = 1.1), show.legend = FALSE) +
            
            # Using a minimal theme with white background
            
            theme_minimal() +
            
            # Removing all sorts of gridlines
            
            theme(
                panel.grid.major.y = element_blank(),
                panel.grid.minor.y = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank()
            )
     })
    
    # Plotting output for 3rd tab
    
    output$Plot2 <- renderPlot({
        
        # Using reactive data
        
        datareact2() %>%
            
            # Plotting total stats for each team
            
            ggplot(aes(x = team, y = total)) +
            geom_col() + 
            
            # Flipping the graph to make it niceer
            
            coord_flip() + 
            
            # Providing title, subtitle, axes labels and data source credit
            
            labs(x = NULL,
                 y = "Selected Defence Stat",
                 title = 'Showing Top 10 teams in the English Premier League across included seasons',
                 subtitle = ifelse(datareact2()$total[1] == datareact2()$total[2], paste(datareact2()$team[1], "&",datareact2()$team[2],  "win the race"), paste(datareact2()$team[1], "wins the race")),
                 caption = 'Source: Official Website of English Premier League') +
            
            #Adding appropriate data labels to make the graph look nicer
            
            geom_text(aes(label = total, color = '#a00901', size = 3, fontface = 'bold', hjust = 1.1), show.legend = FALSE) +
            
            # Using a minimal theme with white background
            
            theme_minimal() +
            
            # Removing all sorts of gridlines
            
            theme(
                panel.grid.major.y = element_blank(),
                panel.grid.minor.y = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank()
            ) 
    })    
    
    # Plotting a plotly plot for 4th tab
    
    output$Plot3 <- renderPlotly({
        plotreact3()
    })
    
    # Producing a DT table for 5th tab
    
    output$table4 <- DT::renderDataTable({
        tablereact4()
    })

    # Creating the map to be used on home page (1st tab)
        
    output$mymap <- renderLeaflet({
        leaflet() %>%
            addTiles() %>%
            
            # Making sure it has zoom in feature on the location I want
            
            setView(lng = -2.2913, lat = 53.4631, zoom = 4) %>%
            
            # Adding a marker to the location I want
            
            addMarkers(lng = -2.2913, lat = 53.4631 )
    })
    
}

# Running the app

shinyApp(ui, server)