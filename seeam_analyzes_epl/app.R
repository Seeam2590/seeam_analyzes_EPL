# This is the dashboard for the application, which houses five tabs.
# Each of the five tabs includes different information on India's export sector.
# Fall 2018 Gov Final Project
# Here, we load the necessary libraries that will be used in the application. 
library(shiny)
library(tidyverse)
library(shinydashboard)
library(dplyr)
library(DT)
library(leaflet)
library(rgdal)
library(scales)
library(ggthemes)
library(plotly)

# This data has information on what fraction of imports India takes up with its exports
# for a specific commodity in a specific country.

laliga_stats <- read_csv("LaLiga_dataset.csv") %>%
    separate(season, c("season_start","season_end"), sep = "-") %>%
    mutate(season_start = as.numeric(season_start), season_end = as.numeric(season_end) + 1900) %>%
    mutate(season_end = ifelse(season_start >= 2000, season_end + 100, season_end)) %>%
    mutate(season_end = ifelse(season_start == 1999, 2000, season_end))

results <- read_csv("results.csv")

stats <- read_csv("stats.csv") %>%
    separate(season, c("season_start","season_end"), sep = "-") %>%
    mutate(season_start = as.numeric(season_start), season_end = as.numeric(season_end))

# Define UI for the application
ui <- dashboardPage(
    # The overall title of the dashboard 
    dashboardHeader(title = "Seeam & EPL"),
    dashboardSidebar(
        # There are a total of four main tabs, followed by a snapshot for a commodity. 
        # I added interesting icons to match the topics of the tabs. 
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
            # This provides information on the contents of the app.
            # Also, provides info on data resource and some basic summary stats on trade.
            # Finally, just for the effect, I included a graph on the page that looks nice.
            # Including a graph is nice as we are looking at international trade data, but this is just for aesthetics.
            tabItem(tabName = "dashboard",
                    h1("Let's analyze the English Premier League!"),
                
                    h3("Starting with a Scavenger Hunt for You!"),
                    p("In case you are wondering what my favorite team from the English Premier League is, look no further. The map below has an explicit hint (the marker) on what my favorite team is. Zoom in to check it out. You can move around in the map as well. If you are an English Premier League fan, chances are that you will recognize the name of the stadium the marker is located at to correctly identify my favorite team."),
                    leafletOutput("mymap", height = "500"),
                    h3("About the project"),
                    p("Hi, I am Seeam Shahid Noor and I am very happy that you decided to check this project out. I was born and brought up in Dhaka, Bangladesh and Football (aka soccer) has always been a huge part of my life growing up. People outside my country might not know this but the football fan culture is huge in Bangladesh. So, when given the chance to present a data analysis project for the class", tags$a("Gov 1005 at Harvard", http = "https://www.davidkane.info/files/gov_1005_spring_2019.html")," I decided to take it to present some basic but interesting stats on the teams of ", tags$a("the English Premier League,",href = "https://en.wikipedia.org/wiki/Premier_League")," the most watched sports league in the world"),
                    h3("Content"),
                    p("The interesting stats presented have been divided into few categories to make them easy to access and understand. The sections can be accessed from the navigation dashboard on the left. The title of the sections are pretty self-explanatory to indicate what the stats will be about. For instance, 'Attack Stats' will show some interesting stats teams had during atacking plays. Finally, I have also decided to add few interesting stats from the Laliga so some of my best friends, who are fans of Barcelona and Real Madrid, doesn't miss out."),
                    h3("Collaboration"),
                    p("The goal was to make the project something English Premier League Fans from Bangladesh would enjoy exploring.

So, at first I took a look at the data available and made a short list of what interesting analysis I could be doing. Then, I reached out to ", tags$a("Plaantik", href = "http://plaantik.com/"),", a platform dedicated to football fans of Bangladesh with over ",   tags$a("81,500 Facebook followers", href = "https://www.facebook.com/plaantik/" ),". Plaantik found the idea of a data analysis project to be interesting and made a public Facebook post asking its followers what type of analysis they would be interested in knowing. The response was overwhelming and it can be found ,", tags$a("here", href = "https://www.facebook.com/plaantik/posts/2370059273026323"),".
The Facebook post was a good way of gauging specific fan interest in analysis that they'd enjoy and provided a very good idea of where I can start. I remain grateful to Plaantik and to every fan who contributed in idea generation. I tried to add a few fun stats in the 'Fun Facts for Fans' section."),
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
            # On this tab, the user chooses which year they want to see.
            # After selecting the year, they see a bar graph of the top India export partners.
            # There is a select height for the graph. 
            tabItem(tabName = "attack",
                    h2("Interesting Stats on Offensive Plays"),
                    h3("Displaying the Top Ten for each stat"),
                    h4("How to use this tool:"),
                    p("1. Select a year range to display the stats for", tags$br(), "Note: As we mentioned before we only have data from 2006/2007 season to 2017/2018 season for the EPL", tags$br(), "2. Select the stats that you want to see.", tags$br(), "3. Enjoy the charts"),
                    box(
                        sliderInput("year_1",
                                    "Premier League Seasons to include:",
                                    min = 2006,
                                    max = 2017,
                                    value = c(2006,2017))
                    ),
                    box(
                        selectInput("attack", "Which attack stat do you want to see:", 
                                    choices =  c("Goals" = "goals",
                                                 "Crossbar Hits" = "hit_woodwork",
                                                 "Free Kicks Scored" = "att_freekick_goal",
                                                 "Penalties Scored" = "att_pen_goal",
                                                 "Counter Attack Goals" = "goal_fastbreak"), selected = "goals"), inline = TRUE
                    ),
                    plotOutput("Plot1", height = 500)
                    #DT::dataTableOutput("commodityPlot")
            ),
            
            # Third tab content
            # This tab includes the information from the first tab (top exporters),
            # however, it has a function where people can search by country name.
            # Also, unlike the previous tab which only shows top, this continues the list. 
            tabItem(tabName = "defence",
                    h2("Interesting Stats on Defensive Plays"),
                    h3("Displaying the Top Ten for each stat"),
                    h4("How to use this tool:"),
                    p("1. Select a year range to display the stats for", tags$br(), "Note: As we mentioned before we only have data from 2006/2007 season to 2017/2018 season for the EPL", tags$br(), "2. Select the stats that you want to see.", tags$br(), "3. Enjoy the charts"),
                    box(
                        sliderInput("year_2",
                                    "Premier League Seasons to include:",
                                    min = 2006,
                                    max = 2017,
                                    value = c(2006,2017))
                    ),
                    box(
                        selectInput("defence", "Which defence stat do you want to see:", 
                                    choices =  c("Red Cards" = "total_red_card",
                                                 "Clean Sheets" = "clean_sheet",
                                                 "Saves" = "saves",
                                                 "Interceptions" = "interception",
                                                 "Goalline clearances" = "clearance_off_line"), selected = "total_red_card"), inline = TRUE
                    ),
                    plotOutput("Plot2", height = 500)
                    #DT::dataTableOutput("commodityPlot")
            ),
            
            # Fourth tab content
            # This tab also requires a new dataset, specifically one on India's fraction of exports.
            # It provides detailed information on the fraction of each country's iron/steel exports
            # that are provided by India, and how this fraction changed over time. 
            # By playing with the slider, users can change how many of the top export partners are chosen.
            tabItem(tabName = "laliga",
                    h2("Interesting Stats on La Liga"),
                    h3("Displaying an Interactive Time series for a club in Laliga for a stat of Your Choice"),
                    h4("How to use this tool:"),
                    p("1. Select a LaLiga team", tags$br(), "Note: For simplicity, you can only choose a club that has won the Laliga at least once", tags$br(), "2. Select the stats that you want to see.", tags$br(), "3. Hover over the interactivegraph and enjoy the stats!"),
                    box(
                        selectInput("club_3", "Choose a club", 
                                    choices =  c("Real Madrid", "Barcelona", "Atletico de Madrid", "Athletic Club", "Deportivo", "Real Sociedad", "Valencia", "Sevilla", "Betis"), selected = "Real Madrid")
                    ),
                    box(
                        selectInput("laliga", "Which stat do you want to see:", 
                                    choices =  c("Home wins" = "home_win",
                                                 "Away wins" = "away_win",
                                                 "Total Wins" = "matches_won",
                                                 "Total points" = "points",
                                                 "Goals Scored" = "goals_scored",
                                                 "Goals Conceded" = "goals_conceded"), selected = "goals_scored")
                    ),
                    br(),
                    br(),
                    br(),
                    br(),
                    hr(),
                    plotlyOutput("Plot3", height = 500),
                    h4("Notes"),
                    p("a. We only have data from 1970/1971 season to 2016/2017 season", tags$br(), "b. Each team played 34 matches in the seasons from 1970 - 1985", tags$br(), "c. Each team played 44 matches in the 1986-1987 season", tags$br(), "d. Each team played 38 matches from then onwards")
                    #DT::dataTableOutput("commodityPlot")
            ),
            
            # We provide another snapshot of a different commodity, this time coffee/tea
            # We provide some background info on the good.
            # The functionality is the same. 
            tabItem(tabName = "funfact",
                    h2("Fun Facts for Fans"),
                    h3("Show a team's home/away performance  against all other teams of the English Premier League"),
                    h4("How to use this tool:"),
                    p("1. Select a major English Premier League team", tags$br(), "2. Select whether you want to see the home/away performance of that team", tags$br(), "3. You can use the search function and sort the columns. Enjoy the table!", tags$br(), "Note: For simplicity, you can only choose a club that has a major fan base in Bangladesh. Furthermore, note that our data only includes Premier League seasons from 2006/2007 to 2017/2018"),
                    h4("Inspiration for this tool:"),
                    p("A fan from Plaantik asked which team can make it on a cold rainy night at Stoke and I personally thought it was funny and fascinating. Although, it's hard to get data for weather conditions, you can get a simpler answer to that question by choosing a club, choosing Away performance and then seeing how they did against Stoke on the table that appears. Inspiration for Data Science can come from anywhere, so thank you for the question!"),
                    box(
                        selectInput("club_4", "Choose a club", 
                                    choices =  c("Manchester United", "Liverpool", "Chelsea", "Arsenal", "Manchester City", "Tottenham Hotspur"), selected = "Manchester United")
                    ),
                    box(
                        selectInput("side", "Which ground performance do you want to see:", 
                                    choices =  c("Home" = "home",
                                                 "Away" = "away"), selected = "home")
                    ),
                    DT::dataTableOutput("table4")
            )
        )
    )
)
# This is the server component of the app, which takes the inputs into account. 
server <- function(input, output) { 
    
    datareact1 <- reactive({
        stats %>%
            filter(season_start >= input$year_1[1] & season_start <= input$year_1[2]) %>%
            # Grouping by teams
            group_by(team) %>%
            # Summing wins of teams across all seasons
            summarise(total = sum(!! rlang::sym(input$attack))) %>%
            # Arraning data with teams with highest wins on top
            arrange(desc(total)) %>%
            # Taking the top 10 teams
            head(n = 10) %>%
            # Turning the teams into factors ordered by wins to make graph look nicer
            mutate(team = fct_reorder(team, total))
            
    })
    # 
    # # Like above, user chooses a year and the partners are ranked. 
    # # There is no interest in seeing whole world, only by country .
    # # Only the top 10 partners are relevant for the graph. 
    datareact2 <- reactive({
        stats %>%
            filter(season_start >= input$year_2[1] & season_start <= input$year_2[2]) %>%
            # Grouping by teams
            group_by(team) %>%
            # Summing wins of teams across all seasons
            summarise(total = sum(!! rlang::sym(input$defence))) %>%
            # Arraning data with teams with highest wins on top
            arrange(desc(total)) %>%
            # Taking the top 10 teams
            head(n = 10) %>%
            # Turning the teams into factors ordered by wins to make graph look nicer
            mutate(team = fct_reorder(team, total))
        
    })
    # # Like above, user chooses a year and now the commodities are ranked in order.
    # # The order is size of total commodity exports in billions
    datareact3 <- reactive({
        laliga_stats %>%
            filter(club == input$club_3)
    })
    
    plotreact3 <- reactive({
        datareact3() %>%
        plot_ly(x = ~season_start, y = ~eval(as.name(input$laliga)),
                hoverinfo = 'text',
                text = ~paste('Start of season:', season_start, "<br>",
                              'Stat selected:', eval(as.name(input$laliga)))) %>%
        add_markers() %>%
        layout(xaxis = list(title = 'Start of Season', showgrid = FALSE),
               yaxis = list(title = 'Selected Stat', showgrid = FALSE)
                )
    
    })
    
    tablereact4 <- reactive({
        if(input$side == "home")
            {
            tablehome <-
                results %>%
                filter(home_team == input$club_4) %>%
                group_by(away_team, result) %>%
                select(-home_goals, -away_goals) %>%
                count() %>%
                ungroup() %>%
                spread(key = result, value = n, fill = 0) %>%
                rename(Opponents = away_team, Losses = A, Tie = D, Wins = H)
        }
        else{
            tableaway <-
                results %>%
                filter(away_team == input$club_4) %>%
                group_by(home_team, result) %>%
                select(-home_goals, -away_goals) %>%
                count() %>%
                ungroup() %>%
                spread(key = result, value = n, fill = 0) %>%
                rename(Opponents = home_team, Losses = H, Tie = D, Wins = A)
        }
    })

    output$Plot1 <- renderPlot({
         datareact1() %>%
            # Plotting total wins for each team
            ggplot(aes(x = team, y = total)) +
            geom_col() + 
            # Flipping the graph to make it niceer
            coord_flip() + 
            # Providing title, subtitle, axes labels and data source credit
            labs(x = NULL,
                 y = "Selected Attack Stat",
                 title = 'Showing Top 10 teams in the English Premier League across included seasons',
                 subtitle = ifelse(datareact1()$total[1] == datareact1()$total[2], paste(datareact1()$team[1], "&",datareact1()$team[2],  "wins the race"), paste(datareact1()$team[1], "wins the race")),
                 caption = 'Source: Official Website of English Premier League') +
            # # Adding appropriate data labels to make the graph look nicer
            geom_text(aes(label = total, color = '#a00901', size = 3, fontface = 'bold', hjust = 1.1), show.legend = FALSE) +
            theme_minimal() +
            theme(
                panel.grid.major.y = element_blank(),
                panel.grid.minor.y = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank()
            )
     })
    # # We add labels for the x and y axis that are necessary.
    output$Plot2 <- renderPlot({
        datareact2() %>%
            # Plotting total wins for each team
            ggplot(aes(x = team, y = total)) +
            geom_col() + 
            # Flipping the graph to make it niceer
            coord_flip() + 
            # Providing title, subtitle, axes labels and data source credit
            labs(x = NULL,
                 y = "Selected Defence Stat",
                 title = 'Showing Top 10 teams in the English Premier League across included seasons',
                 subtitle = ifelse(datareact2()$total[1] == datareact2()$total[2], paste(datareact2()$team[1], "&",datareact2()$team[2],  "wins the race"), paste(datareact2()$team[1], "wins the race")),
                 caption = 'Source: Official Website of English Premier League') +
            # # Adding appropriate data labels to make the graph look nicer
            geom_text(aes(label = total, color = '#a00901', size = 3, fontface = 'bold', hjust = 1.1), show.legend = FALSE) +
            theme_minimal() +
            theme(
                panel.grid.major.y = element_blank(),
                panel.grid.minor.y = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank()
            ) 
    })    
    output$Plot3 <- renderPlotly({
        plotreact3()
    })
    
    output$table4 <- DT::renderDataTable({
        tablereact4()
    })
    
    output$mymap <- renderLeaflet({
        leaflet() %>%
            addTiles() %>%
            setView(lng = -2.2913, lat = 53.4631, zoom = 4) %>%
            addMarkers(lng = -2.2913, lat = 53.4631 )
    })
    
}

shinyApp(ui, server)