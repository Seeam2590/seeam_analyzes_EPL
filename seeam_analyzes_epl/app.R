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

# # Here, we load the datasets that will be used for the dashboard.
# # This data has all the top partners for each year to be used in table and graph.
# partners <- read_rds("top_partners_simp.rds")
# 
# # This data has information on most frequently exported commodities by year. 
# commodities <- read_rds("commodity_totals.rds")
# 
# # This data has information on what fraction of imports India takes up with its exports
# # for a specific commodity in a specific country.
# alpha_two <- read.csv("Alphas2.csv")
# 
# # Here, we add a new column that assigns a rank to each country based on how important
# # the country is as an export partner. We get this from the position of the country in overall exports.
# # Later in the code, we use this rank to decide whether or not to show country on the graph. 
# # This is important for the numerical slider input. 
# alpha_two <- alpha_two %>%
#     mutate(newcol = case_when(Import.Country == "United States of America" ~ 1,
#                               Import.Country == "United Arab Emirates" ~ 2,
#                               Import.Country == "China, Hong Kong SAR" ~ 3,
#                               Import.Country == "China" ~ 4,
#                               Import.Country == "Singapore" ~ 5,
#                               Import.Country == "United Kingdom" ~ 6,
#                               Import.Country == "Germany" ~ 7,
#                               Import.Country == "Netherlands" ~ 8,
#                               Import.Country == "Belgium" ~ 9,
#                               Import.Country == "Japan" ~ 10,
#                               Import.Country == "South Africa" ~ 11,
#                               Import.Country == "Turkey" ~ 12,
#                               Import.Country == "Italy" ~ 13,
#                               Import.Country == "France" ~ 14,
#                               Import.Country == "Malaysia" ~ 15,
#                               Import.Country == "Spain" ~ 16))

# Define UI for the application
ui <- dashboardPage(
    # The overall title of the dashboard 
    dashboardHeader(title = "Seeam & EPL"),
    dashboardSidebar(
        # There are a total of four main tabs, followed by a snapshot for a commodity. 
        # I added interesting icons to match the topics of the tabs. 
        sidebarMenu(
            menuItem("Home", tabName = "dashboard", icon = icon("home")),
            menuItem("Attack Stats", tabName = "graphs", icon = icon("rocket")),
            menuItem("Defence Stats", tabName = "exports", icon = icon("shield-alt")),
            menuItem("Set Pieces", tabName = "commodities", icon = icon("bullseye")),
            menuItem("La Liga", tabName = "snapshot", icon = icon("futbol")),
            menuItem("Fun Facts for Fans", tabName = "snapshot2", icon = icon("coffee"))
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
Dimensions of the data = 908 X 16"),
                    h3("Fun Fact: Scavenger Hunt for You!"),
                    p("In case you are wondering what my favorite team from the English Premier League is, look no further. The map below has an explicit hint (the marker) on what my favorite team is. Zoom in to check it out. You can move around in the map as well. If you are a EPL fan, chances are that you will recognize the name of the stadium the marker is located at to correctly identify my favorite team."),
                    leafletOutput("mymap", height = "500")
            ),
            
            # Second tab content
            # On this tab, the user chooses which year they want to see.
            # After selecting the year, they see a bar graph of the top India export partners.
            # There is a select height for the graph. 
            tabItem(tabName = "graphs",
                    h4("India Top Export Partners"),
                    h6("India's largest export partners stayed relatively constant over the four year period: 
                 the United States of America, United Arab Emirates, Hong Kong, and China occupied the top four positions each year. 
                 Saudi Arabia, Singapore, and the United Kingdom were also consistently among top export partners.
                 In 2014-2016, Both the USA and UAE had over double the amount of imports from India than any other partner."),
                    box(
                        selectInput("year_choice", "Year:", 
                                    choices = c(2014, 2015, 2016, 2017)), inline = TRUE
                    )#,
                    #plotOutput("distPlot2", height = 275)
            ),
            
            # Third tab content
            # This tab includes the information from the first tab (top exporters),
            # however, it has a function where people can search by country name.
            # Also, unlike the previous tab which only shows top, this continues the list. 
            tabItem(tabName = "exports",
                    h4("India Top Export Partners"),
                    box(
                        selectInput("year_choice2", "Year:", 
                                    choices = c(2014, 2015, 2016, 2017)),
                        helpText("Data from United Nations COMTRADE Database.")
                    )#,
                    #DT::dataTableOutput("distPlot")
            ),
            
            # Fourth tab content
            # This tab allows users to see the top exported commodity items by year
            # This requires a new dataset with merged HS commodity names (see analysis file)
            tabItem(tabName = "commodities",
                    h4("India Top Commodities"),
                    h6("The top export commodities for India over the four year period remained constant.
                 Mineral fuels and pearls (diamonds, other gems) were most important, followed by vehicles, nuclear products and chemical goods.
                 However, 2014 had a larger trade value of exports than the following years."),
                    box(
                        selectInput("year_choice3", "Year:",
                                    choices = c(2014, 2015, 2016, 2017)),
                        helpText("Commodities below sorted by two-digit HS code.")
                    )#,
                    #DT::dataTableOutput("commodityPlot")
            ),
            
            # Fifth tab content
            # This tab also requires a new dataset, specifically one on India's fraction of exports.
            # It provides detailed information on the fraction of each country's iron/steel exports
            # that are provided by India, and how this fraction changed over time. 
            # By playing with the slider, users can change how many of the top export partners are chosen.
            tabItem(tabName = "snapshot",
                    h5("Iron and Steel Exports"),
                    h6("The graph below plots the share India has for a given country's imports
                 of iron and steel, for each month between January 2014 and August 2018.
                 The higher the fraction, the more important India's exports are for that country
                 in the given month period."), 
                    h6("I chose this commodity good because iron and steel are relatively homogenous goods
                 which these countries can get from a variety of sources, so looking at how much they choose
                 to get from India over time is informative of the quality of relationship.
                 During the latter half of 2017, Hong Kong got a higher fraction of its iron and steel from India."),
                    box(
                        sliderInput("countries",
                                    "Number of Countries:",
                                    min = 1,
                                    max = 15,
                                    value = 10)
                    )#,
                    #plotOutput("complot1", height = 300)
            ),
            
            # We provide another snapshot of a different commodity, this time coffee/tea
            # We provide some background info on the good.
            # The functionality is the same. 
            tabItem(tabName = "snapshot2",
                    h5("Coffee and Tea Exports"),
                    h6("The graph below plots the share India has for a given country's imports
                 of coffee, tea, mate, and spices for each month between January 2014 and August 2018.
                 The higher the fraction, the more important India's exports are for that country
                 in the given month period. The user can select how many countries he or she
                 would like to see."),
                    h6("I chose this commodity good as India is a large exporter of tea (famous for Darjeeling Tea)
                 and I was interested in which countries rely on India for breakfast! In 2017, the UAE and UK
                 got a high fraction of this commodity from India, compared to other large partners. Trade partners 
                 Japan and Malaysia, although smaller overall importers from India, both have substantial shares of their
                 imports for this commodity coming from India, consistently above 20% in the case of Malaysia."),
                    box(
                        sliderInput("countries2",
                                    "Number of Countries:",
                                    min = 1,
                                    max = 15,
                                    value = 10)
                    )#,
                    #plotOutput("complot2", height = 300)
            )
        )
    )
)
# This is the server component of the app, which takes the inputs into account. 
server <- function(input, output) { 
    
    # The user chooses a year from the selection and the partners are ranked for that year.
    # datareact <- reactive({
    #     partners %>% 
    #         filter(year == input$year_choice2) %>%
    #         arrange(desc(yearly_total))
    # })
    # 
    # # Like above, user chooses a year and the partners are ranked. 
    # # There is no interest in seeing whole world, only by country .
    # # Only the top 10 partners are relevant for the graph. 
    # datareact2 <- reactive({
    #     partners %>% 
    #         filter(year == input$year_choice) %>%
    #         filter(partner != "World") %>%
    #         arrange(desc(yearly_total)) %>%
    #         head(10)
    # })
    # 
    # # Like above, user chooses a year and now the commodities are ranked in order.
    # # The order is size of total commodity exports in billions
    # datareact3 <- reactive({
    #     commodities %>% 
    #         filter(year == input$year_choice3) %>%
    #         arrange(desc(commodity_total))
    # })
    # 
    # # Here, the user selects how many countries they would like to see.
    # # We ranked the countries above in the variable newcol.
    # # Here, you select only a certain number of the countries (top x number)
    # datareact4 <- reactive({
    #     alpha_two %>%
    #         filter(newcol <= input$countries)
    # })
    # 
    # # Same functionality as above for coffee and tea exports
    # datareact5 <- reactive({
    #     alpha_two %>%
    #         filter(newcol <= input$countries2)
    # })
    # 
    # # We do not want to show the "World" partner only the individual countries. 
    # # We also add appropriate column names. 
    # output$distPlot <- DT::renderDataTable({
    #     datareact() %>%
    #         filter(partner != "World") %>%
    #         datatable(colnames = c('Year', 'Trade Partner', 'Trade Value (Billions USD)'))
    # })
    # 
    # # We add appropriate names for column of table.
    # # We include the commodity code and simplified name for reference. 
    # output$commodityPlot <- DT::renderDataTable({
    #     datareact3() %>%
    #         datatable(colnames = c('Year', 'Commodity', 'Name', 'Trade Value (Billions USD)')) 
    # })
    # 
    # # This is the barchart for the first tab of the graph
    # # We choose a color matching India's flag colors orange and green
    # # We make some adjustments to make the labels of the data and bars readable
    # # We also remove the gridlines to make the graph better looking. 
    # output$distPlot2 <- renderPlot({
    #     datareact2() %>%
    #         ggplot( 
    #             aes(x = reorder(partner, -yearly_total), 
    #                 y = yearly_total)) + 
    #         geom_bar(stat="identity", color = "green", fill = "orangered2") + 
    #         geom_text(aes(label = yearly_total), vjust = 1.5, colour = "white", size = 4) +
    #         labs(x = NULL, y = "Trade Value (Billions USD)") + 
    #         theme(axis.text.x = element_text(angle = 60, hjust = 1), 
    #               panel.grid.major = element_blank(),
    #               panel.grid.minor = element_blank()) 
    # })
    # 
    # # This is the graph for the snapshot of iron and steel exports
    # # We are only looking at the commodity for Iron and Steel
    # # We add labels for the x and y axis that are necessary.
    # output$complot1 <- renderPlot({
    #     datareact4() %>%
    #         filter(!Alpha > 1) %>%
    #         mutate(Month = as.Date(paste("01", Month,sep = "-"),"%d-%b-%y")) %>%
    #         filter(Commodity == "Iron and steel") %>%
    #         ggplot(aes(x = Month, y = Alpha, color = Import.Country)) + geom_point() +
    #         xlab("Date") +
    #         ylab("Fraction of Destination Imports from India") +
    #         ggtitle("Iron and Steel") +
    #         labs(caption = "Export Data from United Nations COMTRADE Database") +
    #         scale_colour_discrete(name = "Import Country")
    # })
    # 
    # # This is the graph for the snapshot of coffee/tea exports
    # # We are only looking at the commodity for Coffee and tea
    # # We add labels for the x and y axis that are necessary.
    # # We use the same formatting from the previous page.
    # output$complot2 <- renderPlot({
    #     datareact5() %>%
    #         filter(!Alpha > 1) %>%
    #         mutate(Month = as.Date(paste("01", Month,sep = "-"),"%d-%b-%y")) %>%
    #         filter(Commodity == "Coffee, tea, mate and spices") %>%
    #         ggplot(aes(x = Month, y = Alpha, color = Import.Country)) + geom_point() +
    #         xlab("Date") +
    #         ylab("Fraction of Destination Imports from India") +
    #         ggtitle("Coffee, tea, mate and spices") +
    #         labs(caption = "Export Data from United Nations COMTRADE Database") +
    #         scale_colour_discrete(name = "Import Country")
    # })
    # 
    # This map is just for aesthetic effect for the dashboard.
    output$mymap <- renderLeaflet({
        leaflet() %>%
            addTiles() %>%
            setView(lng = -3.4360, lat = 55.3781, zoom = 4) %>%
            addMarkers(lng = -2.2913, lat = 53.4631 )
    })
    
}

shinyApp(ui, server)