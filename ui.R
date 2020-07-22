#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(dashboardPage(
    dashboardHeader(title = "Colorado COVID Outcomes\n
                        vs. Demographic Information\n
                        by County"),
    dashboardSidebar(
        # sidebarSearchForm(textId = "searchText", 
        #                   buttonId = "searchButton",
        #                   label = "Search Categories"),
        sidebarMenu(
            menuItem("COVID vs. Demographics", tabName = "Main Dashboard",
                     icon=icon("bar-chart-o")),
            radioButtons(inputId = "COVIDbuttons", 
                         label = "COVID Measure:",
                         choices = c("Tests Per 100000" = 
                                         "COVID.Tests.Per.100000",
                                     "Cases Per 100000" = 
                                         "COVID.Cases.Per.100000",
                                     "Deaths Per 100000" = 
                                         "COVID.Deaths.Per.100000",
                                     "Mortality Percentage" = 
                                         "COVID.Mortality.Perc"),
                         selected = "COVID.Cases.Per.100000"),
            radioButtons(inputId = "catbuttons",
                         label = "Demographic Category:",
                         choices = c("Demographics" =
                                         "CO_DEMOGRAPHICS",
                                     "Healthcare Access" =
                                         "CO_HEALTHCARE_ACCESS",
                                     "Home" = "CO_HOME",
                                     "Income/Education" =
                                         "CO_INCOME_EDUCATION",
                                     "Lifestyle" = "CO_LIFESTYLE",
                                     "Mortality/Morbidity" =
                                         "CO_MORTALITY_MORBIDITY",
                                     "Prior Medical History" =
                                         "CO_PRIOR_MEDICAL",
                                     "Summary Scores" =
                                         "CO_SUMMARY_SCORES"),
                         selected = "CO_HEALTHCARE_ACCESS"),
            selectizeInput(inputId = "DemoData",
                           label = "Demographic Measure:",
                           choices = colnames("catbuttons")[-1:-5])
            # ,
            # menuItem("Additional Charts", tabName = "Race/Ethnicity",
            #          icon=icon("bar-chart-o"))
            # ,
            # selectizeInput(inputId = "RaceDemo",
            #             label = "Demographic Measure for\nRace/Ethnicity Comparisons",
            #             choices = (colnames(
            #                 CO_Race_Measures_COVID19[4:13])))
            # ,
            # selectizeInput(inputId = "single",
            #             label = "County Individual Data",
            #             choices = unique(COVID19ALL_MERGE$COUNTY)[-1])
            )),
   
    dashboardBody(
        #titlePanel("Demographic Measures vs. COVID Outcomes by County"),
        fluidRow(
            mainPanel(
                tabsetPanel(
                    tabPanel("COVID vs. Demographics", 
                             #box(plotOutput("top20"), width=9, height = 420),
                             box(ggiraphOutput("top20"), width=9),
                             box(sliderInput("covidrange", 
                                             "Number of COVID Cases in County:", 
                                             0, 10000, value=c(0,10000), step=100),
                                 sliderInput("medianrange", 
                                             "Median Income Range of County:", 
                                             30000, 130000, value=c(30000,130000),
                                             step=2500),
                                 sliderInput("ruralrange", 
                                             "Rural Population % of County:", 
                                             0, 100, value=c(0,100), step=1), 
                                 width=3),
                             box(tableOutput("combined_data"), height = 130, width=12)
                             #,box(tableOutput("demo_data"), height = 130, width=12)
                             #,box(tableOutput("COVID_data"), height = 130, width=12)
                    )
                    ,tabPanel("Maps",
                              box(plotOutput("bi_map_legend"), height=420, width=5),
                              box(ggiraphOutput("bi_map"), height = 420, width=7),
                              box(ggiraphOutput("covid_map"), height = 400, width=6),
                              box(ggiraphOutput("demo_map"), height = 400, width=6)
                              )
                    ,tabPanel("Correlation Measures",
                              box(ggiraphOutput("bi_cor"), height=420, width=6),
                              box(ggiraphOutput("scatterplot"), height=420, width=6),
                              box(radioButtons(inputId = "balancebuttons",
                                           label = "Select a Balancing Measure \n
                                           for the Map",
                                           choices = c("COVID.Groups",
                                                       "Income.Groups",
                                                       "Rural.Groups"),
                                           selected = "Rural.Groups"), width=3),
                              box(ggiraphOutput("corr_map"), height = 400, width=9)
                    )
                   # ,tabPanel("Stats",
                              
                    ,tabPanel("Race/Ethnicity Comparisons",
                              box(selectizeInput(inputId = "RaceDemo",
                                             label = "Choose a Demographic Category for Race/Ethnicity Comparisons",
                                             choices = (colnames(
                                                 CO_Race_Measures_COVID19[4:13])),
                                             selected = "Preventable.Hospitalization.Rate"),
                                  height=100,width=6),
                              box(plotOutput("Racefacetgrid"),
                                 height = 420, width=12),
                    #          box(sliderInput("covidrange1", 
                    #                          "Number of COVID Cases in County:", 
                    #                          0, 10000, value=c(0,10000), step=100),
                    #              sliderInput("medianrange1", 
                    #                          "Median Income Range of County:", 
                    #                          30000, 130000, value=c(30000,130000),
                    #                          step=2500),
                    #              sliderInput("ruralrange1", 
                    #                          "Rural Population % of County:", 
                    #                          0, 100, value=c(0,100), step=1), 
                    #              width=3),
                             box(tableOutput("Race_Stats"),
                                 width=12)
                    )
                    ,tabPanel("County Individual Data",
                              box(ggiraphOutput("indcounty")),
                              box(selectizeInput(inputId = "single",
                                                 label = "Select a County:",
                                                 choices = unique(COVID19ALL_MERGE$COUNTY)[-1])),
                              box(sliderInput("daterange", 
                                              "Select a Date Range:", 
                                              min = as.Date("2020-03-17"), 
                                              max = as.Date("2020-07-17"), 
                                  value = c(as.Date("2020-03-17"), as.Date("2020-07-17"))
                                  # ,step=1,
                                  ,timeFormat="%B %d"
                                  ))
                              )
                              # dateRangeInput("daterange", "Date range:",
                              #                start  = "2020-03-17",
                              #                end    = "2010-07-17",
                              #                min  = "2020-03-17",
                              #                max    = "2010-07-17",
                              #                format = "MM d",
                              #                separator = " - "))
                    
                    
                    
                    
                ))))
))
