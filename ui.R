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
                     icon=icon("dashboard")),
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
            # radioButtons(inputId = "catbuttons",
            #              label = "Demographic Category:",
            #              choices = c("CO_DEMOGRAPHICS",
            #                          "CO_HEALTHCARE_ACCESS",
            #                          "CO_HOME",
            #                          "CO_INCOME_EDUCATION", 
            #                          "CO_LIFESTYLE", 
            #                          "CO_MORTALITY_MORBIDITY", 
            #                          "CO_PRIOR_MEDICAL", 
            #                          "CO_SUMMARY_SCORES")),
            selectizeInput(inputId = "DemoData",
                           label = "Demographic Measure:",
                           choices = colnames("catbuttons")[-1:-5]),
            menuItem("Additional Charts", tabName = "Race/Ethnicity",
                     icon=icon("bar-chart-o")),
            selectInput(inputId = "RaceDemo",
                        label = "Race/Ethnicity",
                        choices = (colnames(
                            CO_Race_Measures_COVID19[4:13]))),
            selectInput(inputId = "single",
                        label = "County Individual Data",
                        choices = unique(CO_COUNTY_COVID_FILTER$COUNTY)
            ))),
    
    dashboardBody(
        fluidRow(
            mainPanel(
                tabsetPanel(
                    tabPanel("COVID vs. Demographics", 
                             #box(plotOutput("Map"), height = 350),
                             box(plotOutput("top20"), width=9),
                             #box(plotOutput("bottom20"), height = 300),
                             box(sliderInput("covidrange", 
                                             "Number of COVID Cases in County:", 
                                             0, 10000, value=c(0,10000)),
                                 sliderInput("medianrange", 
                                             "Median Income Range of County:", 
                                             30000, 130000, value=c(30000,130000)),
                                 sliderInput("ruralrange", 
                                             "Rural Population % of County:", 
                                             0, 100, value=c(0,100)), 
                                 width=3),
                             box(tableOutput("demo_data"), height = 130, width=9),
                             box(tableOutput("COVID_data"), height = 130, width=9))
                    ,tabPanel("Race/Ethnicity",
                             box(plotOutput("Racefacetgrid"),
                                 height = 420, width=9),
                             box(sliderInput("covidrange",
                                             "Number of COVID Cases in County:",
                                             0, 7000, value=c(0,7000)),
                                 sliderInput("medianrange",
                                             "Median Income Range of County:",
                                             0, 200000, value=c(0,200000)),
                                 sliderInput("ruralrange", 
                                             "Rural Population Percentage of County:", 
                                             0, 100, value=c(0,100)),
                                 width=3),
                             box(tableOutput("Race_Stats"),
                                 width=9)
                    )
                    ,tabPanel("County Individual Data")
                    
                    
                    
                    
                ))))
))