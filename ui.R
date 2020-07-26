#lintr can help clean code --> lintr::lint("server.R")

library(shiny)

# Define UI for application that draws a histogram
shinyUI(
    dashboardPage(
        # theme --> semantic.dashboard, skins from SHINY website
    dashboardHeader(title = "Colorado COVID"),
    dashboardSidebar(
        # sidebarSearchForm(textId = "searchText", 
        #                   buttonId = "searchButton",
        #                   label = "Search Categories"),
        sidebarMenu(
            menuItem("A Shiny App by David Gottlieb",
                     #          label="Colorado COVID Data was sourced 
                     # from the CDPHE and Demographic Datasets were sourced from the CDPHE Community-Level Health Outcome Estimates,
                     #  RWJ 2020 County Health Rankings, and BRFSS 2014-2017 County Datasets.", 
                     # tabName = "About",
                     # icon=icon("list-alt")
                     icon=NULL, href = "https://twitter.com/datatodavid"
            ),
            # menuItem("twitter.com/datatodavid", icon=NULL),
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
                                     "Positivity Percentage" = 
                                         "COVID.Positivity.Perc",
                                     "Mortality Percentage" = 
                                         "COVID.Mortality.Perc"),
                         selected = "COVID.Cases.Per.100000"),
            selectizeInput(inputId = "catbuttons",
                         label = "Demographic Category List",
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
                           choices = colnames("catbuttons")[-1:-6]),
            
            menuItem("Source code", icon = icon("file-code-o"), 
                     href = "https://github.com/rstudio/shinydashboard/"),
            menuItem("CDPHE CO COVID Data", icon = icon("list-alt"), 
                     href = "https://data-cdphe.opendata.arcgis.com/search?collection=Dataset&q=covid19&sort=name"),
            menuItem("CDPHE Community Data", icon = icon("list-alt"), 
                     href = "https://data-cdphe.opendata.arcgis.com/search?collection=Dataset&q=cdphe%20community%20level%20estimates%20(county)%20"),
            menuItem("BRFSS County Data", icon = icon("list-alt"), 
                     href = "https://data-cdphe.opendata.arcgis.com/search?collection=Dataset&q=BRFSS%202014-2017%20County%20Datasets&sort=name"),
            menuItem("RWJ County Health Rankings", icon = icon("list-alt"), 
                     href = "https://www.countyhealthrankings.org/app/colorado/2020/downloads")
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
            # mainPanel(
                tabsetPanel(
                    tabPanel("Demographic Explorer",
                             column(width=3,
                                             
                              box(title = "Select a COVID Measure and Demographic Category List from the left sidebar to load new Measures to explore",
                                 status="success",
                                 # background = "green",
                                 width = NULL, solidHeader = T,
                              "This interactive Shiny website allows the examination of the relationships among over 100 Demographic Measures against COVID Outcomes in Colorado. This is an exploratory tool designed for policy makers, epidemiologists, statisticians, and anyone interested in exploring. Almost all of the entire site is interactive and can be exported (check the top right corners of the visualizations).", br(),"Let me know what you discover at twitter.com/datatodavid"
                              ),
                             box(title = "Select a Correlation Method (optional):",
                                 status="warning",
                                 # color = "purple",
                                 width = NULL, solidHeader = T,
                                  radioButtons(inputId = "corr_type",
                                              label = NULL,
                                                  # "Select a Correlation Method:",
                                              choices = c("Pearson" = "pearson",
                                                          "Kendall" = "kendall",
                                                          "Spearman" = "spearman"),
                                              selected = "pearson"), height=150)
                             # ,
                             # box(title = "About this Shiny App",
                             #     status = "primary",
                             #     width = NULL, solidHeader = T,
                             #     "This tool allows you to examine the relationships of over 100 Demographic categories against COVID Outcomes in Colorado. Almost the entire site is interactive and can be exported, so make sure to explore and stay safe!")
                             ),
                             column(width=9,
                                    box(status="primary",ggiraphOutput("corr_explorer"),width = NULL)
                                    
                             ),
                             
                             box(title = "Selected Colorado Demographic Data by County", status="primary",
                                 solidHeader = T, "Export either all returned data or a selection using the buttons below. Click on the boxes below the Column names to filter results by column.",
                                 dataTableOutput("corr_data"), width=12)
                             ),
                    tabPanel("Statewide Data", 
                             #box(plotOutput("top20"), width=9, height = 420),
                             box(status="primary",ggiraphOutput("top20"), width=9),
                             box(title ="Selection Filters",status="warning",solidHeader = T,
                                 # "Selecting these will filter map & data output for this page only",
                                 sliderInput("covidrange", 
                                             "Total COVID Cases in County:", 
                                             0, 10000, value=c(0,10000), step=25),
                                 sliderInput("medianrange", 
                                             "Median Income Range of County:", 
                                             30000, 130000, value=c(30000,130000),
                                             step=2500),
                                 sliderInput("ruralrange", 
                                             "Rural Population % of County:", 
                                             0, 100, value=c(0,100), step=1), 
                                 "Cases, Income, and Rural factors can impact or explain other correlations.", "Use these filters to see if they affect your observations."
                                 ,width=3),
                             box(status="primary",
                                 title = "Summary Statistics", solidHeader = T,
                                 dataTableOutput("combined_data") 
                                 # ,height = 150
                                 , width=12
                                 )
                             #,box(tableOutput("demo_data"), height = 130, width=12)
                             #,box(tableOutput("COVID_data"), height = 130, width=12)
                    )
                    ,tabPanel("Maps",
                              # box(plotOutput("bi_map_legend"), height=120, width=5),
                              box(status="warning",
                                  solidHeader = T, title = "Bivariate Color Legend with number of Counties in each section",
                                  "These maps visualize correlations using a (3-tile) color grid for one or both measures. Each individual measure is split into even groups of Low, Med, or High.", 

                                  # br(), "However, when the two measures are overlapped (as happens to the right), the distribution of counties is not evenly grouped.",
                                  plotOutput("bi_analysis"),
                                  "The three left-most purple colors correspond to the selected COVID Measure", 
                                  br(), "The three bottom teal colors correspond to the selected Demographic Measure.",
                                  height = 540,
                                  width=6),
                              box(status="primary", 
                                  solidHeader=T, title = "Bivariate Interactive Colorado Map",
                                  "Hover over any County to see its data. Click to visit its Wikipedia page.",
                                  girafeOutput("bi_map"), 
                                  height = 540,
                                  width=6),
                              box(status="primary",
                                  ggiraphOutput("covid_map"), 
                                  # height = 400, 
                                  width=6),
                              box(status="primary",
                                  ggiraphOutput("demo_map"), 
                                  # height = 400, 
                                  width=6)
                              )
                    ,tabPanel("Correlation Measures",
                              box(status="primary",ggiraphOutput("scatterplot"), 
                                  # height=420, 
                                  width=6),
                              box(status="primary",ggiraphOutput("bi_cor"), 
                                  # height=420, 
                                  width=6),
                              # box(plotOutput("density"), height=320, width=6),
                              box(title ="Select a Filter Measure for the Scatterplot & Map",status="warning",solidHeader = T,
                                  radioButtons(inputId = "balancebuttons",
                                           label = NULL,
                                           choices = c("COVID Groups" = "COVID.Groups",
                                                       "Income Groups" = "Income.Groups",
                                                       "Rural Groups" = "Rural.Groups"),
                                           selected = "Rural.Groups"), width=3),
                              box(status="primary",girafeOutput("corr_map"), 
                                  # height = 500, 
                                  width=9)
                    )
                   # ,tabPanel("Stats",
                              
                    ,tabPanel("Race/Ethnicity Comparisons",
                              box(status="warning", solidHeader = T,
                                  title = "Select a Demographic Measure for Race/Ethnicity Comparisons",
                                  selectizeInput(inputId = "RaceDemo",
                                             label = NULL,
                                             choices = (colnames(
                                                 CO_Race_Measures_COVID19[4:13])),
                                             selected = "Preventable.Hospitalization.Rate"),
                                  height=100,width=12),
                              box(status="primary",plotOutput("Racefacetgrid"),
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
                             box(status="primary",title = "Race/Ethnicity Summary Statistics",
                                 dataTableOutput("Race_Stats"),
                                 width=12)
                    )
                    ,tabPanel("County COVID Data",
                              valueBoxOutput("DaysSince", width=3), 
                              valueBoxOutput("lastweekcases", width=3),
                              # valueBoxOutput("lastweekdeaths"),
                              valueBoxOutput("diff", width=3),
                              valueBoxOutput("caseranking", width=3),
                              box(status="warning", solidHeader = T, title="Selections for this page are found here:",
                                  selectizeInput(inputId = "single",
                                                 label = "Select a County:",
                                                 choices = unique(COVID19ALL_MERGE$COUNTY)[-1], 
                                                 selected = "DENVER"),
                                  br(),
                                                    # width=4),
                              # box(status="warning", 
                              selectizeInput(inputId = "COVIDselect",
                                             label = "Select a COVID Measure:",
                                             choices = colnames(COVID19ALL_MERGE)[-1:-3],
                                             selected = "New.Cases.5.Day.Avg"),
                              br(),
                              # width=4),
                              # box(status="warning", 
                              sliderInput("daterange", 
                                          "Select a Date Range:", 
                                          min = as.Date("2020-03-17"), 
                                          max = as.Date(Sys.Date()), 
                                          value = c(as.Date("2020-03-17"), as.Date(Sys.Date()))
                                          # ,step=1,
                                          ,timeFormat="%B %d"
                              ),
                              br(),
                              radioButtons(inputId = "state", label="Display State Data?",
                                                         choices = c("Yes"="COLORADO",
                                                                     "No" = ""))
                              
                                                # width=4),
                              # box(status="warning", 
                                  
                              ,width=4),
                              # box(status="primary",girafeOutput("indmap"), width=4),
                              box(status="primary",girafeOutput("indcounty"), width=8),
                              box(title = "Complete Colorado COVID Data by County",status="primary",
                                  solidHeader = T, "Export either all returned data or a selection using the buttons below. Click on the boxes below the Column names to filter results by column.",
                                  dataTableOutput("ind_data"), width=12)
                              )
                              # dateRangeInput("daterange", "Date range:",
                              #                start  = "2020-03-17",
                              #                end    = "2010-07-17",
                              #                min  = "2020-03-17",
                              #                max    = "2010-07-17",
                              #                format = "MM d",
                              #                separator = " - "))
                    
                    
                    
                    
                # )
                )))
))
