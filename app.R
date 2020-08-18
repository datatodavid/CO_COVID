library(shiny)
library(psych)
library(dplyr)
library(ggplot2)
library(magrittr)
library(lubridate)
library(mapproj)

# library(sf)
# library(rgeos)
library(maps)
library(tools)
library(biscale)
library(stringr)
library(stringi)
library(corrr)
library(cowplot)
library(extrafont)
library(extrafontdb)
library(plotrix)
library(grid)
library(gridExtra)
# library(multipanelfigure)
# library(data.table)
require(ggiraph)
require(shinydashboard)
require(DT)

# devtools::install_version("cpp11", version = "0.1", repos = "http://cran.us.r-project.org")

ui <- dashboardPage(
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
            convertMenuItem(menuItem("County Dashboard", icon=icon("info-circle"), 
                                     tabName = "County_Dash", startExpanded = TRUE,
            selectizeInput(inputId = "single",
                           label = "Select a County:",
                           choices = unique(COVID19ALL_MERGE$COUNTY)[-1], 
                           selected = "DENVER"),
            
            # width=4),
            # box(status="warning", 
            selectizeInput(inputId = "COVIDselect",
                           label = "Select a COVID Measure:",
                           choices = c("Total Tests" = "Total.Tests",
                                       "Total Cases" = "Total.Cases",                  
                                       "Total Deaths" = "Total.Deaths",
                                       "Total State Hospitalizations" = "Total.State.Hospitalizations",
                                       "Tests Rate (Per 100000)" = "Tests.Per.100000",
                                       "Cases Rate (Per 100000)" = "Cases.Per.100000",             
                                       "Deaths Rate (Per 100000)" = "Deaths.Per.100000",   
                                       "State Hospitalizations Rate (Per 100000)" = "State.Hospitalizations.Per.100000",
                                       "New Tests Last Week" = "New.Tests.Last.Week",          
                                       "New Cases Last Week" = "New.Cases.Last.Week",          
                                       "New Deaths Last Week" = "New.Deaths.Last.Week",
                                       "New State Hospitalizations Last Week" = "New.State.Hospitalizations.Last.Week",
                                       "New Tests (5 Day Average)" = "New.Tests.5.Day.Avg",          
                                       "New Cases (5 Day Average)" = "New.Cases.5.Day.Avg",          
                                       "New Deaths (5 Day Average)" = "New.Deaths.5.Day.Avg",         
                                       "New State Hospitalizations (5 Day Average)" = "New.State.Hospitalizations.5.Day.Avg",
                                       "Positive Tests Percentange (5 Day Average)" = "Positive.Tests.Perc.5.Day.Avg",
                                       "Mortality Percentage (5 Day Average)" = "Mortality.Perc.5.Day.Avg"),
                           # colnames(COVID19ALL_MERGE)[-1:-3],
                           selected = "New.Cases.5.Day.Avg"),
            
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
            
            radioButtons(inputId = "yscale", label="Choose a Scale:",
                         choices = c("Relative (Logarithmic)" = "log2", 
                                     "Fixed (Linear)"="identity")),
        
            radioButtons(inputId = "state", label="Display State Data?",
                         choices = c("Yes"="COLORADO",
                                     "No" = "")),
            
            radioButtons(inputId = "mapmeasure",
                         label = "Sort Heat Map by:",
                         # "Select a Correlation Method:",
                         choices = c("Latest Value" = "Latest",
                                     "Highest Value in Date Range" = "Maximum",
                                     "Average Value in Date Range" = "Mean"
                                     
                         )
                         # selected = "Maximum"
                         ), br()
            ), 
            tabName = "County_Dash"),
            # menuItem("twitter.com/datatodavid", icon=NULL),
            convertMenuItem(menuItem("COVID vs. Demographics", 
                                     # status="warning",
                                     # color="red",
                                     tabName = "Main_Dash", 
                     icon=icon("bar-chart-o"), #startExpanded = TRUE,
            
            #menuSubItem(#tabName = "COVID Measure:",
                radioButtons(inputId = "COVIDbuttons", 
                         label = "COVID Measure:",
                         choices = c("Tests Rate (Per 100000)" = 
                                         "COVID.Tests.Per.100000",
                                     "Cases Rate (Per 100000)" = 
                                         "COVID.Cases.Per.100000",
                                     "Deaths Rate (Per 100000)" = 
                                         "COVID.Deaths.Per.100000",
                                     "Positive Tests Percentage" = 
                                         "COVID.Positive.Tests.Perc",
                                     "Mortality Percentage" = 
                                         "COVID.Mortality.Perc"),
                         selected = "COVID.Cases.Per.100000")
            #)
            ,
            #menuSubItem(#tabName = "Demographic Category List",
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
                           selected = "CO_HEALTHCARE_ACCESS")
            #)
            ,
            #menuSubItem(#tabName = "Demographic Measure:",
                selectizeInput(inputId = "DemoData",
                           label = "Demographic Measure:",
                           choices = colnames("catbuttons")[-1:-6]))
                           # choices = "catnames"[-1:-6]))
            #)
            , tabName = "Main_Dash"),
            menuItem("Sources:",
                     # icon=icon("list-alt")
                     # icon=NULL
                     icon=icon("folder-open")
             ,
             menuSubItem("Source code", icon = icon("file-code-o"), 
                     href = "https://github.com/rstudio/shinydashboard/"),
             menuSubItem("CDPHE CO COVID Data", icon = icon("list-alt"), 
                     href = "https://data-cdphe.opendata.arcgis.com/search?collection=Dataset&q=covid19&sort=name"),
             menuSubItem("CDPHE Community Data", icon = icon("list-alt"), 
                     href = "https://data-cdphe.opendata.arcgis.com/search?collection=Dataset&q=cdphe%20community%20level%20estimates%20(county)%20"),
             menuSubItem("BRFSS County Data", icon = icon("list-alt"), 
                     href = "https://data-cdphe.opendata.arcgis.com/search?collection=Dataset&q=BRFSS%202014-2017%20County%20Datasets&sort=name"),
             menuSubItem("RWJ County Health Rankings", icon = icon("list-alt"), 
                     href = "https://www.countyhealthrankings.org/app/colorado/2020/downloads")
            )
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
        tags$head(tags$style("*:not(.fa){ font-family: 'calibri', !important;, font-weight:500; }")),
        # tags$sidebar(tags$style(" { font-family: 'Garamond', serif !important;, font-weight:500; }")),
        # tags$title(tags$style(" { font-family: 'Garamond', serif !important;, font-weight:500; }")),
        # tags$tabItems(tags$style(" { font-family: 'Garamond', serif !important;, font-weight:500; }")),
        #titlePanel("Demographic Measures vs. COVID Outcomes by County"),
        # tags$style(HTML("
        #     font-family: 'Adobe Garamond Pro', 'Garamond', serif  !important;
        # }"),
        # style = "font-family: 'Adobe Garamond Pro', 'Garamond', serif !important;, font-weight:500;",
            tabItems(
                tabItem(tabName = "County_Dash",
                        
                tabsetPanel(
                tabPanel("County COVID Dashboard",#tabName = "County_Dash",
                         fluidRow(        
                         valueBoxOutput("DaysSince", width=3), 
                         valueBoxOutput("lastweekcases", width=3),
                         # valueBoxOutput("lastweekdeaths"),
                         valueBoxOutput("diff", width=3),
                         valueBoxOutput("caseranking", width=3)
                         
                         ),
                        fluidRow(
                            # column(width=6,
                            box(status="primary",
                                solidHeader = T, title = "County vs. State COVID Trend Analysis",
                                # )
                                girafeOutput("indcounty"),
                                "Hover over Vertical lines to read about major events in Colorado's statewide response to the pandemic.",
                            ),
                        # column(width=6,
                                box(title = "Heat Map of COVID in Colorado Counties",
                                    
                                 status="primary",
                                 # color = "purple",
                                 # width = NULL, 
                             solidHeader = T,
                                 
                            # box(status="primary", 
                            girafeOutput("ind_map"),
                            "Gray will display when no data is available. Hospitalization data is only published on the state level.")
                        # )
                          
                        
                         # )
                        ),
                        fluidRow(
                         box(title = "County vs. State COVID Summary Data", status="primary",
                             solidHeader = T, 
                             dataTableOutput("ind_graph_data"), width=12))
                
                ),
                tabPanel("Colorado COVID Dataset Explorer",
                         box(title = "Complete & Cleaned Colorado COVID Dataset by County",status="primary",
                             solidHeader = T, 
                             "Click on the boxes below the Column names to filter results by column.",
                             br(),
                             "Export either all returned data or a selection using the buttons below.",
                             dataTableOutput("ind_data"), width=12))
            )
        ),
        tabItem(tabName = "Main_Dash",
        
            # mainPanel(
            tabsetPanel(
                tabPanel("Category Explorer", #tabName = "Main_Dash",
                         fluidRow(
                             column(width=3,
                                box(title = "Welcome to the COVID vs. Demographics Section", status = "success",
                                    width = NULL, solidHeader = T,
                                    "This interactive Shiny website allows the examination of the relationships among over 100 Demographic Measures against COVID Outcomes in Colorado. This is an exploratory tool designed for policy makers, epidemiologists, statisticians, and anyone interested in exploring. Almost all of the entire site is interactive and can be exported (check the top right corners of the visualizations).", br(),"Let me know what you discover at twitter.com/datatodavid"
                                    ),
                                br(),
                                box(title = "Select a COVID Measure and Demographic Category List from the left sidebar to begin",
                                    status="warning",
                                    # background = "green",
                                    width = NULL, solidHeader = T,
                                    "Since there are so many Measures to explore, they are grouped into Category Lists. To see which category may interest you, choose a COVID measure and Demographic Category List on the left sidebar and see which Measures show the strongest relationships. Then, you can click on the tabs above to explore how these factors break down by County."
                                )
                                
                                
                                # ,
                                # box(title = "Select a Correlation Method (optional):",
                                #     status="warning",
                                #     # color = "purple",
                                #     width = NULL, solidHeader = T,
                                #     radioButtons(inputId = "corr_type",
                                #                  label = NULL,
                                #                  # "Select a Correlation Method:",
                                #                  choices = c("Pearson" = "pearson",
                                #                              "Kendall" = "kendall",
                                #                              "Spearman" = "spearman"),
                                #                  selected = "pearson"), height=150)
                                # ,
                                # box(title = "About this Shiny App",
                                #     status = "primary",
                                #     width = NULL, solidHeader = T,
                                #     "This tool allows you to examine the relationships of over 100 Demographic categories against COVID Outcomes in Colorado. Almost the entire site is interactive and can be exported, so make sure to explore and stay safe!")
                         ),
                         
                         column(width=6, 
                                box(status="info",solidHeader=T, title="Use this tool to explore Demographic Categories & Measures",
                                    ggiraphOutput("corr_explorer"),width = NULL)
                                
                         ),
                         column(width=3,
                                box(status="primary",width=NULL, solidHeader = T, 
                                    title = "A Positive Correlation Relationship means:", 
                                    "The higher the Demographic Measure in a county,",br(), "the higher the selected COVID Measure.",
                                    br(), br(), "A value greater than 0.2 suggests that this relationship is statistically significant based on the 64 Colorado counties."),
                                box(status="danger", width=NULL,solidHeader = T, 
                                    title = "A Negative Correlation Relationship means:", 
                                    "The lower the Demographic Measure in a county,",br(), "the higher the selected COVID Measure.",
                                    br(), br(), "A value less than -0.2 suggests that this relationship is statistically significant based on the 64 Colorado counties.")
                         ),
                         
                         box(title = "Selected Colorado Demographic Data by County", status="primary",
                             solidHeader = T, "Export either all returned data or a selection using the buttons below. Click on the boxes below the Column names to filter results by column.",
                             dataTableOutput("corr_data"), width=12)
               ) ),
               tabPanel("Filter Group Explorer", #tabName = "Main_Dash",
                         fluidRow(
                             box(status="primary",
                                 "This page is meant to explore Colorado Counties by important groupings.", br(),
                                 "Select a COVID Measure and Demographic Measure on the left sidebar to update the Table and Scatterplot.", br(),
                                 "Then, choose a Group in the orange box to the right.", br(),
                                 "Hover over any bars, points, or states on this page to see more information.",
                                 br(), br(),
                                 ggiraphOutput("bi_cor"), 
                                 # height=420, 
                                 solidHeader = T, title="Breakdown of Colorado Counties by Group",
                                 width=4, height=620),
                             box(title ="Select a Filter Measure for the Scatterplot & Map",status="warning",solidHeader = T,
                                 selectizeInput(inputId = "balancebuttons",
                                              label = NULL,
                                              choices = c("COVID Groups" = "COVID.Groups",
                                                          "Income Groups" = "Income.Groups",
                                                          "Rural Groups" = "Rural.Groups"),
                                              selected = "Rural.Groups"), width=8, height = 100),
                             box(status="primary",ggiraphOutput("scatterplot"), 
                                 # height=420, 
                                 solidHeader = T, title = "County Scatterplot with Linear Regression", 
                                 width=4, height=500),
                             box(status="primary",
                                 "Hover over any County to find out detailed group information.", br(),
                                 "Legend is the same as the Scatterplot to the left.", 
                                 girafeOutput("corr_map"), 
                                 # height = 500, 
                                 solidHeader = T, title = "Colorado County Map by Group",
                                 width=4, height = 500),
                             box(width=12, status="success", title="How were these groups calculated?", solidHeader = T,
                                 "The 64 Colorado Counties were split into groups using the following methods:", br(),
                                 ">>> COVID Caseload -- 3 (Almost) Even Groups: 22 Low, 21 Med, 21 High", br(),
                                 ">>> Median Income -- 3 (Almost) Even Groups: 22 Low, 21 Med, 21 High", br(),
                                 ">>> Rural Percentage -- 4 Groups by Census Rural Breakdowns: 15 Urban/Suburban (0-24%), 15 Somewhat Rural (25-49%), 10 Mostly Rural (50-99%), 24 100% Rural"
                                 # ">>> Rural Percentage -- [15 Sub/Urban (0-24%), 15 Somewhat (25-49%), 10 Mostly (50-99%), 24 (100%)]"
                                 )
                             
                             # box(status="primary",ggiraphOutput("bi_cor"), 
                             #     # height=420, 
                             #     width=4)
                             # box(plotOutput("density"), height=320, width=6),
                             # box(title ="Select a Filter Measure for the Scatterplot & Map",status="warning",solidHeader = T,
                             #     radioButtons(inputId = "balancebuttons",
                             #                  label = NULL,
                             #                  choices = c("COVID Groups" = "COVID.Groups",
                             #                              "Income Groups" = "Income.Groups",
                             #                              "Rural Groups" = "Rural.Groups"),
                             #                  selected = "Rural.Groups"), width=3),
                             
                         ) ),
                tabPanel("Statewide Data", #tabName = "Main_Dash",
                         #box(plotOutput("top20"), width=9, height = 420),
                         fluidRow(
                         
                        column(width=5,
                               box(status="success", width = NULL,title="How to Use this Interactive Bar Chart", solidHeader = T,
                            "The 64 Colorado Counties are ordered from highest to lowest based on the chosen Demographic Measure on the left sidebar.", br(), 
                            "Then, the bars are shaded by the chosen COVID Measure from the left sidebar, with blue being lowest and red being highest.", br(),
                            "Finally, use the filters below to change which types of Counties you would like to compare. The Chart and Summary Statistics Table will both update automatically.", br(),
                            "Remember, you can hover over any bar to get more detailed information."),
                        box(title ="Selection Filters",width = NULL,status="warning",solidHeader = T,
                             # "Selecting these will filter map & data output for this page only",
                             sliderInput("covidrange", 
                                         "Total COVID Cases in County:", 
                                         0, 11000, value=c(20,11000), step=20),
                             sliderInput("medianrange", 
                                         "Median Income Range of County:", 
                                         31000, 121000, value=c(31000,121000),
                                         step=2000),
                             sliderInput("ruralrange", 
                                         "Rural Population % of County:", 
                                         0, 100, value=c(0,100), step=1) 
                             # "Cases, Income, and Rural factors can impact or explain other correlations.", "Use these filters to see if they affect your observations."
                             )),
                        column(width=7,box(status="primary",width = NULL,solidHeader = T, 
                                           title="County by County Demographic Rankings Bar Chart",
                            ggiraphOutput("top20")))),
                         box(status="primary",
                             title = "Summary Statistics", solidHeader = T,
                             dataTableOutput("combined_data") 
                             # ,height = 150
                             , width=12
                         )
                         #,box(tableOutput("demo_data"), height = 130, width=12)
                         #,box(tableOutput("COVID_data"), height = 130, width=12)
                ) 
                ,tabPanel("Colorado Maps", #tabName = "Main_Dash",
                          # 
                          fluidRow(
                          # 
                          # box(status="primary",
                          #     solidHeader=T, title = "Bivariate Interactive Colorado Map",
                          #     "Hover over any County to see its data. Click to visit its Wikipedia page.",
                          #     girafeOutput("bi_map"),
                          #     height = 540,
                          #     width=6),
                          box(status="primary",
                              "Hover over any County to see its data. Click to visit its Wikipedia page.",
                              ggiraphOutput("covid_map"), 
                              # height = 400, 
                              solidHeader = T, title="COVID Measure Severity Map",
                              width=6),
                          box(status="primary",
                              "Hover over any County to see its data. Click to visit its Wikipedia page.",
                              ggiraphOutput("demo_map"), 
                              # height = 400, 
                              solidHeader = T, title="Demographic Measure Severity Map",
                              width=6)
                          ,
                          # fluidRow(
                              box(status="success",
                              solidHeader=T, title = "Overlap of COVID Measure & Demographic Measure Severity Maps",
                              "Color grid created by overlapping two graphs above - see legend on right.",br(),
                              "Hover over any County to see its data. Click to visit its Wikipedia page.", 
                              
                              girafeOutput("bi_map"),
                              # height = 540,
                              width=8),
                          box(status="warning",
                              solidHeader = T, title = "Overlap Map Color Legend with number of Counties in each group",
                              "This legend shows the distribution of Colorado's 64 counties into each of the 9 Low/Med/High combinations.",
                                   # br(), "However, when the two measures are overlapped (as happens to the right), the distribution of counties is not evenly grouped.",
                                   br(), "The more Purple the color is, the higher the COVID measure.", br(),# (vertical) colors show the selected COVID Measure",
                                   "The more Teal the color is, the higher the Demographic measure.",# (horizontal) colors correspond to the selected Demographic Measure.",
                              #      
                              plotOutput("bi_map_vlegend"),
                              # height = 540,
                              width=4)
                          
                          #     ,
                          # box(status="warning",
                          #      solidHeader = T, title = "Overlap Map Color Legend with number of Counties in each section",
                          #      "This grid shows the distribution of Colorado's 64 counties into each of the 9 Low/Med/High combinations.",
                          #      # br(), "However, when the two measures are overlapped (as happens to the right), the distribution of counties is not evenly grouped.",
                          #      br(), "The more Purple the color is, the higher the COVID measure.", br(),# (vertical) colors show the selected COVID Measure",
                          #      "The more Teal the color is, the higher the Demographic measure.",# (horizontal) colors correspond to the selected Demographic Measure.",
                          #      
                          #      plotOutput("bi_analysis"),
                          #      
                          #      width=5))
                          # width=7)
                          # ,box(status="warning",
                          #          solidHeader = T, title = "Overlap Map Color Legend with number of Counties in each section",
                          #          "This grid shows the distribution of Colorado's 64 counties into each of the 9 Low/Med/High combinations.",
                          #          # br(), "However, when the two measures are overlapped (as happens to the right), the distribution of counties is not evenly grouped.",
                          #      br(), "The more Purple the color is, the higher the COVID measure.", br(),# (vertical) colors show the selected COVID Measure",
                          #      "The more Teal the color is, the higher the Demographic measure.",# (horizontal) colors correspond to the selected Demographic Measure.",
                          #      
                          #      plotOutput("bi_analysis"),
                          #          
                          #          width=5)
                          # # ,
                ) )
                
                # ,tabPanel("Stats",
                
                ,tabPanel("Race/Ethnicity Comparisons", #tabName = "Main_Dash",
                          fluidRow(
                          box(status="warning", solidHeader = T,
                              title = "Select a Demographic Measure Here for Race/Ethnicity Comparisons --- (COVID Measure is still selected on the left sidebar)",
                              selectizeInput(inputId = "RaceDemo",
                                             label = NULL,
                                             choices = 
                                                 c("Years of Potential Life Lost Rate" =
                                                       "Years.of.Potential.Life.Lost.Rate",
                                                   "Teen Birth Rate" = "Teen.Birth.Rate",
                                                   "Preventable Hospitalization Rate" = 
                                                       "Preventable.Hospitalization.Rate",
                                                   "Percentage Flu Vaccinated" = "Perc.Flu.Vaccinated",
                                                   "Percentage Children in Poverty" = "Perc.Children.in.Poverty",
                                                   "Injury Death Rate" = "Injury.Death.Rate",
                                                   "Percentage Drive Alone" = "Perc.Drive.Alone",
                                                   "Median Household Income" = "Median.Household.Income",
                                                   "Demographics Percentage" = "Demographics.Perc",
                                                   "Segregation Index" = "Segregation.Index"),
                                                   
                                                 # colnames(CO_Race_Measures_COVID19[4:13])),
                                             selected = "Preventable.Hospitalization.Rate"),
                              height=100,width=9),
                          box(status = "warning", solidHeader=T, 
                              title="Choose a Scale:", 
                              selectizeInput(inputId="scales", label=NULL, 
                                             choices = c("Relative Scaling (easier to see Counties)" = "free_x", 
                                                         "Fixed Scaling (easier to see Distribution)" = "fixed")), 
                              height=100, width=3),
                          box(status="primary",plotOutput("Racefacetgrid"), 
                              style="font-family: 'times', 'Garamond', serif !important;, font-weight:500;",
                              solidHeader = T, title = "Use this tool to compare and contrast trends for the 10 Demographic Factors with Race/Ethnicity Data",
                              # height = 480, 
                              width=12),
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
                              solidHeader = T,
                              dataTableOutput("Race_Stats"),
                              width=12)
                ) )
                
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

server <- function(input, output, session){
    # DemoData <- reactive({
    #   Demochoices == colnames(input$catbuttons)
    # })
    observe({
        Demochoices = get(input$catbuttons)
        updateSelectizeInput(session, "DemoData", 
                             choices = no_periods(colnames(Demochoices)[-1:-6]),
                             selected = no_periods(colnames(Demochoices)[7]))
    }) 
    catnames = reactive({no_periods(colnames(input$catbuttons))})
    output$value <- renderText({ input$state })
    covidrange = reactive({seq(min(x), max(x), 
                               length.out = input$COVID.Cases.Max)})
    medianrange = reactive({seq(min(x), max(x), 
                                length.out = input$Median.Household.Income)})
    ruralrange = reactive({seq(min(x), max(x), 
                               length.out = input$Perc.Rural)})
    
    xlab_clean = reactive({no_periods(input$DemoData)})
    xlab_perc = reactive({no_perc(no_periods(input$DemoData))})
    #xlab_perc_short = reactive({word(no_perc(no_periods(input$DemoData)), 1, -1, sep=" ")})
    xlab_short = reactive({word(no_periods(input$DemoData), 1, 2, sep=" ")})
    xlab_no_perc = reactive({word(no_periods(input$DemoData), 2, -1, sep=" ")})
    xlab_long_perc = reactive({long_perc(no_periods(input$DemoData))})
    ylab_clean = reactive({per_parentheses(no_periods(input$COVIDbuttons))})
    ylab_perc = reactive({per_parentheses(no_perc(no_periods(input$COVIDbuttons)))})
    ylab_short = reactive({word(no_periods(input$COVIDbuttons), 1,2, sep=" ")})
    ylab_rest = reactive({word(no_periods(input$COVIDbuttons), 3,-1, sep=" ")})
    ylab_rev = reactive({per_parentheses(rev_perc(no_periods(input$COVIDbuttons)))})
    ylab_frontspace = reactive({per_parentheses(rev_perc(frontspace(no_periods(input$COVIDbuttons))))})
    ylab_long_perc = reactive({per_parentheses(long_perc(no_periods(input$COVIDbuttons)))})
    countyname = reactive({input$single})
    bal_lab_clean = reactive({no_periods(input$balancebuttons)})
    catbuttons_clean = reactive({upper_Co(stri_trans_totitle(no_underscore(input$catbuttons)))})
    catbuttons_long = reactive({no_Co(stri_trans_totitle(no_underscore(input$catbuttons)))})
    race_xlab_clean = reactive({no_periods(input$RaceDemo)})
    race_xlab_perc = reactive({no_perc(no_periods(input$RaceDemo))})
    race_xlab_long_perc = reactive({long_perc(no_periods(input$RaceDemo))})
    covid_lab_perc = reactive({no_perc(per_parentheses(avg_parentheses(no_periods(input$COVIDselect))))})
    covid_lab_perc_no_avg = reactive({long_avg(per_parentheses(avg_parentheses(no_perc(no_periods(input$COVIDselect)))))})
    covid_lab_long = reactive({long_avg(per_parentheses(avg_parentheses(long_perc(no_periods(input$COVIDselect)))))})
    covid_lab_long_perc = reactive({per_parentheses(long_perc(no_periods(input$COVIDselect)))})
    #     observe({CO_COUNTY_BI_CLASS = CO_COUNTY_COVID_FILTER %>%
    #         mutate(., 
    #                demo_var = ntile(get(input$DemoData), 3),
    #                covid_var = ntile(get(input$COVIDbuttons), 3),
    #                bi_class = paste0(
    #                    as.numeric(demo_var), "-",
    #                    as.numeric(covid_var)
    #                )) %>% 
    #         select(., COUNTY, demo_var, covid_var, bi_class)
    #     CO_MAP_COVID1 = left_join(CO_MAP_COVID, CO_COUNTY_BI_CLASS, by="COUNTY")
    #     CO_MAP_COVID1$bi_class %<>% 
    #         gsub("NA-1", NA, .) %>% 
    #         gsub("NA-2", NA, .) %>% 
    #         gsub("NA-3", NA, .) 
    #     # CO_MAP_COVID1$demo_var %<>%
    #     #     gsub(1, paste0("Low ",xlab_perc()), .) %>% 
    #     #     gsub(2, paste0("Med ",xlab_perc()), .) %>% 
    #     #     gsub(3, paste0("High ",xlab_perc()), .)
    #     # CO_MAP_COVID1$covid_var %<>%
    #     #     gsub(1, paste0("Low ",ylab_perc()), .) %>% 
    #     #     gsub(2, paste0("Med ",ylab_perc()), .) %>% 
    #     #     gsub(3, paste0("High ",ylab_perc()), .)
    #     CO_MAP_COVID1$demo_var %<>%
    #         gsub(1, "Low", .) %>% 
    #         gsub(2, "Med", .) %>% 
    #         gsub(3, "High", .)
    #     CO_MAP_COVID1$covid_var %<>%
    #         gsub(1, "Low", .) %>% 
    #         gsub(2, "Med", .) %>% 
    #         gsub(3, "High", .)
    # })
    # ylab_2line = reactive({paste(ylab_short, ylab_rest, sep="\n")})
    
    
    output$corr_explorer <- renderGirafe({
        # reactive({levels(colnames(input$catbuttons)) = no_periods(colnames(input$catbuttons))})
        explorer = correlate(get(input$catbuttons)[-1], method="pearson") %>% 
            focus(COVID.Tests.Per.100000, COVID.Cases.Per.100000, 
                  COVID.Deaths.Per.100000, COVID.Positive.Tests.Perc, 
                  COVID.Mortality.Perc) %>% 
            mutate(rowname = reorder(no_periods(rowname), get(input$COVIDbuttons))) %>%
            ggplot(aes(rowname, get(input$COVIDbuttons), fill=get(input$COVIDbuttons))) +
            scale_fill_gradient2(low="red", mid="grey80", high="blue") +
            labs(title=paste("Relationship Strength Analysis:", 
                             paste0(ylab_long_perc(), " vs."),
                             paste0(catbuttons_long()," Measures"), 
                             sep="\n"),
                 fill = str_wrap(ylab_perc(), 7)
                 # ,subtitle = "Scale between -1 and 1"
            ) +
            theme(
                text=element_text(family="calibri"),
                axis.text = element_text(face="bold"),
                legend.title = element_text(color="darkblue", face="bold"),
                legend.text = element_text(color="darkblue"),
                plot.title = element_text(face="bold", hjust=0.5),
                # plot.subtitle = element_text(hjust=0.5, color="darkblue", size=14),
                axis.title = element_text(face="bold", size=12),
                strip.text.x = element_text(face="bold"),
                legend.background =
                    element_rect(fill="#F2F5F7", color="gray", size=1)) +
            xlab(paste0(catbuttons_clean(), " Measures")) +
            ylab(paste0("Correlation range [-1,1] by ", ylab_short())) + 
            # scale_x_discrete(labels = catbuttons_long()) +
            coord_flip() + 
            geom_col_interactive(aes(tooltip=
                                         paste("In Colorado counties,\n", long_perc(no_periods(rowname)), 
                                               "\n& ", ylab_long_perc(),
                                               "\nhave a", round(get(input$COVIDbuttons),2),
                                               "Pearson Correlation", sep=" "))) +
            geom_hline_interactive(yintercept=-0.2, aes(tooltip="Significant Negative Correlation"),
                                   color="darkred", linetype="dashed", alpha=0.5, size=1) +
            geom_hline_interactive(yintercept=0.2, aes(tooltip="Significant Positive Correlation"),
                                   color="darkblue", linetype="dashed", alpha=0.5, size=1) 
        
        # ggiraph(code=print(explorer))
        girafe(ggobj = explorer, options = list(
            # opts_sizing(rescale = TRUE, width = .7),
            opts_hover(css = "opacity:0.8;"),
            opts_tooltip(offx = 20, offy = 35)
        ))
    })
    output$corr_data = renderDataTable({
        corrdata = get(input$catbuttons) %>% 
            select(-starts_with("COVID"), everything(), starts_with("COVID")) %>% 
            arrange(COUNTY)
        corrdata[-1:-7]=round(corrdata[-1:-7], 2)
        # corrdata = formatRound(corrdata, columns=c(-1:-6), digits=2)
        datatable(corrdata, rownames=F,
                  filter = list(position = 'top', clear = FALSE),
                  extensions = 
                      c("Buttons", "Select"),
                  options = list(
                      search = list(regex = TRUE, caseInsensitive = TRUE),
                      pageLength = 5,
                      fixedColumns = list(leftColumns = 1),
                      scrollX=TRUE, 
                      # scrollY=TRUE, 
                      scrollCollapse=TRUE,
                      select=TRUE,
                      dom = 'Bfrtip',
                      buttons = c('copy', 'csv', 'excel'
                                  # , 'pdf', 'print'
                      )
                      # list(
                      # list(
                      #     extend = "copy",
                      #     text = 'Copy',
                      #     exportOptions = list(modifier = list(selected = TRUE))
                      # ), 
                      # list(
                      #     extend = "csv",
                      #     text = 'CSV',
                      #     exportOptions = list(modifier = list(selected = TRUE))
                      # ), 
                      # list(
                      #     extend = "excel",
                      #     text = 'Excel',
                      #     exportOptions = list(modifier = list(selected = TRUE))
                      # ), 
                      # list(
                      #     extend = "pdf",
                      #     text = 'PDF',
                      #     exportOptions = list(modifier = list(selected = TRUE))
                      # ), 
                      # list(
                      #     extend = "print",
                      #     text = 'Print',
                      #     exportOptions = list(modifier = list(selected = TRUE))
                  )
        ) 
        # %>%
        #     formatRound(columns = -c(1:6), digits=2)
        
    })
    output$top20 <- renderGirafe({
        #### ALL_DATA Top 20 BAR CHART BY COUNTY ####
        
        topcounties = 
            CO_COUNTY_COVID_FILTER %>% 
            filter(., !is.na(get(yes_periods(input$DemoData))) &
                       !is.na(get(input$COVIDbuttons)) &
                       !is.na(COUNTY) 
                   & COVID.Cases.Max >= min(input$covidrange)
                   & COVID.Cases.Max <= max(input$covidrange)
                   & Median.Household.Income >= min(input$medianrange)
                   & Median.Household.Income <= max(input$medianrange)
                   & Perc.Rural >= min(input$ruralrange)
                   & Perc.Rural <= max(input$ruralrange)
            ) %>%
            #     arrange(desc(get(input$DemoData))) %>%
            #     slice_head(., n=20)
            # CO_COUNTY_COVID_FILTER %>% 
            #     #filter(., COUNTY %in% top20counties$COUNTY) %>% 
            ggplot(., aes(y=
                              #COUNTY,
                              reorder(COUNTY, get(yes_periods(input$DemoData))), 
                          x=get(yes_periods(input$DemoData)),
                          fill=get(input$COVIDbuttons))) +
            geom_col() + 
            ggtitle(str_wrap(xlab_long_perc(),35)) + 
            scale_fill_gradient(low="#3399FF", high="red2") +
            
            labs(y="Colorado Counties", x=xlab_perc()
                 , fill = str_wrap(ylab_perc(), 6.5)
                 , subtitle = paste0("Color Shading by ", ylab_short())
            ) +
            # geom_text(aes(label=round(get(input$DemoData),1)), 
            #          position = position_jitter(width=0, height=1.5), vjust=5.5, color="white"
            #           ) +
            theme(text=element_text(family="calibri"),
                  panel.grid.major.y = element_blank(),
                  axis.text.y =
                      element_text(#angle=90, hjust=1, vjust=0.3, 
                          face="bold", size=7),
                  axis.text.x = element_text(face="bold"),
                  # axis.ticks = margin(r=0.5),
                  # axis.text.y = position_nudge(x=0.2), 
                  # plot.margin = unit(c(0,0,0,6),"mm"),
                  # axis.ticks.length.y = 0,
                  # axis.ticks.y = element_text(hjust=-0.3),
                  legend.title = element_text(color="darkblue", face="bold"),
                  legend.text = element_text(color="darkblue"),
                  plot.title = element_text(hjust=0.5, face="bold", size=18),
                  plot.subtitle = element_text(hjust=0.5, face="bold", color="darkblue", size=14),
                  axis.title = element_text(face="bold", size=14),
                  strip.text.y = element_text(face="bold"),
                  legend.background =
                      element_rect(fill="#F2F5F7", color="gray", size=1)) +
            
            geom_col_interactive(stat="identity", 
                                 aes(tooltip = paste(paste0(COUNTY, " COUNTY"),
                                                     (paste(round(get(yes_periods(input$DemoData)),1), xlab_perc(), sep=" ")),
                                                     (paste(round(get(input$COVIDbuttons),1), ylab_perc(), sep=" ")),
                                                     sep="\n"),
                                     data_id = COUNTY))
        
        # ggiraph(code=print(topcounties))
        girafe(ggobj = topcounties, options = list(
            # opts_sizing(rescale = TRUE, width = .7),
            opts_hover(css = "opacity:0.8;"),
            # height_svg = 3
            # opts_tooltip(offx = 20, offy = -55)
            opts_selection(type = "none")
        ))
    })
    output$scatterplot = renderGirafe({
        
        # # CO_COUNTY_BI_CLASS = CO_COUNTY_COVID_FILTER %>%
        # #     mutate(., 
        # #            demo_var = ntile(get(input$DemoData), 3),
        # #            covid_var = ntile(get(input$COVIDbuttons), 3),
        # #            bi_class = paste0(
        # #                as.numeric(demo_var), "-",
        # #                as.numeric(covid_var)
        # #            )) %>% 
        # #     select(., COUNTY, demo_var, covid_var, bi_class)
        # # CO_COUNTY_BI_CLASS$bi_class %<>% 
        # #     gsub("NA-1", NA, .) %>% 
        # #     gsub("NA-2", NA, .) %>% 
        # #     gsub("NA-3", NA, .)
        # CO_MAP_COVID2 = left_join(CO_MAP_COVID, left_join(CO_COUNTY_BI_CLASS, 
        #                                                   CO_COUNTY_BAL_SLIDERS,
        #                                                   by="COUNTY"),
        #                           by="COUNTY")
        point2disp = CO_MAP_COVID_BAL %>%
            filter(., #Demographic != "ALL" & 
                   !is.na(get(yes_periods(input$DemoData))) &
                       !is.na(COUNTY) 
            ) %>% 
            ggplot(aes(x=get(yes_periods(input$DemoData)), y=get(input$COVIDbuttons),
                       #,color = bi_class
                       #,color=as.character(var1), shape=as.character(var2)
            )
            # ,size=Population
            ) + 
            geom_point_interactive(stat="identity", 
                                   size=2.5,
                                   # key_glyph="rect",
                                   # size=Population,
                                   aes(
                                       # color=bi_class, 
                                       
                                       color = get(input$balancebuttons),
                                       tooltip = paste(paste0(COUNTY, " COUNTY"),
                                                       get(input$balancebuttons),
                                                       paste0("Population: ", formatC(Population, format="f", big.mark = ",", digits=0)),
                                                       paste0("Median Income: $", formatC(Median.Household.Income, format="f", big.mark = ",", digits=0)),
                                                       paste0(Perc.Rural, " % Rural"),
                                                       paste(get(yes_periods(input$DemoData)),xlab_perc(), sep=" "),
                                                       paste(formatC(get(input$COVIDbuttons),format="f", big.mark = ",", digits=1),ylab_perc(), sep=" "),
                                                       sep="\n")
                                   )) +
            scale_color_brewer_interactive(palette = "Set1") +
            geom_smooth(method="glm") +
            xlab(xlab_perc()) + ylab(ylab_perc()) +
            labs(title = "COVID Trend Analysis by County",
                 subtitle = paste0(ylab_short(), " vs. ", str_wrap(xlab_long_perc(),21)),
                 color=bal_lab_clean()
                 , caption = "Linear Regression Model"
                 ) +
            guides(colour = guide_legend(override.aes = list(size = 10))) +
            theme(text=element_text(family="calibri"),
                  axis.text.x = element_text(face="bold"),
                  axis.text.y = element_text(face="bold"),
                  # legend.key = element_rect(),
                  legend.title = element_text(color="darkblue", face="bold", size=15, hjust=0.5),
                  legend.text = element_text(color="darkblue", face="bold", size=11, hjust=0),
                  plot.title = element_text(face="bold", size=18),
                  plot.subtitle = element_text(hjust=0.5, color="darkblue", size=14),
                  plot.caption = element_text(face="italic"),
                  axis.title = element_text(face="bold", size=12),
                  strip.text.x = element_text(face="bold"),
                  legend.background =
                      element_rect(fill="#F2F5F7", color="gray", size=1)) 
        # +
        # scale_color_manual_interactive(name = paste("Correlation by County of",
        #                                             str_wrap(paste0(xlab_long_perc(), " (Demo)"),21),
        #                                             paste0("& ", ylab_short()),
        #                                             sep="\n"),
        
        # )
        # ggiraph(code=print(point2disp))
        girafe(ggobj = point2disp, options = list(
            # opts_sizing(rescale = TRUE, width = .7),
            opts_hover(css = "opacity:0.8;"),
            # opts_tooltip(offx = 20, offy = -55)
            opts_selection(type = "none")
        ))
    })
    # output$density = renderPlot({
    #     
    #     CO_MAP_COVID_BAL %>%
    #         filter(., #Demographic != "ALL" & 
    #                !is.na(get(input$DemoData)) &
    #                    !is.na(COUNTY) 
    #         ) %>% 
    #         ggplot(aes(x=get(input$DemoData), y=get(input$COVIDbuttons)
    #                    #,color = bi_class
    #                    #,color=as.character(var1), shape=as.character(var2)
    #         )) + 
    #         geom_density_2d(
    #             # stat="identity", size=2.5,
    #                                aes(
    #                                    # color=bi_class, 
    #                                    color = get(input$balancebuttons),
    #                                    # tooltip = paste(paste0(COUNTY, " COUNTY"),
    #                                    #                 (paste(round(get(input$DemoData),1), xlab_perc(), sep=" ")),
    #                                    #                 (paste(round(get(input$COVIDbuttons),1), ylab_perc(), sep=" ")), sep=" \n")
    #                                )) +
    #         xlab(xlab_perc()) + ylab(ylab_perc()) +
    #         labs(title = "Density Correlations by County",
    #              subtitle = paste0(ylab_short(), " vs. ", str_wrap(xlab_clean(),21))) +
    #         theme(axis.text.x = element_text(face="bold"),
    #               axis.text.y = element_text(face="bold"),
    #               legend.title = element_text(color="darkblue", face="bold"),
    #               legend.text = element_text(color="darkblue"),
    #               plot.title = element_text(face="bold", size=18),
    #               plot.subtitle = element_text(hjust=0.5, color="darkblue", size=14),
    #               axis.title = element_text(face="bold", size=12),
    #               strip.text.x = element_text(face="bold"),
    #               legend.background =
    #                   element_rect(fill="#F2F5F7", color="gray", size=1)) 
    
    # ggiraph(code=print(density2disp))
    # })
    # output$demo_data = renderTable({
    #     #### ALL_DATA STATS ####
    #    demo_datasheet = CO_COUNTY_COVID_FILTER %>% 
    #         filter(., #Demographic != "ALL" & 
    #                !is.na(get(input$DemoData)) &
    #                !is.na(COUNTY) 
    #                & COVID.Cases.Max >= min(input$covidrange)
    #                & COVID.Cases.Max <= max(input$covidrange)
    #                & Median.Household.Income >= min(input$medianrange)
    #                & Median.Household.Income <= max(input$medianrange)
    #                & Perc.Rural >= min(input$ruralrange)
    #                & Perc.Rural <= max(input$ruralrange)
    #         ) %>% 
    #         select(., COUNTY, COVID.Tests.Per.100000, 
    #                COVID.Cases.Per.100000,  COVID.Deaths.Per.100000, 
    #                COVID.Mortality.Perc, input$DemoData
    #         ) %>% 
    #         summarise(., n = n(),
    #                   Min = min(get(input$DemoData)),
    #                   Q1 = quantile(get(input$DemoData), 0.25),
    #                   Median = median(get(input$DemoData)),
    #                   Mean = mean(get(input$DemoData)),
    #                   Q3 = quantile(get(input$DemoData), 0.75),
    #                   Max = max(get(input$DemoData)),
    #                   Range = max(get(input$DemoData)) -
    #                       min(get(input$DemoData)),
    #                   S.Dev = sd(get(input$DemoData)),
    #                   S.Error = std.error(get(input$DemoData)),
    #                   Skew = round(skew(get(input$DemoData)),2))
    #                   #paste0("Mean ", ylab_short()) 
    # 
    #                   # ,Margin.of.Error = 1.96*S.E
    #                       # , round((
    #                       # qnorm(.95, Mean, Std.Dev)*Mean/sqrt(Counties.w.Data)),2)
    #     demo_datasheet
    #         
    # }, caption=paste("<b> Statistics for Selected Demographic Measure </b>"),
    # caption.placement = getOption("xtable.caption.placement", "top"),
    # caption.width = getOption("xtable.caption.width", NULL)
    # )
    # 
    #     output$COVID_data = renderTable({
    #         #### ALL_DATA STATS ####
    #        covid_datasheet = CO_COUNTY_COVID_FILTER %>% 
    #             filter(., #Demographic != "ALL" & 
    #                    !is.na(get(input$COVIDbuttons)) &
    #                    !is.na(COUNTY)
    #                    & COVID.Cases.Max >= min(input$covidrange)
    #                    & COVID.Cases.Max <= max(input$covidrange)
    #                    & Median.Household.Income >= min(input$medianrange)
    #                    & Median.Household.Income <= max(input$medianrange)
    #                    & Perc.Rural >= min(input$ruralrange)
    #                    & Perc.Rural <= max(input$ruralrange)
    #             ) %>% 
    #             select(., COUNTY, COVID.Tests.Per.100000, 
    #                    COVID.Cases.Per.100000,  COVID.Deaths.Per.100000, 
    #                    COVID.Mortality.Perc, input$DemoData
    #             ) %>% 
    #             summarise(., n = n(),
    #                       Min = min(get(input$COVIDbuttons)),
    #                       Q1 = quantile(get(input$COVIDbuttons), 0.25),
    #                       Median = median(get(input$COVIDbuttons)),
    #                       Mean = mean(get(input$COVIDbuttons)),
    #                       Q3 = quantile(get(input$COVIDbuttons), 0.75),
    #                       Max = max(get(input$COVIDbuttons)),
    #                       Range = max(get(input$COVIDbuttons)) -
    #                           min(get(input$COVIDbuttons)),
    #                       S.Dev = sd(get(input$COVIDbuttons)),
    #                       S.Error = std.error(get(input$COVIDbuttons)),
    #                       Skew = round(skew(get(input$COVIDbuttons)),2))
    #                       #paste0("Mean ", ylab_short()) 
    #                       
    #                       # ,Margin.of.Error = 1.96*S.E
    #                       # , round((
    #                       # qnorm(.95, Mean, Std.Dev)*Mean/sqrt(Counties.w.Data)),2)
    #             covid_datasheet
    #                       
    # 
    # }, caption=paste("<b> Statistics for Selected COVID Measure </b>"),
    # caption.placement = getOption("xtable.caption.placement", "top"),
    # caption.width = getOption("xtable.caption.width", NULL))
    
    output$combined_data = renderDataTable({
        demo_datasheet = CO_COUNTY_COVID_FILTER %>% 
            filter(., #Demographic != "ALL" & 
                   !is.na(get(yes_periods(input$DemoData))) &
                       !is.na(COUNTY) 
                   & COVID.Cases.Max >= min(input$covidrange)
                   & COVID.Cases.Max <= max(input$covidrange)
                   & Median.Household.Income >= min(input$medianrange)
                   & Median.Household.Income <= max(input$medianrange)
                   & Perc.Rural >= min(input$ruralrange)
                   & Perc.Rural <= max(input$ruralrange)
            ) %>% 
            select(., COUNTY, COVID.Tests.Per.100000, 
                   COVID.Cases.Per.100000,  COVID.Deaths.Per.100000, 
                   COVID.Positive.Tests.Perc, COVID.Mortality.Perc, yes_periods(input$DemoData)
            ) %>% 
            summarise(., Measure = xlab_long_perc(),
                      Counties = n(),
                      Min = round(min(get(yes_periods(input$DemoData))),2),
                      Q1 = round(quantile(get(yes_periods(input$DemoData)), 0.25),2),
                      Median = round(median(get(yes_periods(input$DemoData))),2),
                      Mean = round(mean(get(yes_periods(input$DemoData))),2),
                      Q3 = round(quantile(get(yes_periods(input$DemoData)), 0.75),2),
                      Max = round(max(get(yes_periods(input$DemoData))),2),
                      Range = round(max(get(yes_periods(input$DemoData))) -
                                        min(get(yes_periods(input$DemoData))),2),
                      S.Dev = round(sd(get(yes_periods(input$DemoData))),2),
                      S.Error = round(std.error(get(yes_periods(input$DemoData))),2),
                      Skew = round(skew(get(yes_periods(input$DemoData))),2))
        
        covid_datasheet = CO_COUNTY_COVID_FILTER %>% 
            filter(., #Demographic != "ALL" & 
                   !is.na(get(input$COVIDbuttons)) &
                       !is.na(COUNTY)
                   & COVID.Cases.Max >= min(input$covidrange)
                   & COVID.Cases.Max <= max(input$covidrange)
                   & Median.Household.Income >= min(input$medianrange)
                   & Median.Household.Income <= max(input$medianrange)
                   & Perc.Rural >= min(input$ruralrange)
                   & Perc.Rural <= max(input$ruralrange)
            ) %>% 
            select(., COUNTY, COVID.Tests.Per.100000, 
                   COVID.Cases.Per.100000,  COVID.Deaths.Per.100000, 
                   COVID.Positive.Tests.Perc, COVID.Mortality.Perc, yes_periods(input$DemoData)
            ) %>% 
            summarise(., Measure = ylab_clean(),
                      Counties = n(),
                      Min = round(min(get(input$COVIDbuttons)),2),
                      Q1 = round(quantile(get(input$COVIDbuttons), 0.25),2),
                      Median = round(median(get(input$COVIDbuttons)),2),
                      Mean = round(mean(get(input$COVIDbuttons)),2),
                      Q3 = round(quantile(get(input$COVIDbuttons), 0.75),2),
                      Max = round(max(get(input$COVIDbuttons)),2),
                      Range = round(max(get(input$COVIDbuttons)) -
                                        min(get(input$COVIDbuttons)),2),
                      S.Dev = round(sd(get(input$COVIDbuttons)),2),
                      S.Error = round(std.error(get(input$COVIDbuttons)),2),
                      Skew = round(skew(get(input$COVIDbuttons)),2))
        
        comb_data = union_all(demo_datasheet, covid_datasheet)
        datatable(comb_data, rownames = F, 
                  #escape = F
                  # filter = list(position = 'top', clear = FALSE),
                  extensions = c("Buttons", "Select"),
                  options = list(
                      # search = list(regex = TRUE, caseInsensitive = TRUE),
                      pageLength = 5,
                      scrollX=TRUE, 
                      fixedColumns = list(leftColumns = 1),
                      info=F,
                      paging=F,
                      # scrollY=TRUE, 
                      # scrollCollapse=TRUE,
                      select=TRUE,
                      dom = 'Brtip',
                      buttons = c('copy', 'csv', 'excel'
                                  # , 'pdf', 'print'
                      )
                  )
        )
        # options = list(dom= 't'))
    }, width="100%")
    # output$demo_map1<- renderggiraph({
    #     p<- ggplot(CO_MAP_COVID, aes(x=long, y=lat, group = group,
    #                                 fill = get(input$DemoData)
    #                                  )) +
    #         coord_map("mercator") +
    #         geom_polygon_interactive(aes(tooltip = 
    #                      paste(paste0(COUNTY, " COUNTY"),
    #                            paste(xlab_perc(), get(input$DemoData), sep=": "), 
    #                            sep="\n")))
    #         
    #     
    #     ggiraph(code = print(p))
    # })
    
    output$demo_map <- renderGirafe({
        CO_MAP_COVID_DEMO = CO_MAP_COVID %>%
            mutate(.,
                   demo_var = ntile(get(yes_periods(input$DemoData)), 3)
            ) 
        CO_MAP_COVID_DEMO$demo_var %<>%
            gsub(1, "(Low)", .) %>%
            gsub(2, "(Med)", .) %>%
            gsub(3, "(High)", .)
        
        demographic_map = ggplot(CO_MAP_COVID_DEMO, 
                                 aes(x=long, y=lat,
                                     group=group,
                                     #               fill=as.character(demo_var))) +
                                     # scale_fill_manual_interactive(values=c('#e8e8e8','#ace4e4','#5ac8c8')) +
                                     fill=get(yes_periods(input$DemoData)))) +
            scale_fill_gradient_interactive(low='#e8e8e8',high='#00CED1') +
            # scale_fill_gradient_interactive(low='#e8e8e8',high='#5ac8c8') +
            # scale_fill_distiller(palette = "Spectral") +
            # bi_scale_fill(pal = "DkBlue", dim = 3) +
            labs(subtitle = str_wrap(xlab_long_perc(),29)
                 ,fill = str_wrap(xlab_perc(), 8)
            ) +
            theme(
                text=element_text(family="calibri"),
                plot.subtitle = element_text(hjust=1)
                
                # ,
                # legend.title = element_text(size = 5), 
                # legend.text = element_text(size = 4)
                
                  #plot.margin=unit(c(2,2,0,0),"cm")
            ) +
            guides(
                # fill=FALSE, 
            
                
                x.axis=FALSE) +
            #theme(title)
            bi_theme() + coord_map("mercator") +
            xlim(-109.25, -101.75) + ylim(36.75, 41.25) +
            xlab(NULL) + ylab(NULL) + 
            
            geom_polygon_interactive(aes(tooltip = 
                                             paste(paste0(COUNTY, " COUNTY"),
                                                   paste(round(get(yes_periods(input$DemoData)),1), xlab_perc(), demo_var, sep=" "), 
                                                   sep="\n"),
                                         data_id = COUNTY, onclick = WIKI),color="black")
        
        
        demographic_map = demographic_map +
            guides(shape = guide_legend(override.aes = list(size = 1.5))
        ,color = guide_legend(override.aes = list(size = 1.5))
        ) +
            theme(legend.title = element_text(size = 13),
                   legend.text = element_text(size = 12),
                  text=element_text(family="calibri"))
        # ggiraph(code = print(demographic_map))
        girafe(ggobj = demographic_map, options = list(
            # opts_sizing(rescale = TRUE, width = .7),
            opts_hover(css = "opacity:0.8;"),
            opts_tooltip(offx = 20, offy = -55),
            opts_selection(type = "none")
        ))
    })
    
    output$covid_map <- renderGirafe({
        CO_MAP_COVID_COVID = CO_MAP_COVID %>%
            mutate(.,
                   covid_var = ntile(get(input$COVIDbuttons), 3)
            ) 
        CO_MAP_COVID_COVID$covid_var %<>%
            gsub(1, "(Low)", .) %>%
            gsub(2, "(Med)", .) %>%
            gsub(3, "(High)", .)
        
        cov_map = ggplot(CO_MAP_COVID_COVID,
                         aes(x=long, y=lat,
                             group=group,
                             
                             fill=get(input$COVIDbuttons))) +
            scale_fill_gradient_interactive(low='#e8e8e8', high= "#be64ac") +
            # fill=covid_var)) +
            #         scale_fill_manual_interactive(values=c('#e8e8e8', "#dfb0d6", "#be64ac")) +
            # + scale_fill_brewer_interactive(palette = "")
            labs(subtitle = str_wrap(ylab_perc(),28)
                 ,fill = str_wrap(ylab_perc(), 10)
            ) +
            theme(
                text=element_text(family="calibri"),
                plot.subtitle = element_text(hjust=1)
                  #plot.margin=unit(c(2,2,0,0),"cm")
            ) +
            guides(
                # fill=FALSE, 
                x.axis=FALSE) +
            #theme(title)
            bi_theme() + coord_map("mercator") +
            xlim(-109.25, -101.75) + ylim(36.75, 41.25) +
            xlab(NULL) + ylab(NULL) +
            #coord_map("polyconic" ) #+
            geom_polygon_interactive(aes(tooltip = 
                                             paste(paste0(COUNTY, " COUNTY"),
                                                   paste(formatC(get(input$COVIDbuttons), format="f", big.mark = ",", digits=1), ylab_perc(), covid_var, sep=" "), 
                                                   sep="\n"),
                                         data_id = COUNTY, onclick = WIKI),color="black")
        
        cov_map = cov_map +
            guides(shape = guide_legend(override.aes = list(size = 1.5))
                   ,color = guide_legend(override.aes = list(size = 1.5))
            ) +
            theme(legend.title = element_text(size = 13),
                  legend.text = element_text(size = 12),
                  text=element_text(family="calibri"))
        # ggiraph(code = print(cov_map))
        girafe(ggobj = cov_map, options = list(
            # opts_sizing(rescale = TRUE, width = .7),
            opts_hover(css = "opacity:0.8;"),
            opts_tooltip(offx = 20, offy = -55),
            opts_selection(type = "none")
        ))
    })
    output$corr_map <- renderGirafe({
        
        
        corr_map_out = ggplot(CO_MAP_COVID_BAL,
                              aes(x=long, y=lat,
                                  group=group,
                                  fill=get(input$balancebuttons))) +
            # scale_fill_gradient_interactive(low='#e8e8e8', high= "#be64ac") +
            # fill=covid_var)) +
            #         scale_fill_manual_interactive(values=c('#e8e8e8', "#dfb0d6", "#be64ac")) +
            scale_fill_brewer_interactive(palette = "Set1") +
            labs(title = paste0("Map of Counties by ",bal_lab_clean()), 
                 fill=bal_lab_clean()
            ) +
            
            guides(fill=FALSE) +
            #theme(title)
            bi_theme() + coord_map("mercator") +
            xlim(-109.25, -101.75) + ylim(36.75, 41.25) +
            xlab(NULL) + ylab(NULL) +
            #coord_map("polyconic" ) #+
            geom_polygon_interactive(aes(tooltip = 
                                             paste(paste0(COUNTY, " COUNTY"),
                                                   get(input$balancebuttons),
                                                   paste0("Population: ", formatC(Population, format="f", big.mark = ",", digits=0)),
                                                   paste0("Median Income: $", formatC(Median.Household.Income, format="f", big.mark = ",", digits=0)),
                                                   paste0(Perc.Rural, " % Rural"),
                                                   paste(get(yes_periods(input$DemoData)),xlab_perc(), sep=" "),
                                                   paste(formatC(get(input$COVIDbuttons),format="f", big.mark = ",", digits=1),ylab_perc(), sep=" "),
                                                   sep="\n"),
                                         data_id = COUNTY, onclick = WIKI),
                                     color="black") +
            theme(
                text=element_text(family="calibri"),
                legend.title = element_text(color="darkblue", face = "bold", size=13),
                legend.position = "right",
                legend.text = element_text(color="darkblue", face = "bold", size=11),
                plot.title = element_text(hjust=0.5, size=17),
                axis.title = element_text(face="bold", size=12, vjust=1),
                strip.text.x = element_text(face="bold"),
                legend.background =
                    element_rect(fill="#F2F5F7", color="gray", size=1)
                ,plot.margin=unit(c(-2.2,0,0,0),"cm")
            ) 
        
        girafe(ggobj = corr_map_out, options = list(
            opts_sizing(rescale = TRUE, width = .7),
            opts_hover(css = "opacity:0.8;"),
            opts_tooltip(offx = 20, offy = -55),
            opts_selection(type = "none")
        ))
        # ggiraph(code = print(corr_map_out))
    })
    #map2
    # legend2 = bi_legend(pal = "DkBlue",
    #                     dim = 3,
    #                     xlab = paste0("Higher ", xlab_perc),
    #                     ylab = paste0("Higher ", ylab_short),
    #                     size = 7)
    #
    # BiVarPlotMap = ggdraw() +
    #     draw_plot(map2, -0.09, 0, 1, 1) +
    #     draw_plot(legend2, 0.64, 0.25, 0.35, 0.47)
    # BiVarPlotMap
    
    
    output$bi_map <- renderGirafe({
        CO_MAP_COVID1 = CO_MAP_COVID %>%
            mutate(.,
                   var1 = ntile(get(yes_periods(input$DemoData)), 3),
                   var2 = ntile(get(input$COVIDbuttons), 3),
                   bi_class = paste0(
                       as.numeric(var1), "-", 
                       as.numeric(var2)
                   ))
        CO_COUNTY_BI_CLASS = CO_COUNTY_COVID_FILTER %>%
            mutate(.,
                   demo_var = ntile(get(yes_periods(input$DemoData)), 3),
                   covid_var = ntile(get(input$COVIDbuttons), 3),
                   bi_class = paste0(
                       as.numeric(demo_var), "-",
                       as.numeric(covid_var)
                   )) %>%
            select(., COUNTY, demo_var, covid_var, bi_class)
        bi_var_table = CO_COUNTY_BI_CLASS %>% 
            group_by(bi_class) %>% 
            summarise(counties = n())
        CO_MAP_COVID1 = left_join(CO_MAP_COVID, CO_COUNTY_BI_CLASS, by="COUNTY")
        CO_MAP_COVID1$bi_class %<>%
            gsub("NA-1", NA, .) %>%
            gsub("NA-2", NA, .) %>%
            gsub("NA-3", NA, .) %>% 
            gsub("1-NA", NA, .) %>% 
            gsub("2-NA", NA, .) %>% 
            gsub("3-NA", NA, .) #%>% 
            # gsub("1-1", "Both Low", .) %>%
            # gsub("1-2", "Low Demo / Med COVID", .) %>%
            # gsub("1-3", "Low Demo / High COVID", .) %>% 
            # gsub("2-1", "Med Demo / Low COVID", .) %>% 
            # gsub("2-2", "Both Med", .) %>% 
            # gsub("2-3", "Med Demo / High COVID", .) %>% 
            # gsub("3-1", "High Demo / Low COVID", .) %>% 
            # gsub("3-2", "High Demo / Med COVID", .) %>% 
            # gsub("3-3", "Both High", .)
            
        # CO_MAP_COVID1$demo_var %<>%
        #     gsub(1, paste0("Low ",xlab_perc()), .) %>%
        #     gsub(2, paste0("Med ",xlab_perc()), .) %>%
        #     gsub(3, paste0("High ",xlab_perc()), .)
        # CO_MAP_COVID1$covid_var %<>%
        #     gsub(1, paste0("Low ",ylab_perc()), .) %>%
        #     gsub(2, paste0("Med ",ylab_perc()), .) %>%
        #     gsub(3, paste0("High ",ylab_perc()), .)
        CO_MAP_COVID1$demo_var %<>%
            gsub(1, "Low", .) %>%
            gsub(2, "Med", .) %>%
            gsub(3, "High", .)
        CO_MAP_COVID1$covid_var %<>%
            gsub(1, "Low", .) %>%
            gsub(2, "Med", .) %>%
            gsub(3, "High", .)
        map2 = ggplot(CO_MAP_COVID1,
                      aes(x=long, y=lat,
                          group=group,
                          fill=bi_class)) +
            bi_scale_fill(pal = "DkBlue", dim = 3) +
            labs(subtitle = str_wrap(paste(ylab_long_perc(), xlab_long_perc(), sep="\nvs. "), 20)
                 # ,x = "Bivariate Analysis")
            ) + 
            
            theme(text=element_text(family="calibri"),
                  plot.subtitle = element_text(hjust=1, size = 8),
                  legend.title = element_blank()
                  #plot.margin=unit(c(2,2,0,0),"cm")
            ) +
            guides(
                fill=FALSE,
                   x.axis=FALSE) +
            #theme(title)
            bi_theme() + coord_map("mercator") +
            xlim(-109.25, -101.75) + ylim(36.75, 41.25) +
            xlab(NULL) + ylab(NULL) +
            #coord_map("polyconic" ) #+
            geom_polygon_interactive(aes(tooltip = 
                                             paste(paste0(COUNTY, " COUNTY"),
                                                   paste(round(get(yes_periods(input$DemoData)),1), xlab_perc(), 
                                                         paste0("(",demo_var,")"), sep=" "), 
                                                   paste(formatC(get(input$COVIDbuttons), format="f", big.mark = ",", digits=1), ylab_frontspace(), 
                                                         paste0(" (",covid_var,") "),
                                                         sep=""), sep="\n"),
                                         data_id = COUNTY, onclick = WIKI
            ),color="black")
        
        map2 = map2 +
        #     # bi_scale_fill(pal = "DkBlue", dim = 3) +
        #     scale_fill_manual(#name = "County Types",
        #         # labels = c("Both Low", "Low Demo / Med COVID", "Low Demo / High COVID",
        #         #            "Med Demo / Low COVID", "Both Medium", "Med Demo / High COVID",
        #         #            "High Demo / Low COVID", "High Demo / Med COVID", "Both High"),
        #         labels = c(paste0("Both Low (", bi_var_table$counties[1], ")"), 
        #                    paste0("Low Demo / Med COVID (", bi_var_table$counties[2], ")"), 
        #                    paste0("Low Demo / High COVID (", bi_var_table$counties[3], ")"),
        #                    paste0("Med Demo / Low COVID (", bi_var_table$counties[4], ")"), 
        #                    paste0("Both Medium (", bi_var_table$counties[5], ")"), 
        #                    paste0("Med Demo / High COVID (", bi_var_table$counties[6], ")"),
        #                    paste0("High Demo / Low COVID (", bi_var_table$counties[7], ")"), 
        #                    paste0("High Demo / Med COVID (", bi_var_table$counties[8], ")"), 
        #                    paste0("Both High (", bi_var_table$counties[9], ")")),
        #         # values = bi_colors) +
        #         values = c("#e8e8e8","#dfb0d6","#be64ac",
        #                    "#ace4e4","#a5add3","#8c62aa",
        #                    "#5ac8c8","#5698b9","#3b4994")) +
            #     
            # guides(shape = guide_legend(override.aes = list(size = 0.6))
            #        ,fill = guide_legend(override.aes = list(size = 0.4))
            # ) +
            theme(text=element_text(family="calibri"))
            #       legend.text = element_text(size = 8)) #+ 
        
        # ggiraph(code = print(map2))
        girafe(ggobj = map2, options = list(
            # opts_sizing(rescale = TRUE, width = .7),
            opts_hover(css = "opacity:0.8;"),
            opts_tooltip(offx = 20, offy = -55),
            opts_selection(type = "none"
                           , only_shiny = FALSE
            )
        ))
    })
    output$bi_map_vlegend <- renderPlot({
        CO_MAP_COVID1 = CO_MAP_COVID %>%
            mutate(.,
                   var1 = ntile(get(yes_periods(input$DemoData)), 3),
                   var2 = ntile(get(input$COVIDbuttons), 3),
                   bi_class = paste0(
                       as.numeric(var1), "-", 
                       as.numeric(var2)
                   ))
        CO_COUNTY_BI_CLASS = CO_COUNTY_COVID_FILTER %>%
            mutate(.,
                   demo_var = ntile(get(yes_periods(input$DemoData)), 3),
                   covid_var = ntile(get(input$COVIDbuttons), 3),
                   bi_class = paste0(
                       as.numeric(demo_var), "-",
                       as.numeric(covid_var)
                   )) %>%
            select(., COUNTY, demo_var, covid_var, bi_class)
        bi_var_table = CO_COUNTY_BI_CLASS %>% 
            group_by(bi_class) %>% 
            summarise(counties = n())
        CO_MAP_COVID1 = left_join(CO_MAP_COVID, CO_COUNTY_BI_CLASS, by="COUNTY")
        CO_MAP_COVID1$bi_class %<>%
            gsub("NA-1", NA, .) %>%
            gsub("NA-2", NA, .) %>%
            gsub("NA-3", NA, .) %>% 
            gsub("1-NA", NA, .) %>% 
            gsub("2-NA", NA, .) %>% 
            gsub("3-NA", NA, .) #%>% 
        # gsub("1-1", "Both Low", .) %>%
        # gsub("1-2", "Low Demo / Med COVID", .) %>%
        # gsub("1-3", "Low Demo / High COVID", .) %>% 
        # gsub("2-1", "Med Demo / Low COVID", .) %>% 
        # gsub("2-2", "Both Med", .) %>% 
        # gsub("2-3", "Med Demo / High COVID", .) %>% 
        # gsub("3-1", "High Demo / Low COVID", .) %>% 
        # gsub("3-2", "High Demo / Med COVID", .) %>% 
        # gsub("3-3", "Both High", .)
        
        # CO_MAP_COVID1$demo_var %<>%
        #     gsub(1, paste0("Low ",xlab_perc()), .) %>%
        #     gsub(2, paste0("Med ",xlab_perc()), .) %>%
        #     gsub(3, paste0("High ",xlab_perc()), .)
        # CO_MAP_COVID1$covid_var %<>%
        #     gsub(1, paste0("Low ",ylab_perc()), .) %>%
        #     gsub(2, paste0("Med ",ylab_perc()), .) %>%
        #     gsub(3, paste0("High ",ylab_perc()), .)
        CO_MAP_COVID1$demo_var %<>%
            gsub(1, "Low", .) %>%
            gsub(2, "Med", .) %>%
            gsub(3, "High", .)
        CO_MAP_COVID1$covid_var %<>%
            gsub(1, "Low", .) %>%
            gsub(2, "Med", .) %>%
            gsub(3, "High", .)
        leg2 = ggplot(CO_MAP_COVID1,
                      aes(x=long, y=lat,
                          group=group,
                          fill=bi_class)) +
            bi_scale_fill(pal = "DkBlue", dim = 3) +
            labs(subtitle = str_wrap(paste(ylab_long_perc(), xlab_long_perc(), sep="\nvs. "), 20)
                 # ,x = "Bivariate Analysis")
            ) + 
            
            theme(text=element_text(family="calibri"),
                  plot.subtitle = element_text(hjust=1, size = 8)
                  # ,
                  # legend.title = element_blank()
                  # #plot.margin=unit(c(2,2,0,0),"cm")
            ) +
            guides(
                # fill=FALSE, 
                x.axis=FALSE) +
            #theme(title)
            bi_theme() + coord_map("mercator") +
            xlim(-109.25, -101.75) + ylim(36.75, 41.25) +
            xlab(NULL) + ylab(NULL) +
            #coord_map("polyconic" ) #+
            geom_polygon_interactive(aes(tooltip = 
                                             paste(paste0(COUNTY, " COUNTY"),
                                                   paste(round(get(yes_periods(input$DemoData)),1), xlab_perc(), 
                                                         paste0("(",demo_var,")"), sep=" "), 
                                                   paste(formatC(get(input$COVIDbuttons), format="f", big.mark = ",", digits=1), ylab_frontspace(), 
                                                         paste0(" (",covid_var,") "),
                                                         sep=""), sep="\n"),
                                         data_id = COUNTY, onclick = WIKI
            ),color="black")
        
        leg2 = leg2 +
            # bi_scale_fill(pal = "DkBlue", dim = 3) +
            scale_fill_manual(name = str_wrap(paste0("Colorado's 64 Counties by\n", xlab_long_perc(), " vs. \n", ylab_long_perc()), 30),
                # labels = c("Both Low", "Low Demo / Med COVID", "Low Demo / High COVID",
                #            "Med Demo / Low COVID", "Both Medium", "Med Demo / High COVID",
                #            "High Demo / Low COVID", "High Demo / Med COVID", "Both High"),
                labels = c(paste0("Both Low (", bi_var_table$counties[1], ")"), 
                           paste0("Low Demo / Med COVID (", bi_var_table$counties[2], ")"), 
                           paste0("Low Demo / High COVID (", bi_var_table$counties[3], ")"),
                           paste0("Med Demo / Low COVID (", bi_var_table$counties[4], ")"), 
                           paste0("Both Medium (", bi_var_table$counties[5], ")"), 
                           paste0("Med Demo / High COVID (", bi_var_table$counties[6], ")"),
                           paste0("High Demo / Low COVID (", bi_var_table$counties[7], ")"), 
                           paste0("High Demo / Med COVID (", bi_var_table$counties[8], ")"), 
                           paste0("Both High (", bi_var_table$counties[9], ")")),
                # values = bi_colors) +
                values = c("#e8e8e8","#dfb0d6","#be64ac",
                           "#ace4e4","#a5add3","#8c62aa",
                           "#5ac8c8","#5698b9","#3b4994")) +
            
            # guides(shape = guide_legend(override.aes = list(size = 4))
            #        ,color = guide_legend(override.aes = list(size = 4))
            # ) +
            theme(#legend.title = element_blank()
                  legend.spacing = unit(0.4,'cm'),
                  legend.title = element_text(hjust=0.5,size = 22, face="bold", family="calibri"), #+
                  legend.text = element_text(size = 20, family="calibri"
                                             ,margin=margin(t = 0.2, b=0.2, l=0.2, unit="cm")
                                             )
            )
        leg2 = leg2 + theme(text=element_text(family="serif"))
        legend <- cowplot::get_legend(leg2)
        
        grid.newpage()
        grid.draw(legend)
        
        # ggiraph(code = print(map2))
        # girafe(code = print(legend), options = list(
        #     # opts_sizing(rescale = TRUE, width = .7),
        #     opts_hover(css = "opacity:0.8;"),
        #     opts_tooltip(offx = 20, offy = -55),
        #     opts_selection(type = "none"
        #                    , only_shiny = FALSE
            # )
        # ))
    })
    output$bi_cor <- renderGirafe({
        CO_COUNTY_BI_SLIDERS = CO_COUNTY_COVID_FILTER %>%
            mutate(., 
                   demo_var = ntile(get(yes_periods(input$DemoData)), 3),
                   covid_var = ntile(get(input$COVIDbuttons), 3),
                   bi_class = paste0(
                       as.numeric(demo_var), "-",
                       as.numeric(covid_var)
                   ),
                   COVID_MAX_var = ntile(COVID.Cases.Max, 3),
                   INCOME_var = ntile(Median.Household.Income, 3),
                   # RURAL_var = ntile(Perc.Rural, 3)
                   RURAL_var = ifelse(Perc.Rural<25,1,
                                      ifelse((Perc.Rural>=25 & Perc.Rural<50),2,
                                             ifelse((Perc.Rural>=50 & Perc.Rural<100),3,
                                                    ifelse(Perc.Rural==100,4,NA))))
            ) %>% 
            select(., COUNTY, demo_var, covid_var, bi_class, 
                   COVID_MAX_var, INCOME_var, RURAL_var) 
        CO_COUNTY_BI_SLIDERS$bi_class %<>% 
            gsub("NA-1", NA, .) %>% 
            gsub("NA-2", NA, .) %>% 
            gsub("NA-3", NA, .) %>% 
            gsub("1-NA", NA, .) %>% 
            gsub("2-NA", NA, .) %>% 
            gsub("3-NA", NA, .)
        
        
        #ALL COUNTIES
        pos_cor = CO_COUNTY_BI_SLIDERS %>% 
            filter(., bi_class=="1-1" | bi_class=="2-2" | bi_class=="3-3") %>% 
            nrow()
        neg_cor = CO_COUNTY_BI_SLIDERS %>% 
            filter(., bi_class=="1-3" | bi_class=="2-2" | bi_class=="3-1") %>% 
            nrow()
        
        #HIGH CASES
        pos_high_case_cor = CO_COUNTY_BI_SLIDERS %>% 
            filter(., (bi_class=="1-1" | bi_class=="2-2" | bi_class=="3-3")
                   & COVID_MAX_var=="3") %>% 
            nrow()
        neg_high_case_cor = CO_COUNTY_BI_SLIDERS %>% 
            filter(., (bi_class=="1-3" | bi_class=="2-2" | bi_class=="3-1")
                   & COVID_MAX_var=="3") %>% 
            nrow()
        #LOW CASES
        pos_low_case_cor = CO_COUNTY_BI_SLIDERS %>% 
            filter(., (bi_class=="1-1" | bi_class=="2-2" | bi_class=="3-3")
                   & COVID_MAX_var=="1") %>% 
            nrow()
        neg_low_case_cor = CO_COUNTY_BI_SLIDERS %>% 
            filter(., (bi_class=="1-3" | bi_class=="2-2" | bi_class=="3-1")
                   & COVID_MAX_var=="1") %>% 
            nrow()
        
        #HIGH INCOME
        pos_high_income_cor = CO_COUNTY_BI_SLIDERS %>% 
            filter(., (bi_class=="1-1" | bi_class=="2-2" | bi_class=="3-3")
                   & INCOME_var=="3") %>% 
            nrow()
        neg_high_income_cor = CO_COUNTY_BI_SLIDERS %>% 
            filter(., (bi_class=="1-3" | bi_class=="2-2" | bi_class=="3-1")
                   & INCOME_var=="3") %>% 
            nrow()
        #LOW INCOME
        pos_low_income_cor = CO_COUNTY_BI_SLIDERS %>% 
            filter(., (bi_class=="1-1" | bi_class=="2-2" | bi_class=="3-3")
                   & INCOME_var=="1") %>% 
            nrow()
        neg_low_income_cor = CO_COUNTY_BI_SLIDERS %>% 
            filter(., (bi_class=="1-3" | bi_class=="2-2" | bi_class=="3-1")
                   & INCOME_var=="1") %>% 
            nrow()
        
        #100% RURAL
        pos_100_rural_cor = CO_COUNTY_BI_SLIDERS %>% 
            filter(., (bi_class=="1-1" | bi_class=="2-2" | bi_class=="3-3")
                   & RURAL_var=="4") %>% 
            nrow()
        neg_100_rural_cor = CO_COUNTY_BI_SLIDERS %>% 
            filter(., (bi_class=="1-3" | bi_class=="2-2" | bi_class=="3-1")
                   & RURAL_var=="4") %>% 
            nrow()
        #MOSTLY RURAL
        pos_most_rural_cor = CO_COUNTY_BI_SLIDERS %>% 
            filter(., (bi_class=="1-1" | bi_class=="2-2" | bi_class=="3-3")
                   & RURAL_var=="3") %>% 
            nrow()
        neg_most_rural_cor = CO_COUNTY_BI_SLIDERS %>% 
            filter(., (bi_class=="1-3" | bi_class=="2-2" | bi_class=="3-1")
                   & RURAL_var=="3") %>% 
            nrow()
        #MOSTLY URBAN
        pos_most_urban_cor = CO_COUNTY_BI_SLIDERS %>% 
            filter(., (bi_class=="1-1" | bi_class=="2-2" | bi_class=="3-3")
                   & RURAL_var=="2") %>% 
            nrow()
        neg_most_urban_cor = CO_COUNTY_BI_SLIDERS %>% 
            filter(., (bi_class=="1-3" | bi_class=="2-2" | bi_class=="3-1")
                   & RURAL_var=="2") %>% 
            nrow()
        #URBAN
        pos_urban_cor = CO_COUNTY_BI_SLIDERS %>% 
            filter(., (bi_class=="1-1" | bi_class=="2-2" | bi_class=="3-3")
                   & RURAL_var=="1") %>% 
            nrow()
        neg_urban_cor = CO_COUNTY_BI_SLIDERS %>% 
            filter(., (bi_class=="1-3" | bi_class=="2-2" | bi_class=="3-1")
                   & RURAL_var=="1") %>% 
            nrow()
        
        
        perc_pos_cor = round(pos_cor/64*100,1)
        perc_neg_cor = round(neg_cor/64*100,1)
        perc_pos_high_case_cor = round(pos_high_case_cor/21*100,1)
        perc_neg_high_case_cor = round(neg_high_case_cor/21*100,1)
        perc_pos_low_case_cor = round(pos_low_case_cor/22*100,1)
        perc_neg_low_case_cor = round(neg_low_case_cor/22*100,1)
        perc_pos_high_income_cor = round(pos_high_income_cor/21*100,1)
        perc_neg_high_income_cor = round(neg_high_income_cor/21*100,1)
        perc_pos_low_income_cor = round(pos_low_income_cor/22*100,1)
        perc_neg_low_income_cor = round(neg_low_income_cor/22*100,1)
        perc_pos_100_rural_cor = round(pos_100_rural_cor/24*100,1)
        perc_neg_100_rural_cor = round(neg_100_rural_cor/24*100,1)
        perc_pos_most_rural_cor = round(pos_most_rural_cor/10*100,1)
        perc_neg_most_rural_cor = round(neg_most_rural_cor/10*100,1)
        perc_pos_most_urban_cor = round(pos_most_urban_cor/15*100,1)
        perc_neg_most_urban_cor = round(neg_most_urban_cor/15*100,1)
        perc_pos_urban_cor = round(pos_urban_cor/15*100,1)
        perc_neg_urban_cor = round(neg_urban_cor/15*100,1)
        
        
        maptable2 = data.frame(
            County.Type = rep(c("All", "High COVID", "Low COVID", "High Income", 
                                "Low Income", "100% Rural", "Mostly Rural", "Somewhat Rural", 
                                "Urban/Suburban"), each=2),
            Correlation = rep(c("Positive", "Negative"), 9),
            Total.Counties = rep(c(64, 21, 22,21, 22, 24, 10, 15, 15), each=2),
            Number.of.Counties = c(pos_cor, neg_cor, 
                                   pos_high_case_cor, neg_high_case_cor,
                                   pos_low_case_cor, neg_low_case_cor,
                                   pos_high_income_cor, neg_high_income_cor,
                                   pos_low_income_cor, neg_low_income_cor,
                                   pos_100_rural_cor, neg_100_rural_cor,
                                   pos_most_rural_cor, neg_most_rural_cor,
                                   pos_most_urban_cor, neg_most_urban_cor,
                                   pos_urban_cor, neg_urban_cor),
            Perc.Counties = c(perc_pos_cor, perc_neg_cor, 
                              perc_pos_high_case_cor, perc_neg_high_case_cor,
                              perc_pos_low_case_cor, perc_neg_low_case_cor,
                              perc_pos_high_income_cor, perc_neg_high_income_cor,
                              perc_pos_low_income_cor, perc_neg_low_income_cor,
                              perc_pos_100_rural_cor, perc_neg_100_rural_cor,
                              perc_pos_most_rural_cor, perc_neg_most_rural_cor,
                              perc_pos_most_urban_cor, perc_neg_most_urban_cor,
                              perc_pos_urban_cor, perc_neg_urban_cor)
        )
        maptable2 = maptable2 %>% 
            mutate(Perc.0_1.Counties = Perc.Counties/100)
        maptable2$Correlation =
            factor(maptable2$Correlation,
                   levels = c("Negative", "Positive"))
        maptable2$County.Type = 
            factor(maptable2$County.Type, 
                   levels = c( "100% Rural", "Mostly Rural", 
                               "Somewhat Rural", "Urban/Suburban", "Low Income", "High Income", 
                               "Low COVID", "High COVID", "All"))
        
        maptable2disp = 
            maptable2 %>% 
            ggplot(aes(x=County.Type, y=Perc.0_1.Counties, fill=Correlation)) + 
            # geom_col(width=0.8, position=position_dodge(width=0.8)) + 
            # theme(
                # axis.text.x = element_text(angle=90, hjust=1, vjust=0.3)
            # ) + 
            xlab("County Category") +
            ylab("Percent of Counties with Correlation") +
            scale_y_continuous(labels = scales::percent) +
            labs(title="Counties with Influencing Factors:\nStrong Positive or Negative Correlation", 
                 subtitle = paste0(ylab_short(), " vs. ", xlab_long_perc())
                 # ,caption = "Hover over any bar to see County Type statistics"
                 ) +
            theme(text=element_text(family="calibri"),
                  panel.grid.major.y = element_blank(),
                  axis.text.x = element_text(face="bold"),
                  axis.text.y = element_text(face="bold"),
                  legend.title = element_text(color="darkblue", face="bold", size=13, hjust=0),
                  legend.text = element_text(face="bold", color="darkblue", size=11),
                  plot.title = element_text(hjust=0.5, face="bold", size=18),
                  plot.subtitle = element_text(hjust=0.5, color="darkblue", size=14),
                  axis.title = element_text(face="bold", size=12),
                  strip.text.x = element_text(face="bold"),
                  legend.background =
                      element_rect(fill="#F2F5F7", color="gray", size=1)) +
            geom_bar_interactive(stat="identity", 
                                 aes(order=Correlation, 
                                     tooltip = paste0(paste0(Perc.Counties, "%"),
                                                      paste0(" (", Number.of.Counties, "/", Total.Counties, ") of ", County.Type, " Counties"),
                                                      paste0("\nhave a ", Correlation, " Correlation")
                                     ),
                                     data_id = County.Type
                                 ),
                                 width=0.8, 
                                 position=position_dodge2(width=0.8)) +
            coord_flip() +
            guides(fill = guide_legend(reverse = TRUE)) 
        
        # ggiraph(code=print(maptable2disp))
        girafe(ggobj = maptable2disp, options = list(
            # opts_sizing(rescale = TRUE, width = .7),
            opts_hover(css = "opacity:0.8;"),
            opts_tooltip(offx = 20, offy = -55),
            opts_selection(type = "none"
                           , only_shiny = FALSE
            )
        ))
        
    })
    output$bi_analysis <- renderPlot({
        CO_COUNTY_BI_CLASS = CO_COUNTY_COVID_FILTER %>%
            mutate(., 
                   demo_var = ntile(get(yes_periods(input$DemoData)), 3),
                   covid_var = ntile(get(input$COVIDbuttons), 3),
                   bi_class = paste0(
                       as.numeric(demo_var), "-",
                       as.numeric(covid_var)
                   )) %>% 
            select(., COUNTY, demo_var, covid_var, bi_class)
        CO_COUNTY_BI_CLASS$bi_class %<>% 
            gsub("NA-1", NA, .) %>% 
            gsub("NA-2", NA, .) %>% 
            gsub("NA-3", NA, .) %>% 
            gsub("1-NA", NA, .) %>% 
            gsub("2-NA", NA, .) %>% 
            gsub("3-NA", NA, .)
        #TRYING TO make a Matrix
        bi_var_table = CO_COUNTY_BI_CLASS %>% 
            group_by(bi_class) %>% 
            summarise(counties = n())
        bi_matrix = matrix(bi_var_table$counties, nrow=3, ncol=3)
        bi_matrix = t(bi_matrix)
        bi_matrix = rotate(rotate(rotate(bi_matrix)))
        
        bi_matplot = color2D.matplot(bi_matrix, axes=F, 
                                     xlab = paste0("Higher ", str_wrap(xlab_perc(),29), " ->"),
                                     ylab = paste0("Higher ", ylab_perc(), " ->"),
                                     cellcolors = bi_colors,
                                     # bi_colors, 
                                     border=NA, show.values = 0.5)
        # ggex = ggplot(CO_MAP_COVID, aes(x=get(input$DemoData), 
        #               y=get(input$COVIDbuttons))) +
        #     geom_point() + 
        #     xlab(paste0("Higher ", str_wrap(xlab_perc(),29), " ->")) +
        #      ylab(paste0("Higher ", ylab_perc(), " ->")) +
        #     labs(title = "Legend with division of 64 Counties"
        #          ) +
        #     theme(axis.text.x = element_text(face="bold"),
        #           axis.text.y = element_text(face="bold"),
        #           plot.title = element_text(face="bold", size=18),
        #           axis.title = element_text(face="bold", size=12))
        # bi_matplot
        bi_matplot
        # +
        #     get_y_axis(ggex, position = "left") +
        #     get_title(ggex)
        
        
    })
    # output$bi_map_legend <- renderPlot({
    #     CO_COUNTY_BI_CLASS = CO_COUNTY_COVID_FILTER %>%
    #         mutate(., 
    #                demo_var = ntile(get(yes_periods(input$DemoData)), 3),
    #                covid_var = ntile(get(input$COVIDbuttons), 3),
    #                bi_class = paste0(
    #                    as.numeric(demo_var), "-",
    #                    as.numeric(covid_var)
    #                )) %>% 
    #         select(., COUNTY, demo_var, covid_var, bi_class)
    #     CO_COUNTY_BI_CLASS$bi_class %<>% 
    #         gsub("NA-1", NA, .) %>% 
    #         gsub("NA-2", NA, .) %>% 
    #         gsub("NA-3", NA, .) %>% 
    #         gsub("1-NA", NA, .) %>% 
    #         gsub("2-NA", NA, .) %>% 
    #         gsub("3-NA", NA, .)
    #     #TRYING TO make a Matrix
    #     bi_var_table = CO_COUNTY_BI_CLASS %>%
    #         group_by(bi_class) %>%
    #         summarise(counties = n())
    #     bi_matrix = matrix(bi_var_table$counties, nrow=3, ncol=3)
    #     bi_matrix = t(bi_matrix)
    #     bi_matrix = rotate(rotate(rotate(bi_matrix)))
    #     bi_colors = c('#4575b4', '#74add1', '#abd9e9', '#e0f3f8', '#ffffbf', '#fee090',
    #     '#fdae61', '#f46d43', '#d73027')
    #     bi_colors_matrix = matrix(bi_colors, nrow=3, ncol=3)
    #     color2D.matplot(bi_matrix, axes=T,
    #                     xlab = paste0("Higher ", str_wrap(xlab_perc(),29), expression("" %->% "")),
    #                     ylab = paste0("Higher ", ylab_short(),expression("" %->% "")),
    #                     cellcolors = c([['#4575b4', '#74add1', '#abd9e9'],
    #                                       ['#e0f3f8', '#ffffbf', '#fee090'],
    #                                       ['#fdae61', '#f46d43', '#d73027']])
    #                     )
        
        
        # bi_df = as.data.frame(bi_matrix)
        # #### This has correct numbers. Now to just paste them...
        # legend2 = bi_legend(pal = "DkBlue",
        #                     dim = 3,
        #                     #xlab = paste0("Higher ", xlab_perc()),
        #                     xlab = paste0("Higher ", str_wrap(xlab_perc(),29)),
        #                     ylab = paste0("Higher ", ylab_short()),
        #                     # xlab = "xlab",
        #                     # ylab = "ylab",
        #                     size = 18)
        # legend2
        # BiVarPlotMap = ggdraw() +
        #     draw_label(bi_df) +
        # #     draw_plot(map2, -0.09, 0, 1, 1) +
        # #     draw_plot(legend2, 0.64, 0.25, 0.35, 0.47)
        #         draw_plot(legend2, 0, 0.06, 1, 0.9)
        # BiVarPlotMap
        # ggiraph(code = print(legend2))
        #ggiraph(code = print(BiVarPlotMap))
        # ggdraw(legend2) +
        #     draw_label(bi_var_table$counties[1], size=30, hjust=0.33, vjust=0.25) +
        #     draw_label(bi_var_table$counties[2], size=30, x=0.33, y=0.50) +
        #     draw_label(bi_var_table$counties[3], size=30, x=0.33, y=0.75) +
        #     draw_label(bi_var_table$counties[4], size=30, x=0.50, y=0.25) +
        #     draw_label(bi_var_table$counties[5], size=30, x=0.50, y=0.50) +
        #     draw_label(bi_var_table$counties[6], size=30, hjust=0.50, vjust=0.75) +
        #     draw_label(bi_var_table$counties[7], size=30, x=0.67, y=0.25) +
        #     draw_label(bi_var_table$counties[8], size=30, x=0.67, y=0.50) +
        #     draw_label(bi_var_table$counties[9], size=30, hjust=0.67, vjust=0.75) 
    # })
    
    
    
    output$Race_Stats <- renderDataTable({
        unique(CO_Race_Measures_COVID19$Test.Type)
        colnames(CO_Race_Measures_COVID19)
        # colnames(CO_Race_Measures_COVID19)
        #### RACE_DATA STATS ####
        racestats = CO_Race_Measures_COVID19 %>%
            # select(., COUNTY, Demographic, COVID.Testing.Per.100000,
            #        COVID.Cases.Per.100000,  COVID.Deaths.Per.100000,
            #        COVID.Mortality.Perc, Perc.Children.in.Poverty) %>%
            filter(., !is.na(get(input$RaceDemo)) &
                       !is.na(get(input$COVIDbuttons)) &
                       !is.na(COUNTY) &
                       Test.Type == "Measure"
                   # & COVID.Cases.Max >= min(input$covidrange1)
                   # & COVID.Cases.Max <= max(input$covidrange1)
                   # & Median.Household.Income >= min(input$medianrange1)
                   # & Median.Household.Income <= max(input$medianrange1)
                   # & Perc.Rural >= min(input$ruralrange1)
                   # & Perc.Rural <= max(input$ruralrange1)
            ) %>%
            group_by(., Demographic) %>%
            summarise(., Measure = race_xlab_long_perc(),
                      Counties = n(),
                      Min = round(min(get(input$RaceDemo)),2),
                      Q1 = round(quantile(get(input$RaceDemo), 0.25),2),
                      Median = round(median(get(input$RaceDemo)),2),
                      Mean = round(mean(get(input$RaceDemo)),2),
                      Q3 = round(quantile(get(input$RaceDemo), 0.75),2),
                      Max = round(max(get(input$RaceDemo)),2),
                      Range = round(max(get(input$RaceDemo))-
                                        min(get(input$RaceDemo)),2),
                      S.Dev = round(sd(get(input$RaceDemo)),2),
                      S.Error = round(std.error(get(input$RaceDemo)),2),
                      Skew = round(skew(get(input$RaceDemo)),2)) %>%
            arrange(., desc(Counties))
        
        datatable(racestats, rownames = F, 
                  # filter = list(position = 'top', clear = FALSE),
                  extensions = c("Buttons", "Select"),
                  options = list(
                      # search = list(regex = TRUE, caseInsensitive = TRUE),
                      pageLength = 6,
                      scrollX=TRUE, 
                      fixedColumns = list(leftColumns = 1),
                      info=F,
                      paging=F,
                      # scrollY=TRUE, 
                      # scrollCollapse=TRUE,
                      select=TRUE,
                      dom = 'Brtip',
                      buttons = c('copy', 'csv', 'excel'
                                  # , 'pdf', 'print'
                      )
                  )
        )
        
        # covid_datasheet = CO_COUNTY_COVID_FILTER %>% 
        #     filter(., #Demographic != "ALL" & 
        #            !is.na(get(input$COVIDbuttons)) &
        #                !is.na(COUNTY)
        #     ) %>% 
        #     select(., COUNTY, COVID.Tests.Per.100000, 
        #            COVID.Cases.Per.100000,  COVID.Deaths.Per.100000, 
        #            COVID.Mortality.Perc, input$DemoData
        #     ) %>% 
        #     summarise(., Measure = ylab_clean(),
        #               Counties = n(),
        #               Min = min(get(input$COVIDbuttons)),
        #               Q1 = quantile(get(input$COVIDbuttons), 0.25),
        #               Median = median(get(input$COVIDbuttons)),
        #               Mean = mean(get(input$COVIDbuttons)),
        #               Q3 = quantile(get(input$COVIDbuttons), 0.75),
        #               Max = max(get(input$COVIDbuttons)),
        #               Range = max(get(input$COVIDbuttons)) -
        #                   min(get(input$COVIDbuttons)),
        #               S.Dev = sd(get(input$COVIDbuttons)),
        #               S.Error = std.error(get(input$COVIDbuttons)),
        #               Skew = round(skew(get(input$COVIDbuttons)),2))
    })
    
    
    
    #   output$table<-renderTable(NAMES_CO_COMPLETE_SUBLISTS[input$catbuttons])
    # }
    
    output$Racefacetgrid <- renderPlot({
        
        
        
        colnames(CO_Race_Measures_COVID19)
        #### Race / Ethinicity Outcomes by County ####
        Facet_race = CO_Race_Measures_COVID19 %>%
            # select(., COUNTY, Demographic, Test.Type, input$COVID,
            # input$RaceDemo) %>%
            filter(., !is.na(get(input$RaceDemo)) &
                       !is.na(get(input$COVIDbuttons)) &
                       !is.na(COUNTY) &
                       Test.Type == "Measure" 
                   # & COVID.Cases.Max >= min(input$covidrange1)
                   # & COVID.Cases.Max <= max(input$covidrange1)
                   # & Median.Household.Income >= min(input$medianrange1)
                   # & Median.Household.Income <= max(input$medianrange1)
                   # & Perc.Rural >= min(input$ruralrange1)
                   # & Perc.Rural <= max(input$ruralrange1)
            ) %>%
            ggplot(.,
                   aes(x=get(input$RaceDemo),
                       y=get(input$COVIDbuttons),
                       fill=Demographic, color=Demographic)) +
            geom_point() +
            facet_grid(. ~ Demographic, scales=input$scales) +
            stat_smooth(method=lm, color="black", alpha=0.2) +
            ggtitle("Race/Ethnicity Outcomes by County") +
            labs(subtitle = paste0(ylab_short(), " vs. ", race_xlab_long_perc())) +
            theme(text=element_text(family="calibri"),
                  plot.title =
                      element_text(face="bold", hjust=0.5, size=22),
                  plot.subtitle = element_text(hjust=0.5, color="darkblue", size=18),
                  legend.text = element_text(size=12, color="darkblue", face="bold"),
                  legend.title = element_text(color="darkblue", face="bold", size=14),
                  legend.background =
                      element_rect(fill="#F2F5F7", color="gray", size=1),
                  # 
                  # legend.background =
                  #     element_rect(fill="snow2", color="lightgray", size=1),
                  # legend.title = element_text(),
                  axis.title = element_text(face="bold", size=16),
                  axis.text = element_text(face="bold"),
                  strip.text.x = element_text(face="bold", size=14)) +
            xlab(race_xlab_perc()) +
            ylab(ylab_perc())
        Facet_race
    })
    
    output$indcounty = renderGirafe({
        # COVID19ALL_MERGE = COVID19ALL_MERGE %>% 
        #     filter(., 
        #            (COUNTY == input$single | COUNTY == "STATE AVG")
        #            & Date >= min(input$daterange)
        #            & Date <= max(input$daterange)
        #     )
        # onecounty = COVID19ALL_MERGE %>% 
        #     filter(., COUNTY == input$single) %>% 
        #     ggplot(aes(x=Date, y=get(input$COVIDbuttons))) +
        #     geom_line_interactive(
        #         aes(
        #             x=Date, y=get(input$COVIDbuttons), color=COUNTY,
        #             tooltip = paste(get(input$COVIDbuttons),Date,
        #                             sep=" ")
        #         ),
        #         size=2
        #         #x=Date, y=get(input$COVIDbuttons), color=COUNTY,
        #         #                               get(input$COVIDbuttons)
        #         # paste(paste(input$COVIDbuttons,
        #         #                     ylab_clean(), sep=" "),
        #         #               paste0("on ", Date), sep="\n")
        #         # )
        #     ) + 
        #     
        # stateaverage = COVID19ALL_MERGE %>% 
        #     filter(., COUNTY == input$single
        indcounties = COVID19ALL_MERGE %>% 
            filter(., COUNTY == input$single | 
                       COUNTY == input$state
                   # COUNTY == "COLORADO"
            ) %>% 
            ggplot(
                # aes(x=Date, y=get(input$COVIDbuttons))
                aes(x=Date, y=get(input$COVIDselect), color=COUNTY
                    #,tooltip=Date
                    # paste(paste(input$COVIDbuttons,
                    #             ylab_clean(), sep=" "),
                    #       paste0("on ", Date), sep="\n")
                )
            ) +
            geom_vline_interactive(xintercept = as.Date("2020-03-25"), aes(
                tooltip="March 25th - Shutdown Announced\n(Stay-at-Home Order)"), 
                color="#CC5500", linetype="dashed", size=1) +
            geom_vline_interactive(xintercept=as.Date("2020-04-27"),aes(
                tooltip="April 27th - Phased Reopening Begins\n(Safer-at-Home Order)"),
                color="orange",linetype="dashed", size=1) +
            geom_vline_interactive(xintercept=as.Date("2020-05-27"),aes(
                tooltip="May 27th - Phased Reopening Continues\nIn-Person Dining / Summer Camps Reopen"),
                color="yellow3",linetype="dashed", size=1) +
            geom_vline_interactive(xintercept=as.Date("2020-06-18"),aes(
                tooltip="June 18th - Bars Reopen at 50% Capacity\n(Protect-Our-Neighbors Order)"),
                color="green",linetype="dashed", size=1) +
            geom_vline_interactive(xintercept=as.Date("2020-06-30"),aes(
                tooltip="June 30th - Bars Reclosed"),
                color="yellow3",linetype="dashed", size=1) +
            geom_vline_interactive(xintercept=as.Date("2020-07-16"), aes(
                tooltip="July 16th - Governor Polis issues\nStatewide Mask Order"
                # ,onclick="location.href = https://coloradosun.com/2020/06/30/colorado-bars-close-back-down-coronavirus/"
            ),
            color="orange",linetype="dashed", size=1) +
            geom_line_interactive(size=1.5) +
            geom_point_interactive(
                aes(
                    
                    # tooltip = paste(get(input$COVIDbuttons),Date,
                    #                 sep=" ")
                    
                    #x=Date, y=get(input$COVIDbuttons), color=COUNTY,
                    #                               get(input$COVIDbuttons)
                    tooltip = paste(paste(round(get(input$COVIDselect),1),
                                          covid_lab_perc(), sep=" "),
                                    paste0("on ", format(Date, format = "%B %d"), 
                                           " for ", stri_trans_totitle(COUNTY)),
                                    sep="\n")
                    ,
                    data_id = Date),
                size=1.6
            ) + 
            
            #    geom_line_interactive(aes(x=Date, y=get(input$COVIDbuttons), color=COUNTY,
            #    tooltip=Date),size=2
            #                              #x=Date, y=get(input$COVIDbuttons), color=COUNTY,
            #    #                               get(input$COVIDbuttons)
            #    # paste(paste(input$COVIDbuttons,
            #    #                     ylab_clean(), sep=" "),
            #    #               paste0("on ", Date), sep="\n")
            #    # )
            # ) +
            # scale_color_manual_interactive(values = c("COUNTY" = c("blue","red"), 
        #                                           "Shutdown Announced" = "#CC5500")) +
        labs(title = paste0(stri_trans_totitle(countyname()), 
                            " County Timeline:\nCOVID ", covid_lab_long()),
             y=covid_lab_perc(), 
             subtitle = "including Major Events in Colorado's COVID response") +
            theme(text=element_text(family="calibri"),
                  legend.position = "bottom", 
                  legend.title = element_blank(),
                  legend.text = element_text(size=12, face="bold", color="darkblue"), 
                  plot.subtitle = element_text(hjust=0.5, color = "darkblue", 
                                               face="italic", size=12),
                  axis.text = element_text(face="bold"),
                  plot.title = element_text(hjust=0.5, face="bold", size=18),
                  axis.title = element_text(face="bold", size=14),
                  strip.text.x = element_text(face="bold"),
                  legend.background =
                      element_rect(fill="#F2F5F7", color="gray", size=1)) +
            scale_x_date(date_labels = "%B %d"
                         # ,breaks = "weekly"
            ) +
            scale_y_continuous(trans = input$yscale) +
            xlim(min(input$daterange),max(input$daterange)) 
        # ?scale_x_date()
        
        # ggiraph(code=print(indcounties))
        options(scipen=999)
        girafe(ggobj = indcounties, options = list(
            # opts_sizing(rescale = TRUE, width = .7),
            opts_hover(css = "opacity:0.8;"),
            # opts_tooltip(offx = 20, offy = -55)
            opts_selection(type = "none")
        ))
    })
    
    output$ind_data = renderDataTable({
        inddata = COVID19DATA %>% 
            # filter(., 
            #        # (COUNTY == input$single | COUNTY == input$state)
            #                   # & 
            #            Date >= min(input$daterange)
            #                   & 
            #            Date <= max(input$daterange)
            #        ) %>% 
            arrange(desc(Date), COUNTY)
        
        # inddata = formatRound(inddata, columns=7:25, digits=2)
        datatable(inddata, rownames=F,
                  filter = list(position = 'top', clear = FALSE),
                  extensions = c("Buttons", "Select"), 
                  options = list(
                      # dom ='t',
                      search = list(regex = TRUE, caseInsensitive = TRUE),
                      pageLength = 10,
                      scrollX=TRUE, 
                      fixedColumns = list(leftColumns = 1),
                      # scrollY=TRUE, 
                      # scrollCollapse=TRUE,
                      select=TRUE,
                      dom = 'Bfrtip',
                      buttons = c('copy', 'csv', 'excel'
                                  # , 'pdf', 'print'
                      )
                  )
        ) %>%
            formatRound(columns = -c(1:12), digits=2)
        
    })
    output$DaysSince <- renderValueBox({
        DaysCOVID = COVID19CountyANALYSIS %>% 
            select(COUNTY, COVID_Num_Days_Since_First_Case) %>% 
            filter(COUNTY == input$single)
        Days = DaysCOVID$COVID_Num_Days_Since_First_Case
        valueBox(
            paste0(Days, " Days"),
            paste0("since 1st case in ", stri_trans_totitle(countyname())),
            # icon = icon("calendar"),
            color = "navy"
        )
    })
    output$caseranking <- renderValueBox({
        RankCOVID = COVID19DATA %>% 
            filter(., Date == max(Date)) %>% 
            mutate(minrank = min_rank(desc(Positive.Tests.Perc))) %>% 
            filter(COUNTY == input$single)
        Rank = RankCOVID$minrank
        Pos = RankCOVID$Positive.Tests.Perc
        valueBox(
            paste0("#",Rank," (", round(Pos,1), "%)"), "of 64 in Positive Tests %",
            # icon = icon("rank"),
            color = "purple"
        )
    })
    output$lastweekcases <- renderValueBox({
        lastwk = COVID19DATA %>%
            select(COUNTY, Date, New.Cases.Last.Week) %>%
            filter(COUNTY == input$single & Date == max(Date))
        
        # lastwk = COVID19DATA %>% 
        #     select(COUNTY, Date, New.Cases.Last.Week) %>% 
        #     filter(COUNTY == input$single) %>% 
        #     arrange
        lastwkcase = lastwk$New.Cases.Last.Week
        valueBox(
            paste0(lastwkcase, " Cases"), 
            paste0("last week in ", stri_trans_totitle(countyname())), 
            # icon = icon("list-alt"),
            color = "blue"
        )
    })
    # output$lastweekdeaths <- renderValueBox({
    #     lastwk = COVID19DATA %>%
    #         select(COUNTY, Date, New.Deaths.Last.Week) %>%
    #         filter(COUNTY == input$single & Date == max(Date))
    #     
    #     # lastwk = COVID19DATA %>% 
    #     #     select(COUNTY, Date, New.Cases.Last.Week) %>% 
    #     #     filter(COUNTY == input$single) %>% 
    #     #     arrange
    #     lastwkdeath = lastwk$New.Deaths.Last.Week
    #     valueBox(
    #         paste0(lastwkdeath, " Deaths"), 
    #         paste0("last week in ", stri_trans_totitle(countyname()))
    #         , icon = icon("list"),
    #         color = "red"
    #     )
    # })
    output$diff <- renderValueBox({
        twowk = COVID19DATA %>%
            select(COUNTY, Date, New.Cases.Last.Week) %>%
            filter(COUNTY == input$single & 
                       Date == max(Date)-9)
        lastwk = COVID19DATA %>%
            select(COUNTY, Date, New.Cases.Last.Week) %>%
            filter(COUNTY == input$single & 
                       Date == max(Date)-2)
        twowkcase = twowk$New.Cases.Last.Week
        lastwkcase = lastwk$New.Cases.Last.Week
        difference = ifelse(twowkcase!=0, 
                            round(100*(lastwkcase-twowkcase)/twowkcase,0), 0)
        absdiff = abs(difference)
        valueBox(
            ifelse(difference >=0,
                   paste0(absdiff, "% More"), 
                   paste0(absdiff, "% Fewer")),
            "Cases since last week", 
            # icon = icon("bar-chart-o"),
            color = "red"
        )
    })
    output$ind_map <- renderGirafe({
        ind_map_fill = COVID19ALL_MERGE %>% 
            filter(., 
                   !is.na(get(input$COVIDselect)) 
                   # & Date >= min(input$daterange)
                   # & Date <= max(input$daterange)
                   # ) %>% 
                   # select(., COUNTY, Date, input$COVIDselect
            ) %>% 
            filter(., Date == max(Date)) %>%
            select(COUNTY, Max.Date = Date, Latest = input$COVIDselect)
        
        
        
        ind_map_hover = COVID19ALL_MERGE %>% 
            filter(., 
                   !is.na(get(input$COVIDselect)) 
                   & Date >= min(input$daterange)
                   & Date <= max(input$daterange)
                   # ) %>% 
                   # select(., COUNTY, Date, input$COVIDselect
            ) %>% 
            # mutate(Rank = IndRank) %>%
            group_by(., COUNTY) %>%
            summarise(., 
                      Median = round(median(get(input$COVIDselect)),2),
                      Mean = round(mean(get(input$COVIDselect)),2),
                      Maximum = round(max(get(input$COVIDselect)),2),
                      Min_Date_Range = min(Date),
                      Max_Date_Range = max(Date)) 
        
        CO_MAP_IND = left_join(CO_MAP_COVID_BAL, full_join(ind_map_fill, ind_map_hover,
                                                           by="COUNTY"), by="COUNTY")
        # CO_MAP_SINGLE = CO_MAP_IND %>%
        #     filter(., COUNTY == input$single)
        ind_map_out = ggplot(CO_MAP_IND,
                              aes(x=long, y=lat,
                                  group=group,
                                  fill=get(input$mapmeasure)
                                  # , color = CO_MAP_SINGLE$COUNTY
                                  )) +
            scale_fill_gradient_interactive(low="#3399FF", high="red2") +
            # scale_color_manual_interactive("yellow") +
            # scale_color_brewer() +
            # geom_polygon(CO_MAP_SINGLE, aes(x=long, y=lat, group=group, color="yellow")) +
            # fill=covid_var)) +
            #         scale_fill_manual_interactive(values=c('#e8e8e8', "#dfb0d6", "#be64ac")) +
            # scale_fill_brewer_interactive(palette = "Set1") +
            labs(title = str_wrap(paste0("COVID ",covid_lab_long()), 32), 
                 fill=str_wrap(covid_lab_perc_no_avg(),8),
                 subtitle = "Hover over any county for additional information"
            ) +
            
            #guides(fill=FALSE, x.axis=FALSE) +
            #theme(title)
            bi_theme() + coord_map("mercator") +
            xlim(-109.25, -101.75) + ylim(36.75, 41.25) +
            # xlab("Selected County in yellow. Click for County Wikipedia Page") + 
            xlab(NULL) +
            ylab(NULL) +
            # geom_polygon(aes(COUNTY == get(input$single), color = "yellow")) +
            #coord_map("polyconic" ) #+
            
            geom_polygon_interactive(aes(tooltip = 
                                             paste(paste0(COUNTY, " COUNTY  --  Population: ", formatC(Population, format="f", big.mark = ",", digits=0)),
                                                   paste0("Median Income: $", 
                                                          formatC(Median.Household.Income, format="f", big.mark = ",", digits=0), 
                                                          "  --  ",
                                                   Perc.Rural, "% Rural"),
                                                   paste0("Between ", format(Min_Date_Range, format = "%B %d"), " & ",
                                                          format(Max_Date_Range, format = "%B %d"), ":"),
                                                   paste0("Highest ", covid_lab_perc_no_avg(), ": ", Maximum),
                                                   paste0("Average ", covid_lab_perc_no_avg(), ": ", Mean),
                                                   paste0("Latest Value on ",
                                                          format(Max.Date, format = "%B %d"), ": ",
                                                          round(Latest,2)),
                                                   sep="\n"),
                                         data_id = COUNTY, onclick = WIKI)
                                     ,color="black"
                                     ) +
            theme(text=element_text(family="calibri"),
                legend.title = element_text(color="darkblue", face = "bold", size=13, hjust=0),
                legend.position = "right",
                legend.text = element_text(color="darkblue", face = "bold", size=11),
                plot.title = element_text(hjust=0.5, size=18, vjust=-1),
                plot.subtitle = element_text(hjust=0.5, color = "darkblue", 
                                             face="italic", size=12, vjust=-1),
                # axis.title.x = element_text(face="italic", size=6),
                # axis.title = element_text(face="bold", size=12),
                strip.text.x = element_text(face="bold"),
                legend.background =
                    element_rect(fill="#F2F5F7", color="gray", size=1)
                ,plot.margin=unit(c(-1.2,0,0,0),"cm")
            ) 
       
        girafe(ggobj = ind_map_out, 
               # fonts = list(sans = "Arial"),
               options = list(
            opts_sizing(rescale = TRUE, width = .7),
            opts_hover(css = "opacity:0.8;"),
            opts_tooltip(offx = 20, offy = -55),
            opts_selection(selected = input$single,
                           type = "single", only_shiny = FALSE,
                           css = "stroke:yellow;stroke-width:3.5;"
                           # type = "none"
                           )
        ))
        # ggiraph(code = print(corr_map_out))
    })
    output$ind_graph_data = renderDataTable({
        IndRankCOVID = COVID19ALL_MERGE %>%
            filter(., !is.na(get(input$COVIDselect))) %>%
            filter(., Date == max(Date) &
                       COUNTY != "COLORADO") %>%
            mutate(minrank = min_rank(desc(get(input$COVIDselect))))
        IndRankCOVID = IndRankCOVID %>%
            select(COUNTY, Rank = minrank) %>%
            filter(COUNTY == input$single)
        
        MaxDateCounty = COVID19ALL_MERGE %>%
            filter(., COUNTY == input$single
                   & Date >= min(input$daterange)
                   & Date <= max(input$daterange)) %>%
            arrange(desc(get(input$COVIDselect))) %>%
            slice(., 1) %>%
            select(., COUNTY, Max.Date = Date)
        MaxDateState = COVID19ALL_MERGE %>%
            filter(., COUNTY == "COLORADO"
                   & Date >= min(input$daterange)
                   & Date <= max(input$daterange)) %>%
            arrange(desc(get(input$COVIDselect))) %>%
            slice(., 1) %>%
            select(., COUNTY, Max.Date = Date)
        MaxDate = union_all(MaxDateCounty, MaxDateState)
        
        # IndRank = IndRankCOVID$minrank
        ind_table_graph = COVID19ALL_MERGE %>% 
            filter(., (COUNTY == input$single | COUNTY == "COLORADO") 
                   & !is.na(get(input$COVIDselect)) 
                   & Date >= min(input$daterange)
                   & Date <= max(input$daterange)
            # ) %>% 
            # select(., COUNTY, Date, input$COVIDselect
            ) %>% 
            # mutate(Rank = IndRank) %>%
            group_by(., COUNTY) %>%
            summarise(., Measure = covid_lab_long(),
                      Start.Date = min(Date),
                      End.Date = max(Date),
                      # Rank = IndRank,
                      Min = round(min(get(input$COVIDselect)),2),
                      Q1 = round(quantile(get(input$COVIDselect), 0.25),2),
                      Median = round(median(get(input$COVIDselect)),2),
                      Mean = round(mean(get(input$COVIDselect)),2),
                      Q3 = round(quantile(get(input$COVIDselect), 0.75),2),
                      Max = round(max(get(input$COVIDselect)),2),
                      # Max.Date = ifelse(input$COVIDselect == max(input$COVIDselect), Date, NA),
                      Range = round(max(get(input$COVIDselect)) -
                                        min(get(input$COVIDselect)),2),
                      S.Dev = round(sd(get(input$COVIDselect)),2),
                      S.Error = round(std.error(get(input$COVIDselect)),2),
                      Skew = round(skew(get(input$COVIDselect)),2))
        ind_table_join = full_join(IndRankCOVID, 
                                   full_join(MaxDate, ind_table_graph, 
                                            by="COUNTY"), by="COUNTY")
        ind_table_join = ind_table_join %>% select(
            COUNTY, Measure, Rank, Start.Date, Highest.Point = Max.Date, everything()
        )
       
        datatable(ind_table_join, rownames = F, 
                  #escape = F
                  # filter = list(position = 'top', clear = FALSE),
                  extensions = c("Buttons", "Select"),
                  options = list(
                      # search = list(regex = TRUE, caseInsensitive = TRUE),
                      # pageLength = 5,
                      scrollX=TRUE, 
                      fixedColumns = list(leftColumns = 1),
                      info = F,
                      paging = F,
                      # scrollY=TRUE, 
                      # scrollCollapse=TRUE,
                      select=TRUE,
                      dom = 'Brtip',
                      buttons = c('copy', 'csv', 'excel'
                                  # , 'pdf', 'print'
                      )
                  )
        )
        # options = list(dom= 't'))
    }, width="100%")
    # output$Rank <- renderValueBox({
    #     RankCOVID = COVID19CountyANALYSIS %>% 
    #         select(COUNTY, COVID_Num_Days_Since_First_Case) %>% 
    #         filter(COUNTY == input$single)
    #     valueBox(
    #         , "Progress", icon = icon("list"),
    #         color = "red"
    #     )
    # })
    # 
    # output$indmap = renderGirafe({
    #     CO_county_filter = CO_county_map %>%
    #         filter(., COUNTY == input$single)
    #     ind_map = 
    #         ggplot(CO_county_map,
    #            aes(x=long, y=lat,
    #                group=group)) +
    #         # ggtitle(stri_trans_totitle(get(input$single))) +
    #         # labs(title = paste(ylab_clean(), str_wrap(xlab_long_perc(),30), sep="\nvs. ")
    #              # ,x = "Bivariate Analysis")
    #         
    #         # theme(plot.title = element_text(hjust=1),
    #               #plot.margin=unit(c(2,2,0,0),"cm")
    #         # ) +
    #         guides(fill=FALSE, x.axis=FALSE, y.axis=FALSE) +
    #         #theme(title)
    #         theme_void() + coord_map("mercator") +
    #         xlim(-109.25, -101.75) + ylim(36.75, 41.25) +
    #         xlab(NULL) + ylab(NULL) +
    #         #coord_map("polyconic" ) #+
    #         
    #         geom_polygon_interactive(aes(tooltip =  
    #                                          paste0(stri_trans_totitle(COUNTY), " County"),
    #                                      data_id = COUNTY
    #                                  # , onclick = WIKI
    #         ),color="black")
    #     ind_map = ind_map + geom_polygon_interactive(CO_county_filter, 
    #                                                  aes(x=long, y=lat,
    #                                                      group=group, fill = "red"))
    #     # ggiraph(code = print(map2))
    #     girafe(ggobj = ind_map, options = list(
    #         opts_sizing(rescale = TRUE, width = .7),
    #         opts_hover(css = "opacity:0.8;"),
    #         opts_tooltip(offx = 20, offy = -55),
    #         opts_selection(type = "none"
    #                        , only_shiny = FALSE
    #         )
    #     ))
    # })
}

# Run the application 
shinyApp(ui = ui, server = server)
