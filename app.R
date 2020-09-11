## A Shiny App by David Gottlieb (@datatodavid) ##

#### Libraries ####
library(shiny)
library(psych)
library(dplyr)
library(ggplot2)
library(magrittr)
library(lubridate)
library(mapproj)
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
require(ggiraph)
require(shinydashboard)
require(DT)

# Newest version of tidyr uses version 0.2 of cppll, which is not yet Shiny compliant. 
# If needed, download version below (0.1) to fix for Shiny App.
# devtools::install_version("cpp11", version = "0.1", repos = "http://cran.us.r-project.org")

############################# UI ######################################
ui <- dashboardPage(
    
    dashboardHeader(title = "Colorado COVID"),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem("A Shiny App by David Gottlieb",
                     icon=NULL, href = "https://twitter.com/datatodavid"
            ),
            convertMenuItem(
                menuItem("County Dashboard", icon=icon("info-circle"), 
                                     tabName = "County_Dash", startExpanded = TRUE,
                                     
            selectizeInput(inputId = "single",
                           label = "Select a County:",
                           choices = unique(COVID19ALL_MERGE$COUNTY)[-1], 
                           selected = "DENVER"),
            
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
                           selected = "New.Cases.5.Day.Avg"),
            sliderInput("daterange", 
                        "Select a Date Range:", 
                        min = as.Date("2020-03-17"), 
                        max = as.Date(Sys.Date()), 
                        value = c(as.Date("2020-03-17"), as.Date(Sys.Date())),
                        timeFormat="%b %d"
            ),
            
            # radioButtons(inputId = "yscale", label="Choose a Scale:",
            #              choices = c("Relative (Logarithmic)" = "log2", 
            #                          "Fixed (Linear)"="identity")),
            # 
            # radioButtons(inputId = "state", label="Display State Data?",
            #              choices = c("Yes"="COLORADO",
            #                          "No" = "")),
            # 
            # radioButtons(inputId = "mapmeasure",
            #              label = "Sort Heat Map by:",
            #              choices = c("Latest Value" = "Latest",
            #                          "Highest Value in Date Range" = "Maximum",
            #                          "Average Value in Date Range" = "Mean")
            #              ), 
            br()
            ), 
            
        tabName = "County_Dash"),
            convertMenuItem(
                menuItem("COVID vs. Demographics", tabName = "Main_Dash", 
                           icon=icon("bar-chart-o"), 
                     
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
                           choices = colnames("catbuttons")[-1:-6])
            ),
            
        tabName = "Main_Dash"),
            menuItem("Sources", icon=icon("folder-open"),
                 menuSubItem("Github R Source Code", icon = icon("file-code-o"), 
                         href = "https://github.com/datatodavid/CO_COVID"),
                 menuSubItem("CDPHE CO COVID Data", icon = icon("list-alt"), 
                         href = "https://data-cdphe.opendata.arcgis.com/search?collection=Dataset&q=covid19&sort=name"),
                 menuSubItem("CDPHE Community Data", icon = icon("list-alt"), 
                         href = "https://data-cdphe.opendata.arcgis.com/search?collection=Dataset&q=cdphe%20community%20level%20estimates%20(county)%20"),
                 menuSubItem("BRFSS County Data", icon = icon("list-alt"), 
                         href = "https://data-cdphe.opendata.arcgis.com/search?collection=Dataset&q=BRFSS%202014-2017%20County%20Datasets&sort=name"),
                 menuSubItem("RWJ County Health Rankings", icon = icon("list-alt"), 
                         href = "https://www.countyhealthrankings.org/app/colorado/2020/downloads")
            ),
            menuItem("~ David Gottlieb LinkedIn ~", icon = icon("linkedin"), 
                     href = "https://www.linkedin.com/in/david-gottlieb-a351bb3b/")
            )
        ),
    dashboardBody( 
        tags$head(tags$style("*:not(.fa){ font-family: 'calibri', !important;, font-weight:500; }")),
         tabItems(
            tabItem(tabName = "County_Dash",
                tabsetPanel(
                    tabPanel("County COVID Dashboard",
                         fluidRow(        
                         valueBoxOutput("DaysSince", width=3), 
                         valueBoxOutput("lastweekcases", width=3),
                         valueBoxOutput("diff", width=3),
                         valueBoxOutput("caseranking", width=3)
                         ),
                        fluidRow(
                            box(status="primary",
                                solidHeader = T, title = "County vs. State COVID Trend Analysis",
                                girafeOutput("indcounty"),
                                "Hover over Vertical lines to read about major events in Colorado's statewide response to the pandemic.",
                            ),
                            box(title = "Heat Map of COVID in Colorado Counties",
                                status="primary",
                                solidHeader = T,
                                girafeOutput("ind_map"),
                                "Gray displays when no data is available. Hospitalization data is only published on the state level.")
                        ),
                        fluidRow(
                            box(title = "Choose a Scale:", solidHeader = T, status="warning",
                                radioButtons(inputId = "yscale", label=NULL,
                                              choices = c("Relative (Logarithmic)" = "log2", 
                                                          "Fixed (Linear)"="identity")), width=3),
                                 
                            box(title="Display State Data?", solidHeader = T, status="warning",
                                radioButtons(inputId = "state", label=NULL,
                                              choices = c("Yes"="COLORADO",
                                                          "No" = "")), width=3),
                                 
                            box(title = "Sort Heat Map by:", solidHeader = T, status="warning",
                                 selectizeInput(inputId = "mapmeasure", label=NULL,
                                              choices = c("Most Recent Value" = "Latest",
                                                          "Highest Value in Date Range" = "Maximum",
                                                          "Average Value in Date Range" = "Mean")), width=6)
                        ),
                        fluidRow(
                         box(title = "County vs. State COVID Summary Data", status="primary",
                             solidHeader = T, 
                             dataTableOutput("ind_graph_data"), width=12))
                ),
                tabPanel("Colorado COVID Dataset Explorer",
                         box(title = "Complete & Cleaned Colorado COVID Dataset by County",status="primary",
                             solidHeader = T, 
                             "Click on the boxes below the column Category names to filter results by Category.",
                             br(),
                             "Export selected data using the buttons below.",
                             dataTableOutput("ind_data"), width=12))
            )
        ),
        tabItem(tabName = "Main_Dash",
            tabsetPanel(
                tabPanel("Category Explorer", 
                         fluidRow(
                             column(width=3,
                                box(title = "Welcome to the COVID vs. Demographics Section", status = "success",
                                    width = NULL, solidHeader = T,
                                    "This interactive Shiny website allows the examination of the relationships among over 100 Demographic Measures against COVID Outcomes in Colorado. This is an exploratory tool designed for policy makers, epidemiologists, statisticians, and anyone interested in exploring. Almost all of the entire site is interactive and can be exported (check the top right corners of the visualizations).", br(),"Let me know what you discover at twitter.com/datatodavid"
                                    ),
                                br(),
                                box(title = "Select a COVID Measure and Demographic Category List from the left sidebar to begin",
                                    status="warning",
                                    width = NULL, solidHeader = T,
                                    "Since there are so many Measures to explore, they are grouped into Category Lists. To see which category may interest you, choose a COVID measure and Demographic Category List on the left sidebar and see which Measures show the strongest relationships. Then, you can click on the tabs above to explore how these factors break down by County."
                                )
                         ),
                         column(width=6, 
                                box(status="success",solidHeader=T, title="Use this tool to explore Demographic Categories & Measures",
                                    ggiraphOutput("corr_explorer"),width = NULL)
                         ),
                         column(width=3,
                                box(status="info",width=NULL, solidHeader = T, 
                                    title = "A Positive Correlation Relationship means:", 
                                    "The higher the Demographic Measure in a county,",br(), "the higher the selected COVID Measure.",
                                    br(), br(), "A value greater than 0.2 suggests that this relationship is statistically significant based on the 64 Colorado counties."),
                                box(status="danger", width=NULL,solidHeader = T, 
                                    title = "A Negative Correlation Relationship means:", 
                                    "The lower the Demographic Measure in a county,",br(), "the higher the selected COVID Measure.",
                                    br(), br(), "A value less than -0.2 suggests that this relationship is statistically significant based on the 64 Colorado counties.")
                         ),
                         
                         box(title = "Selected Colorado Demographic Data by County", status="primary",
                             solidHeader = T, "Export selected data using the buttons below. Click on the boxes below the Column names to filter results by column.",
                             dataTableOutput("corr_data"), width=12)
                         ) 
               ),
               tabPanel("Filter Group Explorer", 
                         fluidRow(
                             box(status="primary",
                                 "This page is meant to explore Colorado Counties by important groupings.", br(),
                                 "Select a COVID Measure and Demographic Measure on the left sidebar to update the Table and Scatterplot.", br(),
                                 "Then, choose a Group in the orange box to the right.", br(),
                                 "Hover over any bars, points, or states on this page to see more information.",
                                 br(), br(),
                                 ggiraphOutput("bi_cor"),
                                 solidHeader = T, title="Breakdown of Colorado Counties by Group",
                                 width=4, height=620),
                             box(title ="Select a Filter Measure for the Scatterplot & Map",status="warning",solidHeader = T,
                                 selectizeInput(inputId = "balancebuttons",
                                              label = NULL,
                                              choices = c("COVID Groups" = "COVID.Groups",
                                                          "Income Groups" = "Income.Groups",
                                                          "Rural Groups" = "Rural.Groups"),
                                              selected = "Rural.Groups"), width=8, height = 100),
                             box(status="primary",
                                 ggiraphOutput("scatterplot"),
                                 solidHeader = T, title = "County Scatterplot with Linear Regression", 
                                 width=4, height=500),
                             box(status="primary",
                                 "Hover over any County to find out detailed group information.", br(),
                                 "Legend is the same as the Scatterplot to the left.", 
                                 girafeOutput("corr_map"),
                                 solidHeader = T, title = "Colorado County Map by Group",
                                 width=4, height = 500),
                             box(width=12, status="success", title="How were these groups calculated?", solidHeader = T,
                                 "The 64 Colorado Counties were split into groups using the following methods:", br(),
                                 ">>> COVID Caseload -- 3 (Almost) Even Groups: 22 Low, 21 Med, 21 High", br(),
                                 ">>> Median Income -- 3 (Almost) Even Groups: 22 Low, 21 Med, 21 High", br(),
                                 ">>> Rural Percentage -- 4 Groups by Census Rural Breakdowns: 15 Urban/Suburban (0-24%), 15 Somewhat Rural (25-49%), 10 Mostly Rural (50-99%), 24 100% Rural"
                                 )
                             ) 
                        ),
                tabPanel("Statewide Data", 
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
                                         0, denver_cases, value=c(20,denver_cases), step=20),
                             sliderInput("medianrange", 
                                         "Median Income Range of County:", 
                                         min_income, max_income, value=c(min_income,max_income), pre="$",
                                         step=2000),
                             sliderInput("ruralrange", 
                                         "Rural Population % of County:", 
                                         0, 100, value=c(0,100), step=1, post="%")
                            )
                        ),
                        column(width=7,box(status="primary",width = NULL,solidHeader = T, 
                                           title="County by County Demographic Rankings Bar Chart",
                            ggiraphOutput("top20")))),
                         box(status="primary",
                             title = "Summary Statistics", solidHeader = T,
                             dataTableOutput("combined_data"),
                             width=12
                         )
                     ) 
                ,tabPanel("Colorado Maps", 
                      fluidRow(
                          box(status="primary",
                              "Hover over any County to see its data. Click to visit its Wikipedia page.",
                              ggiraphOutput("covid_map"), 
                              solidHeader = T, title="COVID Measure Severity Map",
                              width=6),
                          box(status="primary",
                              "Hover over any County to see its data. Click to visit its Wikipedia page.",
                              ggiraphOutput("demo_map"), 
                              solidHeader = T, title="Demographic Measure Severity Map",
                              width=6),
                          box(status="success",
                              solidHeader=T, title = "Overlap of COVID Measure & Demographic Measure Severity Maps",
                              "Color grid created by overlapping two graphs above - see legend on right.",br(),
                              "Hover over any County to see its data. Click to visit its Wikipedia page.", 
                              girafeOutput("bi_map"),
                              width=8),
                          box(status="warning",
                              solidHeader = T, title = "Overlap Map Color Legend with number of Counties in each group",
                              "This legend shows the distribution of Colorado's 64 counties into each of the 9 Low/Med/High combinations.", br(),
                              "The more Purple the color is, the higher the COVID measure.", br(),
                              "The more Teal the color is, the higher the Demographic measure.",
                              plotOutput("bi_map_vlegend"),
                              width=4)
                          ) 
                ),
                tabPanel("Race/Ethnicity Comparisons", 
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
                              width=12),
                          box(status="primary",title = "Race/Ethnicity Summary Statistics",
                              solidHeader = T,
                              dataTableOutput("Race_Stats"),
                              width=12)
                          )
                )
            )))
))

####################### SERVER ################################
server <- function(input, output, session){

    ############ SETUP -- REACTIVE FUNCTIONS ############
    
    # Nesting Demographic Measures to react to Demographic Category List selection #
    observe({
        Demochoices = get(input$catbuttons)
        updateSelectizeInput(session, "DemoData", 
                             choices = no_periods(colnames(Demochoices)[-1:-6]),
                             selected = no_periods(colnames(Demochoices)[7]))
    }) 
    # Filter reactive inputs / ranges #
    output$value <- renderText({ input$state })
    covidrange = reactive({seq(min(x), max(x), 
                               length.out = input$COVID.Cases.Max)})
    medianrange = reactive({seq(min(x), max(x), 
                                length.out = input$Median.Household.Income)})
    ruralrange = reactive({seq(min(x), max(x), 
                               length.out = input$Perc.Rural)})
    
    # Functions for text / title generation #
    xlab_clean = reactive({no_periods(input$DemoData)})
    xlab_perc = reactive({no_perc(no_periods(input$DemoData))})
    xlab_short = reactive({word(no_periods(input$DemoData), 1, 2, sep=" ")})
    xlab_long_perc = reactive({long_perc(no_periods(input$DemoData))})
    
    ylab_clean = reactive({per_parentheses(no_periods(input$COVIDbuttons))})
    ylab_perc = reactive({per_parentheses(no_perc(no_periods(input$COVIDbuttons)))})
    ylab_perc_flip = reactive({per_parentheses(rev_perc_pos_co(rev_perc_mort_co(no_periods(input$COVIDbuttons))))})
    ylab_short = reactive({word(no_periods(input$COVIDbuttons), 1,2, sep=" ")})
    ylab_frontspace = reactive({per_parentheses(rev_perc_pos(rev_perc_mort(frontspace(no_periods(input$COVIDbuttons)))))})
    ylab_long_perc = reactive({per_parentheses(long_perc(no_periods(input$COVIDbuttons)))})
    
    countyname = reactive({input$single})
    
    bal_lab_clean = reactive({no_periods(input$balancebuttons)})
    
    catnames = reactive({no_periods(colnames(input$catbuttons))})
    catbuttons_clean = reactive({upper_Co(stri_trans_totitle(no_underscore(input$catbuttons)))})
    catbuttons_long = reactive({no_Co(stri_trans_totitle(no_underscore(input$catbuttons)))})
    
    race_xlab_perc = reactive({no_perc(no_periods(input$RaceDemo))})
    race_xlab_long_perc = reactive({long_perc(no_periods(input$RaceDemo))})
    
    covid_lab_perc = reactive({no_perc(per_parentheses(avg_parentheses(no_periods(input$COVIDselect))))})
    covid_lab_rev_perc = reactive({no_perc(per_parentheses(avg_parentheses(rev_perc_mort(rev_perc_pos(no_periods(input$COVIDselect))))))})
    covid_lab_perc_no_avg = reactive({long_avg(per_parentheses(avg_parentheses(no_perc(no_periods(input$COVIDselect)))))})
    covid_lab_long = reactive({long_avg(per_parentheses(avg_parentheses(long_perc(no_periods(input$COVIDselect)))))})
    
    mapmeasurelab = reactive({long_map_latest(long_map_avg(long_map_max(input$mapmeasure)))})
#################################  PAGES  ################################# 
    
################### County Dashboard ###################
    
    ######## County COVID Dashboard ########
    
    # ValueBox #1 (Days in County with COVID) #
    output$DaysSince <- renderValueBox({
        DaysCOVID = COVID19CountyANALYSIS %>% 
            select(COUNTY, COVID_Num_Days_Since_First_Case) %>% 
            filter(COUNTY == input$single)
        
        Days = DaysCOVID$COVID_Num_Days_Since_First_Case
        
        valueBox(
            paste0(Days, " Days"),
            paste0("since 1st case in ", stri_trans_totitle(countyname())),
            color = "navy"
        )
    })
    
    
    # ValueBox #2 (COVID Cases Last Week in County) #
    output$lastweekcases <- renderValueBox({
        lastwk = COVID19DATA %>%
            select(COUNTY, Date, New.Cases.Last.Week) %>%
            filter(COUNTY == input$single & Date == max(Date))
        
        lastwkcase = lastwk$New.Cases.Last.Week
        valueBox(
            paste0(lastwkcase, " Cases"), 
            paste0("last week in ", stri_trans_totitle(countyname())), 
            color = "blue"
        )
    })
    
    # ValueBox #3 (COVID Cases Difference since Last Week by County) #
    output$diff <- renderValueBox({
        twowk = COVID19DATA %>%
            select(COUNTY, Date, New.Cases.Last.Week) %>%
            filter(COUNTY == input$single & 
                       Date == max(Date)-7)
        
        lastwk = COVID19DATA %>%
            select(COUNTY, Date, New.Cases.Last.Week) %>%
            filter(COUNTY == input$single & 
                       Date == max(Date))
        
        twowkcase = twowk$New.Cases.Last.Week
        lastwkcase = lastwk$New.Cases.Last.Week
        difference = ifelse(twowkcase!=0, 
                            round(100*(lastwkcase-twowkcase)/twowkcase,0), 0)
        absdiff = abs(difference)
        
        valueBox(
            ifelse(difference >=0,
                   paste0(absdiff, "% More"), 
                   paste0(absdiff, "% Fewer")),
            paste0("cases this week in ", stri_trans_totitle(countyname())), 
            color = "red"
        )
    })
    
    # ValueBox #4 (County Positive Test Percentage and Rank) #
    output$caseranking <- renderValueBox({
        RankCOVID = COVID19DATA %>% 
            filter(., Date == max(Date)) %>% 
            mutate(minrank = min_rank(desc(Positive.Tests.Perc))) %>% 
            filter(COUNTY == input$single)
        
        Rank = RankCOVID$minrank
        Pos = RankCOVID$Positive.Tests.Perc
        
        valueBox(
            paste0("#",Rank," (", round(Pos,1), "%)"), "of 64 in CO Positive Tests %",
            color = "purple"
        )
    })
    
    # County vs. State COVID Trend Analysis (Time Series) #
    output$indcounty <- renderGirafe({
        ind_dates = COVID19ALL_MERGE %>% 
            filter(., (COUNTY == input$single | COUNTY == input$state) 
                   & !is.na(get(input$COVIDselect)) 
                   & Date >= min(input$daterange)
                   & Date <= max(input$daterange)
                   ) %>% 
            summarise(., 
                      Min_Date_Range = min(Date),
                      Max_Date_Range = max(Date)
                      ) 
        Min_Date = ind_dates$Min_Date_Range
        Max_Date = ind_dates$Max_Date_Range
        
        indcounties = COVID19ALL_MERGE %>% 
            filter(., COUNTY == input$single | COUNTY == input$state) %>% 
            ggplot(
                aes(x=Date, y=get(input$COVIDselect), color=COUNTY)
            ) +
            geom_vline_interactive(xintercept = as.Date("2020-03-25"), aes(
                tooltip="March 25th - Shutdown Announced\n(Stay-at-Home Order)"), 
                color="#CC5500", linetype="longdash", size=1) +
            geom_vline_interactive(xintercept=as.Date("2020-04-27"),aes(
                tooltip="April 27th - Phased Reopening Begins\n(Safer-at-Home Order)"),
                color="orange",linetype="longdash", size=1) +
            geom_vline_interactive(xintercept=as.Date("2020-05-27"),aes(
                tooltip="May 27th - Phased Reopening Continues\nIn-Person Dining / Summer Camps Reopen"),
                color="yellow3",linetype="longdash", size=1) +
            geom_vline_interactive(xintercept=as.Date("2020-06-18"),aes(
                tooltip="June 18th - Bars Reopen at 50% Capacity\n(Protect-Our-Neighbors Order)"),
                color="green",linetype="longdash", size=1) +
            geom_vline_interactive(xintercept=as.Date("2020-06-30"),aes(
                tooltip="June 30th - Bars Reclosed"),
                color="yellow3",linetype="longdash", size=1) +
            geom_vline_interactive(xintercept=as.Date("2020-07-16"), aes(
                tooltip="July 16th - Governor Polis issues\nStatewide Mask Order"),
                color="orange",linetype="longdash", size=1) +
            geom_vline_interactive(xintercept=as.Date("2020-08-17"),aes(
                tooltip="August 17th - School Year Starts for most of CO\n(Mix of In Person / Hybrid / Online Learning)"),
                color="green",linetype="longdash", size=1) +
            geom_line_interactive(size=1.5) +
            geom_point_interactive(
                aes(tooltip = paste(paste(round(get(input$COVIDselect),1),
                                          covid_lab_rev_perc(), sep=" "),
                                    paste0("on ", format(Date, format = "%B %d"), 
                                           " for ", stri_trans_totitle(COUNTY)),
                                    sep="\n"),
                    data_id = Date),
                size=1.6
            ) + 
            labs(title = paste0(stri_trans_totitle(countyname()), 
                                " County Timeline:\nCOVID ", covid_lab_long()),
                 x=paste0("Date Range\n(", format(Min_Date, format="%B %d")," - ", format(Max_Date,format="%B %d"), ")"),
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
            scale_y_continuous(trans = input$yscale)  
        
        indcounties = indcounties +
            # xlim(min(input$daterange),max(input$daterange)) + 
            scale_x_date(date_labels = "%b %d", date_minor_breaks = "1 day", 
                         limits = c(min(input$daterange),max(input$daterange)))
        
        options(scipen=999)
        girafe(ggobj = indcounties, options = list(
            opts_hover(css = "opacity:0.8;"),
            opts_selection(type = "none")
        ))
    })
    
    # Heat Map of COVID in Colorado Counties #
    output$ind_map <- renderGirafe({
        ind_map_fill = COVID19ALL_MERGE %>% 
            filter(., 
                   !is.na(get(input$COVIDselect))
            ) %>% 
            filter(., Date == max(Date)) %>%
            select(COUNTY, Max.Date = Date, Latest = input$COVIDselect)
        
        Max_Date = ind_map_fill$Max.Date[1]
            
        ind_map_hover = COVID19ALL_MERGE %>% 
            filter(., 
                   !is.na(get(input$COVIDselect)) 
                   & Date >= min(input$daterange)
                   & Date <= max(input$daterange)
            ) %>% 
            group_by(., COUNTY) %>%
            summarise(., 
                      Median = round(median(get(input$COVIDselect)),2),
                      Mean = round(mean(get(input$COVIDselect)),2),
                      Maximum = round(max(get(input$COVIDselect)),2),
                      Min_Date_Range = min(Date),
                      Max_Date_Range = max(Date))
        
        ind_map_assign = ind_map_hover %>% 
            summarise(.,
                      Min_Min_Date = min(Min_Date_Range),
                      Max_Max_Date = max(Max_Date_Range)) 
        
        Min_Date_State = ind_map_assign$Min_Min_Date[1]
        Max_Date_State = ind_map_assign$Max_Max_Date[1]

        CO_MAP_IND = left_join(CO_MAP_COVID_BAL, full_join(ind_map_fill, ind_map_hover,
                                                           by="COUNTY"), by="COUNTY")
        
        ind_map_out = ggplot(CO_MAP_IND,
                             aes(x=long, y=lat,
                                 group=group,
                                 fill=get(input$mapmeasure)
                             )) +
            scale_fill_gradient_interactive(low="#3399FF", high="red2") +
            labs(title = str_wrap(paste0("COVID ",covid_lab_long()), 32), 
                 subtitle = paste0("Sorted by ", mapmeasurelab()),
                 fill=str_wrap(covid_lab_perc_no_avg(),8),
                 # caption = "Hover over any county for additional information",
                 caption = paste0("Most Recent Value: ",
                                  format(Max_Date, format = "%B %d"),  ". Date Range: ",
                                  format(Min_Date_State, format = "%B %d"), " - ",
                                  format(Max_Date_State, format = "%B %d"), ".")
                 
            ) +
            bi_theme() + 
            coord_map("mercator") +
            xlim(-109.25, -101.75) + ylim(36.75, 41.25) +
            xlab(NULL) + ylab(NULL) +
            geom_polygon_interactive(aes(tooltip = 
                                             paste(paste0(COUNTY, " COUNTY  --  Population: ", formatC(Population, format="f", big.mark = ",", digits=0)),
                                                   paste0("Median Income: $", 
                                                          formatC(Median.Household.Income, format="f", big.mark = ",", digits=0), 
                                                          "  --  ",
                                                          Perc.Rural, "% Rural"),
                                                   paste0("Most Recent Value on ",
                                                          format(Max.Date, format = "%B %d"), ": ",
                                                          round(Latest,2)),
                                                   paste0("Between ", format(Min_Date_Range, format = "%B %d"), " & ",
                                                          format(Max_Date_Range, format = "%B %d"), ":"),
                                                   paste0("Highest ", covid_lab_perc_no_avg(), ": ", Maximum),
                                                   paste0("Average ", covid_lab_perc_no_avg(), ": ", Mean),
                                                   sep="\n"),
                                         data_id = COUNTY
                                         # , onclick = WIKI
                                         )
                                     ,color="black"
            ) +
            theme(text=element_text(family="calibri"),
                  plot.title = element_text(hjust=0.5, size=18, vjust=-1),
                  plot.caption = element_text(hjust=0.5, color = "darkblue", 
                                               face="bold", size=11, vjust=-1),
                  plot.subtitle = element_text(face="italic", size=15, color="darkblue", hjust=0.5, margin=margin(5,0,-5,0)),
                  strip.text.x = element_text(face="botld"),
                  legend.title = element_text(color="darkblue", face = "bold", size=13, hjust=0),
                  legend.position = "right",
                  legend.text = element_text(color="darkblue", face = "bold", size=11),
                  legend.background =
                      element_rect(fill="#F2F5F7", color="gray", size=1)
                  ,plot.margin=unit(c(-1.2,0,0,0),"cm")
            ) 
        
        girafe(ggobj = ind_map_out, 
               options = list(
                   opts_sizing(rescale = TRUE, width = .7),
                   opts_hover(css = "opacity:0.8;"),
                   opts_tooltip(offx = 20, offy = -55),
                   opts_selection(selected = input$single,
                                  type = "single", only_shiny = FALSE,
                                  css = "stroke:yellow;stroke-width:3.5;"
                   )
               )
        )
    })
    
    # County vs. State COVID Summary Data #
    output$ind_graph_data <- renderDataTable({
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
        
        ind_table_graph = COVID19ALL_MERGE %>% 
            filter(., (COUNTY == input$single | COUNTY == "COLORADO") 
                   & !is.na(get(input$COVIDselect)) 
                   & Date >= min(input$daterange)
                   & Date <= max(input$daterange)
            ) %>% 
            group_by(., COUNTY) %>%
            summarise(., Measure = covid_lab_long(),
                      Start.Date = min(Date),
                      End.Date = max(Date),
                      Min = round(min(get(input$COVIDselect)),2),
                      Q1 = round(quantile(get(input$COVIDselect), 0.25),2),
                      Median = round(median(get(input$COVIDselect)),2),
                      Mean = round(mean(get(input$COVIDselect)),2),
                      Q3 = round(quantile(get(input$COVIDselect), 0.75),2),
                      Max = round(max(get(input$COVIDselect)),2),
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
                  extensions = c("Buttons", "Select"),
                  options = list(
                      scrollX=TRUE, 
                      fixedColumns = list(leftColumns = 1),
                      info = F,
                      paging = F,
                      select=TRUE,
                      dom = 'Brtip',
                      buttons = c('copy', 'csv', 'excel'
                      )
                  )
        )
    }, width="100%")
    
    ######## Colorado COVID Dataset Explorer ########
    
    # Complete & Cleaned Colorado COVID Dataset by County #
    output$ind_data <- renderDataTable({
        inddata = COVID19DATA %>% 
            arrange(desc(Date), COUNTY)
        
        datatable(inddata, rownames=F,
                  filter = list(position = 'top', clear = FALSE),
                  extensions = c("Buttons", "Select"), 
                  options = list(
                      search = list(regex = TRUE, caseInsensitive = TRUE),
                      pageLength = 10,
                      scrollX=TRUE, 
                      fixedColumns = list(leftColumns = 1),
                      select=TRUE,
                      dom = 'Bfrtip',
                      buttons = c('copy', 'csv', 'excel')
                  )
        ) %>%
            formatRound(columns = -c(1:12), digits=2)
    })
    
    
    # Main Correlation Explorer Horizontal Bar Chart
    output$corr_explorer <- renderGirafe({
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
                 ) +
            theme(
                text=element_text(family="calibri"),
                plot.title = element_text(face="bold", hjust=0.5),
                axis.title = element_text(face="bold", size=12),
                axis.text = element_text(face="bold"),
                strip.text.x = element_text(face="bold"),
                legend.title = element_text(color="darkblue", face="bold"),
                legend.text = element_text(color="darkblue"),
                legend.background = element_rect(fill="#F2F5F7", color="gray", size=1)
                ) +
            xlab(paste0(catbuttons_clean(), " Measures")) +
            ylab(paste0("Correlation range [-1,1] by ", ylab_short())) + 
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
    
################### COVID vs. Demographics ###################
    
    ######## Category Explorer ########
    
    #### Selected Colorado Demographic Data by County ####
    output$corr_data <- renderDataTable({
        corrdata = get(input$catbuttons) %>% 
            select(-starts_with("COVID"), everything(), starts_with("COVID")) %>% 
            arrange(COUNTY)
        
        corrdata[-1:-7]=round(corrdata[-1:-7], 2)
        
        datatable(corrdata, rownames=F,
                  filter = list(position = 'top', clear = FALSE),
                  extensions = 
                      c("Buttons", "Select"),
                  options = list(
                      search = list(regex = TRUE, caseInsensitive = TRUE),
                      pageLength = 5,
                      fixedColumns = list(leftColumns = 1),
                      scrollX=TRUE,  
                      scrollCollapse=TRUE,
                      select=TRUE,
                      dom = 'Bfrtip',
                      buttons = c('copy', 'csv', 'excel'
                      )
                  )
        ) 
    })
    
    ######## Filter Group Explorer ########
    
    
    #### Breakdown of Colorado Counties by Group ####
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
            xlab("County Category") +
            ylab("Percent of Counties with Correlation") +
            scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
            labs(title="Counties with Influencing Factors:\nStrong Positive or Negative Correlation", 
                 subtitle = paste0(ylab_short(), " vs. ", xlab_long_perc())
            ) +
            theme(text=element_text(family="calibri"),
                  panel.grid.major.y = element_blank(),
                  plot.title = element_text(hjust=0.5, face="bold", size=18),
                  plot.subtitle = element_text(hjust=0.5, color="darkblue", size=14),
                  axis.title = element_text(face="bold", size=12),
                  axis.text.x = element_text(face="bold"),
                  axis.text.y = element_text(face="bold"),
                  legend.title = element_text(color="darkblue", face="bold", size=13, hjust=0),
                  legend.text = element_text(face="bold", color="darkblue", size=11),
                  legend.background =
                      element_rect(fill="#F2F5F7", color="gray", size=1),
                  strip.text.x = element_text(face="bold")) +
            geom_col_interactive(stat="identity", 
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
        
        girafe(ggobj = maptable2disp, options = list(
            opts_hover(css = "opacity:0.8;"),
            opts_tooltip(offx = 20, offy = -55),
            opts_selection(type = "none"
                           , only_shiny = FALSE
            )
        ))
    })
    
    #### County Scatterplot with Linear Regression ####
    output$scatterplot <- renderGirafe({
        point2disp = CO_MAP_COVID_BAL %>%
            filter(., !is.na(get(yes_periods(input$DemoData))) &
                       !is.na(COUNTY) 
            ) %>% 
            ggplot(aes(x=get(yes_periods(input$DemoData)), 
                       y=get(input$COVIDbuttons)
            )
            ) + 
            geom_point_interactive(stat="identity", 
                                   size=2.5,
                                   aes(color = get(input$balancebuttons),
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
                 color=bal_lab_clean(),
                 caption = "Linear Regression Model"
            ) +
            guides(colour = guide_legend(override.aes = list(size = 10))) +
            theme(text=element_text(family="calibri"),
                  plot.title = element_text(face="bold", size=18),
                  plot.subtitle = element_text(hjust=0.5, color="darkblue", size=14),
                  plot.caption = element_text(face="italic"),
                  axis.title = element_text(face="bold", size=12),
                  axis.text.x = element_text(face="bold"),
                  axis.text.y = element_text(face="bold"),
                  legend.title = element_text(color="darkblue", face="bold", size=15, hjust=0.5),
                  legend.text = element_text(color="darkblue", face="bold", size=11, hjust=0),
                  legend.background =
                      element_rect(fill="#F2F5F7", color="gray", size=1),
                  strip.text.x = element_text(face="bold")) 
        
        girafe(ggobj = point2disp, options = list(
            opts_hover(css = "opacity:0.8;"),
            opts_selection(type = "none")
        ))
    })
    
    #### Colorado County Map by Group ####
    output$corr_map <- renderGirafe({
        corr_map_out = ggplot(CO_MAP_COVID_BAL,
                              aes(x=long, y=lat,
                                  group=group,
                                  fill=get(input$balancebuttons))) +
            scale_fill_brewer_interactive(palette = "Set1") +
            labs(title = paste0("Map of Counties by ",bal_lab_clean())
               # ,
               # fill=bal_lab_clean()
            ) +
            bi_theme() + 
            coord_map("mercator") +
            xlim(-109.25, -101.75) + ylim(36.75, 41.25) +
            xlab(NULL) + ylab(NULL) +
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
                plot.title = element_text(hjust=0.5, size=17),
                legend.position = "none"
                # legend.title = element_text(color="darkblue", face = "bold", size=13),
                # legend.position = "right",
                # legend.text = element_text(color="darkblue", face = "bold", size=11),
                # legend.background =
                #     element_rect(fill="#F2F5F7", color="gray", size=1),
                # plot.margin=unit(c(-2.2,0,0,0),"cm")
            ) 
        
        girafe(ggobj = corr_map_out, options = list(
            opts_sizing(rescale = TRUE, width = .7),
            opts_hover(css = "opacity:0.8;"),
            opts_tooltip(offx = 20, offy = -55),
            opts_selection(type = "none")
        ))
    })
    
    
    
    
    ######## Statewide Data ########
    
    #### County by County Demographic Rankings Bar Chart ####
    output$top20 <- renderGirafe({
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
            ) 
        CorrelationTable = topcounties %>% 
            select(., COUNTY, COVID.Tests.Per.100000, 
                   COVID.Cases.Per.100000, COVID.Deaths.Per.100000, 
                   COVID.Positive.Tests.Perc, COVID.Mortality.Perc, 
                   yes_periods(input$DemoData)
            ) %>% 
            summarise(., Counties = n(), 
                         Correlation = round(cor(get(input$COVIDbuttons),get(yes_periods(input$DemoData))),2)
                     )
        Correlation = CorrelationTable$Correlation
        Counties = CorrelationTable$Counties
        Significance = ifelse(Correlation>=0.2, "(Significant Positive Correlation)",
                                                    ifelse((Correlation<0.2 & Correlation>=0.1), "(Weak Positive Correlation)",
                                                           ifelse((Correlation<0.1 & Correlation>-0.1), "(No Meaningful Correlation)",
                                                                 ifelse((Correlation>-0.2 & Correlation<=-0.1), "(Weak Negative Correlation)",
                                                                         ifelse(Correlation<=-0.2, "(Significant Negative Correlation)",
                                                                            "(No Correlation Available)")))))
        
        
        topcountiesgg = ggplot(topcounties, aes(x=get(yes_periods(input$DemoData)),
                          y=reorder(COUNTY, get(yes_periods(input$DemoData))), 
                          fill=get(input$COVIDbuttons))) +
            geom_col() + 
            ggtitle(str_wrap(xlab_long_perc(),35)) + 
            scale_fill_gradient(low="#3399FF", high="red2") +
            labs(x=xlab_perc(),
                 y="Colorado Counties", 
                 fill = str_wrap(ylab_perc(), 6.5),
                 subtitle = paste0("Color Shading by ", ylab_short()),
                 caption = paste0("Correlation = ", Correlation, 
                                  " ", Significance,
                                  " for the ", Counties," Colorado Counties with\n",
                                  formatC(min(input$covidrange), format="f", big.mark = ",", digits=0), "-", 
                                  formatC(max(input$covidrange), format="f", big.mark = ",", digits=0), " COVID Cases, $",
                                  formatC(min(input$medianrange), format="f", big.mark = ",", digits=0), "-$", 
                                  formatC(max(input$medianrange), format="f", big.mark = ",", digits=0), " Median Income, ", " and ",
                                  min(input$ruralrange), "-", max(input$ruralrange), "% Rural Population.")
            ) +
            theme(text=element_text(family="calibri"),
                  plot.title = element_text(hjust=0.5, face="bold", size=18),
                  plot.subtitle = element_text(hjust=0.5, face="bold", color="darkblue",
                                               size=14),
                  plot.caption = element_text(face="bold", color="darkblue", hjust=0.5),
                  strip.text.y = element_text(face="bold"),
                  axis.title = element_text(face="bold", size=14),
                  axis.text.y =
                      element_text(face="bold", size=7),
                  axis.text.x = element_text(face="bold"),
                  legend.title = element_text(color="darkblue", face="bold"),
                  legend.text = element_text(color="darkblue"),
                  legend.background =
                      element_rect(fill="#F2F5F7", color="gray", size=1),
                  panel.grid.major.y = element_blank()
                  ) +
            geom_col_interactive(stat="identity", 
                                 aes(tooltip = paste(paste0(COUNTY, " COUNTY"),
                                                     (paste(round(get(yes_periods(input$DemoData)),1), xlab_perc(), sep=" ")),
                                                     (paste(round(get(input$COVIDbuttons),1), ylab_perc_flip(), sep=" ")),
                                                     sep="\n"),
                                     data_id = COUNTY))
        
        
        girafe(ggobj = topcountiesgg, options = list(
            opts_hover(css = "opacity:0.8;"),
            opts_selection(type = "none")
        ))
    })
    
    #### Summary Statistics ####
    output$combined_data <- renderDataTable({
        demo_datasheet = CO_COUNTY_COVID_FILTER %>% 
            filter(.,  !is.na(get(yes_periods(input$DemoData))) &
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
                   COVID.Positive.Tests.Perc, COVID.Mortality.Perc, 
                   yes_periods(input$DemoData)
            ) %>% 
            summarise(., Measure = xlab_long_perc(),
                      Counties = n(),
                      Correlation = round(cor(get(input$COVIDbuttons),get(yes_periods(input$DemoData))),2),
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
            filter(.,  !is.na(get(input$COVIDbuttons)) &
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
                   COVID.Positive.Tests.Perc, COVID.Mortality.Perc, 
                   yes_periods(input$DemoData)
            ) %>% 
            summarise(., Measure = ylab_clean(),
                      Counties = n(),
                      Correlation = round(cor(get(input$COVIDbuttons),get(yes_periods(input$DemoData))),2),
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
                  extensions = c("Buttons", "Select"),
                  options = list(
                      pageLength = 5,
                      scrollX=TRUE, 
                      fixedColumns = list(leftColumns = 1),
                      info=F,
                      paging=F,
                      select=TRUE,
                      dom = 'Brtip',
                      buttons = c('copy', 'csv', 'excel'
                      )
                  )
        )
    }, width="100%")
    
    
    ######## Colorado Maps #########
    
    #### COVID Measure Severity Map ####
    output$covid_map <- renderGirafe({
        CO_MAP_COVID_COVID = CO_MAP_COVID %>%
            mutate(., covid_var = ntile(get(input$COVIDbuttons), 3)) 
        CO_MAP_COVID_COVID$covid_var %<>%
            gsub(1, "(Low)", .) %>%
            gsub(2, "(Med)", .) %>%
            gsub(3, "(High)", .)
        
        cov_map = ggplot(CO_MAP_COVID_COVID,
                         aes(x=long, y=lat,
                             group=group,
                             fill=get(input$COVIDbuttons))) +
            scale_fill_gradient_interactive(low='#e8e8e8', high= "#be64ac") +
            labs(subtitle = str_wrap(ylab_long_perc(),28),
                 fill = str_wrap(ylab_perc(), 10)
                 ) +
            theme(
                text=element_text(family="calibri"),
                plot.subtitle = element_text(hjust=1)
                ) +
            guides(x.axis=FALSE) +
            bi_theme() + 
            coord_map("mercator") +
            xlim(-109.25, -101.75) + ylim(36.75, 41.25) +
            xlab(NULL) + ylab(NULL) +
            geom_polygon_interactive(aes(tooltip = 
                                             paste(paste0(COUNTY, " COUNTY"),
                                                   paste(formatC(get(input$COVIDbuttons), format="f", big.mark = ",", digits=1), ylab_perc_flip(), covid_var, sep=" "), 
                                                   sep="\n"),
                                         data_id = COUNTY, onclick = WIKI),color="black")
        
        cov_map = cov_map +
            guides(shape = guide_legend(override.aes = list(size = 1.5)),
                   color = guide_legend(override.aes = list(size = 1.5))
                   ) +
            theme(legend.title = element_text(size = 13, face="bold"),
                  legend.text = element_text(size = 11),
                  text=element_text(family="calibri"))
        
        girafe(ggobj = cov_map, options = list(
            opts_hover(css = "opacity:0.8;"),
            opts_tooltip(offx = 20, offy = -55),
            opts_selection(type = "none")
        ))
    })
    
    #### Demographic Measure Severity Map ####
    output$demo_map <- renderGirafe({
        CO_MAP_COVID_DEMO = CO_MAP_COVID %>%
            mutate(., demo_var = ntile(get(yes_periods(input$DemoData)), 3)) 
        CO_MAP_COVID_DEMO$demo_var %<>%
            gsub(1, "(Low)", .) %>%
            gsub(2, "(Med)", .) %>%
            gsub(3, "(High)", .)
        
        demographic_map = ggplot(CO_MAP_COVID_DEMO, 
                                 aes(x=long, y=lat,
                                     group=group,
                                     fill=get(yes_periods(input$DemoData)))) +
            scale_fill_gradient_interactive(low='#e8e8e8',high='#00CED1') +
            labs(subtitle = str_wrap(xlab_long_perc(),29),
                 fill = str_wrap(xlab_perc(), 8)
                 ) +
            theme(
                text=element_text(family="calibri"),
                plot.subtitle = element_text(hjust=1)
                ) +
            guides(x.axis=FALSE) +
            bi_theme() + 
            coord_map("mercator") +
            xlim(-109.25, -101.75) + ylim(36.75, 41.25) +
            xlab(NULL) + ylab(NULL) + 
            geom_polygon_interactive(aes(tooltip = 
                                             paste(paste0(COUNTY, " COUNTY"),
                                                   paste(round(get(yes_periods(input$DemoData)),1), xlab_perc(), demo_var, sep=" "), 
                                                   sep="\n"),
                                         data_id = COUNTY, onclick = WIKI),color="black")
        
        demographic_map = demographic_map +
            guides(shape = guide_legend(override.aes = list(size = 1.5)),
                   color = guide_legend(override.aes = list(size = 1.5))
                   ) +
            theme(legend.title = element_text(size = 13, face="bold"),
                   legend.text = element_text(size = 11),
                  text=element_text(family="calibri"))
        
        girafe(ggobj = demographic_map, options = list(
            opts_hover(css = "opacity:0.8;"),
            opts_tooltip(offx = 20, offy = -55),
            opts_selection(type = "none")
        ))
    })
    
    #### Overlap of COVID Measure & Demographic Measure Severity Maps ####
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
            gsub("3-NA", NA, .) 
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
                 ) + 
            theme(text=element_text(family="calibri"),
                  plot.subtitle = element_text(hjust=1, size = 8),
                  legend.title = element_blank()
                  ) +
            guides(fill=FALSE,
                   x.axis=FALSE) +
            bi_theme() + 
            coord_map("mercator") +
            xlim(-109.25, -101.75) + ylim(36.75, 41.25) +
            xlab(NULL) + ylab(NULL) +
            geom_polygon_interactive(aes(tooltip = 
                                             paste(paste0(COUNTY, " COUNTY"),
                                                   paste(round(get(yes_periods(input$DemoData)),1), xlab_perc(), 
                                                         paste0("(",demo_var,")"), sep=" "), 
                                                   paste(formatC(get(input$COVIDbuttons), format="f", big.mark = ",", digits=1), ylab_perc_flip(), 
                                                         paste0(" (",covid_var,")"),
                                                         sep=" "), sep="\n"),
                                         data_id = COUNTY, onclick = WIKI
                                         ),
                                     color="black")
        
        map2 = map2 +
            theme(text=element_text(family="calibri"))
            
        girafe(ggobj = map2, options = list(
            opts_hover(css = "opacity:0.8;"),
            opts_tooltip(offx = 20, offy = -55),
            opts_selection(type = "none"
                           , only_shiny = FALSE
            )
        ))
    })
    
    #### Overlap Map Color Legend with number of Counties in each group ####
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
            gsub("3-NA", NA, .) 
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
            labs(subtitle = str_wrap(paste(ylab_long_perc(), xlab_long_perc(), sep="\nvs. "), 20)) + 
            theme(text=element_text(family="calibri"),
                  plot.subtitle = element_text(hjust=1, size = 8)
                  ) +
            guides(x.axis=FALSE) +
            bi_theme() + 
            coord_map("mercator") +
            xlim(-109.25, -101.75) + ylim(36.75, 41.25) +
            xlab(NULL) + ylab(NULL) +
            geom_polygon_interactive(aes(tooltip = 
                                             paste(paste0(COUNTY, " COUNTY"),
                                                   paste(round(get(yes_periods(input$DemoData)),1), xlab_perc(), 
                                                         paste0("(",demo_var,")"), sep=" "), 
                                                   paste(formatC(get(input$COVIDbuttons), format="f", big.mark = ",", digits=1), ylab_frontspace(), 
                                                         paste0(" (",covid_var,") "),
                                                         sep=""), sep="\n"),
                                         data_id = COUNTY, onclick = WIKI
                                         ),
                                     color="black")
        
        leg2 = leg2 +
            scale_fill_manual(
                name = str_wrap(paste0("Colorado's 64 Counties by\n", xlab_long_perc(), " vs. \n", ylab_long_perc()), 30),
                labels = c(paste0("Both Low (", bi_var_table$counties[1], ")"), 
                           paste0("Low Demo / Med COVID (", bi_var_table$counties[2], ")"), 
                           paste0("Low Demo / High COVID (", bi_var_table$counties[3], ")"),
                           paste0("Med Demo / Low COVID (", bi_var_table$counties[4], ")"), 
                           paste0("Both Medium (", bi_var_table$counties[5], ")"), 
                           paste0("Med Demo / High COVID (", bi_var_table$counties[6], ")"),
                           paste0("High Demo / Low COVID (", bi_var_table$counties[7], ")"), 
                           paste0("High Demo / Med COVID (", bi_var_table$counties[8], ")"), 
                           paste0("Both High (", bi_var_table$counties[9], ")")),
                values = c("#e8e8e8","#dfb0d6","#be64ac",
                           "#ace4e4","#a5add3","#8c62aa",
                           "#5ac8c8","#5698b9","#3b4994")
                ) +
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
    })
    
    
    ######## Race/Ethnicity Comparisons ########
    
    #### Use this tool to compare and contrast trends for the 10 Demographic Factors with Race/Ethnicity Data ####
    output$Racefacetgrid <- renderPlot({
        Facet_race = CO_Race_Measures_COVID19 %>%
            filter(.,  !is.na(get(input$RaceDemo)) &
                       !is.na(get(input$COVIDbuttons)) &
                       !is.na(COUNTY) &
                       Test.Type == "Measure" 
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
                  axis.title = element_text(face="bold", size=16),
                  axis.text = element_text(face="bold"),
                  legend.text = element_text(size=12, color="darkblue", face="bold"),
                  legend.title = element_text(color="darkblue", face="bold", size=14),
                  legend.background =
                      element_rect(fill="#F2F5F7", color="gray", size=1),
                  strip.text.x = element_text(face="bold", size=14)) +
            xlab(race_xlab_perc()) +
            ylab(ylab_perc())
        
        Facet_race
    })
    
    #### Race/Ethnicity Summary Statistics ####
    output$Race_Stats <- renderDataTable({
        racestats = CO_Race_Measures_COVID19 %>%
            filter(.,  !is.na(get(input$RaceDemo)) &
                       !is.na(get(input$COVIDbuttons)) &
                       !is.na(COUNTY) &
                       Test.Type == "Measure"
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
                  extensions = c("Buttons", "Select"),
                  options = list(
                      pageLength = 6,
                      scrollX=TRUE, 
                      fixedColumns = list(leftColumns = 1),
                      info=F,
                      paging=F,
                      select=TRUE,
                      dom = 'Brtip',
                      buttons = c('copy', 'csv', 'excel'
                      )
                  )
        )
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
