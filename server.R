library(shiny)
library(tidyverse)
library(magrittr)
library(sf)
library(rgeos)
library(maps)
library(tools)
library(biscale)
library(stringr)
library(cowplot)
library(psych)
library(plotrix)
library(gridExtra)
library(multipanelfigure)
library(data.table)

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session){
    # DemoData <- reactive({
    #   Demochoices == colnames(input$catbuttons)
    # })
    observe({
        Demochoices = get(input$catbuttons)
        updateSelectizeInput(session, "DemoData", 
                             choices = colnames(Demochoices)[-1:-5],
                             selected = colnames(Demochoices)[6])
    }) 
    covidrange = reactive({seq(min(x), max(x), 
                               length.out = input$COVID.Cases.Max)})
    medianrange = reactive({seq(min(x), max(x), 
                                length.out = input$Median.Household.Income + 1)})
    ruralrange = reactive({seq(min(x), max(x), 
                               length.out = input$Perc.Rural + 1)})
    
    xlab_clean = reactive({no_periods(input$DemoData)})
    xlab_perc = reactive({no_perc(no_periods(input$DemoData))})
    xlab_short = reactive({word(no_periods(input$DemoData), 1, 2, sep=" ")})
    xlab_no_perc = reactive({word(no_periods(input$DemoData), 2, -1, sep=" ")})
    ylab_clean = reactive({no_periods(input$COVIDbuttons)})
    ylab_perc = reactive({no_perc(no_periods(input$COVIDbuttons))})
    ylab_short = reactive({word(no_periods(input$COVIDbuttons), 1,2, sep=" ")})
    ylab_rest = reactive({word(no_periods(input$COVIDbuttons), 3,-1, sep=" ")})
   # ylab_2line = reactive({paste(ylab_short, ylab_rest, sep="\n")})
    
    output$top20 <- renderPlot({
        #### ALL_DATA Top 20 BAR CHART BY COUNTY ####
        #top20counties = 
        CO_COUNTY_COVID_FILTER %>% 
            filter(., !is.na(get(input$DemoData)) &
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
            ggplot(., aes(x=
                              #COUNTY,
                              reorder(COUNTY, -get(input$DemoData)), 
                          y=get(input$DemoData),
                          fill=get(input$COVIDbuttons))) +
            geom_col() + 
            ggtitle(paste0("Colorado Counties -- ", xlab_clean())) + 
            scale_fill_gradient(low="#3399FF", high="red2") +
        
            labs(x="Counties", y=xlab_perc()
                    , fill = str_wrap(ylab_perc(), 12)
                    , subtitle = paste0("Color Shading by ", ylab_short())
                   ) +
             theme(panel.grid.major.x = element_blank(),
                  axis.text.x =
                      element_text(angle=90, hjust=1, vjust=0.3, face="bold"),
                  axis.text.y = element_text(face="bold"),
                  legend.title = element_text(color="darkblue", face="bold"),
                  legend.text = element_text(color="darkblue"),
                  plot.title = element_text(hjust=0.5, face="bold", size=22),
                  plot.subtitle = element_text(hjust=0.5, color="darkblue", size=18),
                  axis.title = element_text(face="bold", size=14),
                  strip.text.x = element_text(face="bold"),
                  legend.background =
                      element_rect(fill="#F2F5F7", color="gray", size=1))
    })
    output$demo_data = renderTable({
        #### ALL_DATA STATS ####
        CO_COUNTY_COVID_FILTER %>% 
            filter(., #Demographic != "ALL" & 
                   !is.na(get(input$DemoData)) &
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
                   COVID.Mortality.Perc, input$DemoData
            ) %>% 
            summarise(., Counties.w.Data = n(),
                      Mean = mean(get(input$DemoData)),
                      Median = median(get(input$DemoData)),
                      Min = min(get(input$DemoData)),
                      Max = max(get(input$DemoData)),
                      Range = max(get(input$DemoData)) -
                          min(get(input$DemoData)),
                      Std.Dev = sd(get(input$DemoData)),
                      Srd.Error = std.error(get(input$DemoData)),
                      Skew = round(skew(get(input$DemoData)),2)
                      #paste0("Mean ", ylab_short()) 

                      # ,Margin.of.Error = 1.96*S.E
                          # , round((
                          # qnorm(.95, Mean, Std.Dev)*Mean/sqrt(Counties.w.Data)),2)
            )
    }, caption=paste("<b> Statistics for Selected Demographic Measure </b>"),
    caption.placement = getOption("xtable.caption.placement", "top"),
    caption.width = getOption("xtable.caption.width", NULL)
    )
    
        output$COVID_data = renderTable({
            #### ALL_DATA STATS ####
            CO_COUNTY_COVID_FILTER %>% 
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
                       COVID.Mortality.Perc, input$DemoData
                ) %>% 
                summarise(., Counties.w.Data = n(),
                          Mean = mean(get(input$COVIDbuttons)),
                          Median = median(get(input$COVIDbuttons)),
                          Min = min(get(input$COVIDbuttons)),
                          Max = max(get(input$COVIDbuttons)),
                          Range = max(get(input$COVIDbuttons)) -
                              min(get(input$COVIDbuttons)),
                          Std.Dev = sd(get(input$COVIDbuttons)),
                          Srd.Error = std.error(get(input$COVIDbuttons)),
                          Skew = round(skew(get(input$COVIDbuttons)),2)
                          #paste0("Mean ", ylab_short()) 
                          
                          # ,Margin.of.Error = 1.96*S.E
                          # , round((
                          # qnorm(.95, Mean, Std.Dev)*Mean/sqrt(Counties.w.Data)),2)
                )

    }, caption=paste("<b> Statistics for Selected COVID Measure </b>"),
    caption.placement = getOption("xtable.caption.placement", "top"),
    caption.width = getOption("xtable.caption.width", NULL))
    
})

