library(shiny)
library(tidyverse)
library(magrittr)
library(lubridate)
library(sf)
library(rgeos)
library(maps)
library(tools)
library(biscale)
library(stringr)
library(corrr)
library(cowplot)
library(psych)
library(plotrix)
library(gridExtra)
library(multipanelfigure)
library(data.table)
library(ggiraph)
library(shinydashboard)
library(GGally)
library(DT)

# install.packages("shinyjs")
# library(shinyjs)

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
    ylab_clean = reactive({no_periods(input$COVIDbuttons)})
    ylab_perc = reactive({no_perc(no_periods(input$COVIDbuttons))})
    ylab_short = reactive({word(no_periods(input$COVIDbuttons), 1,2, sep=" ")})
    ylab_rest = reactive({word(no_periods(input$COVIDbuttons), 3,-1, sep=" ")})
    ylab_rev = reactive({rev_perc(no_periods(input$COVIDbuttons))})
    ylab_frontspace = reactive({rev_perc(frontspace(no_periods(input$COVIDbuttons)))})
    ylab_long_perc = reactive({long_perc(no_periods(input$COVIDbuttons))})
    countyname = reactive({input$single})
    bal_lab_clean = reactive({no_periods(input$balancebuttons)})
    catbuttons_clean = reactive({upper_Co(stri_trans_totitle(no_underscore(input$catbuttons)))})
    catbuttons_long = reactive({no_Co(stri_trans_totitle(no_underscore(input$catbuttons)))})
    covid_lab_perc = reactive({no_perc(no_periods(input$COVIDselect))})
    covid_lab_long_perc = reactive({long_perc(no_periods(input$COVIDselect))})
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
        explorer = correlate(get(input$catbuttons)[-1], method=input$corr_type) %>% 
            focus(COVID.Tests.Per.100000, COVID.Cases.Per.100000, 
                  COVID.Deaths.Per.100000, COVID.Mortality.Perc) %>% 
            mutate(rowname = reorder(rowname, get(input$COVIDbuttons))) %>%
            ggplot(aes(rowname, get(input$COVIDbuttons), fill=get(input$COVIDbuttons))) +
            scale_fill_gradient2(low="red", mid="grey80", high="blue") +
            labs(title=paste("Correlation Values of", 
                             paste0(ylab_long_perc(), " vs."),
                             paste0(catbuttons_long()," Measures"), 
                             sep="\n"),
                 fill = str_wrap(ylab_perc(), 7)
                 # ,subtitle = "Scale between -1 and 1"
            ) +
            theme(
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
            ylab(paste0("Correlation range [-1,1] by ", ylab_short())) + coord_flip() + 
            geom_col_interactive(aes(tooltip=
                         paste("In Colorado counties,\n", long_perc(no_periods(rowname)), 
                               "\n& ", ylab_long_perc(),
                               "\nhave a", round(get(input$COVIDbuttons),2),
                               stri_trans_totitle(input$corr_type),"Correlation", sep=" "))) +
            geom_vline_interactive(xintercept=-0.2, aes(tooltip="Significant Negative Correlation"),
                                   color="darkblue", linetype="dashed", alpha=0.5, size=1) +
            geom_vline_interactive(xintercept=0.2, aes(tooltip="Significant Positive Correlation"),
                                   color="darkred", linetype="dashed", alpha=0.5, size=1) 
        
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
        corrdata[-1:-6]=round(corrdata[-1:-6], 2)
        # corrdata = formatRound(corrdata, columns=c(-1:-6), digits=2)
        datatable(corrdata, rownames=F,
                  filter = list(position = 'top', clear = FALSE),
                  extensions = c("Buttons", "Select"),
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
            ggtitle(str_wrap(paste0("Colorado Counties -- ", xlab_long_perc()),35)) + 
            scale_fill_gradient(low="#3399FF", high="red2") +
        
            labs(x="Counties", y=xlab_perc()
                    , fill = str_wrap(ylab_perc(), 7)
                    , subtitle = paste0("Color Shading by ", ylab_short())
                   ) +
             # geom_text(aes(label=round(get(input$DemoData),1)), 
             #          position = position_jitter(width=0, height=1.5), vjust=5.5, color="white"
             #           ) +
             theme(panel.grid.major.x = element_blank(),
                  axis.text.x =
                      element_text(angle=90, hjust=1, vjust=0.3, face="bold", size=6),
                  axis.text.y = element_text(face="bold"),
                  legend.title = element_text(color="darkblue", face="bold"),
                  legend.text = element_text(color="darkblue"),
                  plot.title = element_text(hjust=0.5, face="bold", size=18),
                  plot.subtitle = element_text(hjust=0.5, face="bold", color="darkblue", size=14),
                  axis.title = element_text(face="bold", size=14),
                  strip.text.x = element_text(face="bold"),
                  legend.background =
                      element_rect(fill="#F2F5F7", color="gray", size=1)) +
                    geom_col_interactive(stat="identity", 
                         aes(tooltip = paste(paste0(COUNTY, " COUNTY"),
                                             (paste(round(get(input$DemoData),1), xlab_perc(), sep=" ")),
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
                   !is.na(get(input$DemoData)) &
                       !is.na(COUNTY) 
            ) %>% 
            ggplot(aes(x=get(input$DemoData), y=get(input$COVIDbuttons)
                       #,color = bi_class
                       #,color=as.character(var1), shape=as.character(var2)
            )
            # ,size=Population
            ) + 
            geom_point_interactive(stat="identity", 
                                   size=2.5,
                                   # size=Population,
                                   aes(
                                       # color=bi_class, 
                                       
                                       color = get(input$balancebuttons),
                                       tooltip = paste(paste0(COUNTY, " COUNTY"),
                                                       (paste(round(get(input$DemoData),1), xlab_perc(), sep=" ")),
                                                       (paste(round(get(input$COVIDbuttons),1), ylab_perc(), sep=" ")), sep=" \n")
                                   )) +
            geom_smooth(method="glm") +
            xlab(xlab_perc()) + ylab(ylab_perc()) +
            labs(title = "Scatterplot Correlations by County",
                 subtitle = paste0(ylab_short(), " vs. ", str_wrap(xlab_long_perc(),21)),
                 color=bal_lab_clean(),
                 caption = "Hover over any point to see County statistics") +
            theme(axis.text.x = element_text(face="bold"),
                  axis.text.y = element_text(face="bold"),
                  legend.title = element_text(color="darkblue", face="bold", size=13),
                  legend.text = element_text(color="darkblue", face="bold", size=11),
                  plot.title = element_text(face="bold", size=18),
                  plot.subtitle = element_text(hjust=0.5, color="darkblue", size=14),
                  axis.title = element_text(face="bold", size=12),
                  strip.text.x = element_text(face="bold"),
                  legend.background =
                      element_rect(fill="#F2F5F7", color="gray", size=1)) 
        # +
            # scale_color_manual_interactive(name = paste("Correlation by County of",
            #                                             str_wrap(paste0(xlab_long_perc(), " (Demo)"),21),
            #                                             paste0("& ", ylab_short()),
            #                                             sep="\n"),
                                           
                                           ### RdYlBl colors
                                           #' breaks = c("1-1", "2-1", "3-1",
                                           #'            "1-2", "2-2", "3-2",
                                           #'            "1-3", "2-3", "3-3"),
                                           #' # FIRST COLOR PALETTE (blue-->red)
                                           #' values = c('#234F1E',
                                           #'            '#4575b4','#74add1','#abd9e9',
                                           #'            #'#e0f3f8','#ffffbf','yellow',
                                           #'            '#fee090',
                                           #'            '#fdae61','#f46d43','#d73027', 
                                           #'            'darkred'),
                                           #' # SECOND COLOR PALETTE (blue green yellow red)
                                           #' # values = c('#3288bd','#66c2a5','#abdda4',
                                           #' #            '#e6f598','#ffffbf','#fee08b',
                                           #' #            '#fdae61','#f46d43','#d53e4f'),
                                           #' labels = c("Low Demo / Low COVID", 
                                           #'            "Med Demo / Low COVID", 
                                           #'            "High Demo / Low COVID",
                                           #'            "Low Demo / Med COVID", 
                                           #'            "Med Demo / Med COVID",
                                           #'            "High Demo / Med COVID", 
                                           #'            "Low Demo / High COVID", 
                                           #'            "Med Demo / High COVID", 
                                           #'            "High Demo / High COVID")
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
                summarise(., Measure = xlab_clean(),
                          Counties = n(),
                          Min = round(min(get(input$DemoData)),2),
                          Q1 = round(quantile(get(input$DemoData), 0.25),2),
                          Median = round(median(get(input$DemoData)),2),
                          Mean = round(mean(get(input$DemoData)),2),
                          Q3 = round(quantile(get(input$DemoData), 0.75),2),
                          Max = round(max(get(input$DemoData)),2),
                          Range = round(max(get(input$DemoData)) -
                              min(get(input$DemoData)),2),
                          S.Dev = round(sd(get(input$DemoData)),2),
                          S.Error = round(std.error(get(input$DemoData)),2),
                          Skew = round(skew(get(input$DemoData)),2))
            
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
                       COVID.Mortality.Perc, input$DemoData
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
                       demo_var = ntile(get(input$DemoData), 3)
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
                            fill=get(input$DemoData))) +
                scale_fill_gradient_interactive(low='#e8e8e8',high='#5ac8c8') +
                # scale_fill_distiller(palette = "Spectral") +
               # bi_scale_fill(pal = "DkBlue", dim = 3) +
                labs(subtitle = str_wrap(xlab_long_perc(),29)
                     
                ) +
                theme(plot.subtitle = element_text(hjust=1),
                      #plot.margin=unit(c(2,2,0,0),"cm")
                ) +
               guides(fill=FALSE, x.axis=FALSE) +
                #theme(title)
                bi_theme() + coord_map("mercator") +
                xlim(-109.25, -101.75) + ylim(36.75, 41.25) +
                xlab(NULL) + ylab(NULL) + 
                
                geom_polygon_interactive(aes(tooltip = 
                         paste(paste0(COUNTY, " COUNTY"),
                               paste(round(get(input$DemoData),1), xlab_perc(), demo_var, sep=" "), 
                                sep="\n"),
                         data_id = COUNTY, onclick = WIKI),color="black")
            
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
                labs(subtitle = ylab_perc()
                ) +
                theme(plot.subtitle = element_text(hjust=1),
                      #plot.margin=unit(c(2,2,0,0),"cm")
                ) +
                guides(fill=FALSE, x.axis=FALSE) +
                #theme(title)
                bi_theme() + coord_map("mercator") +
                xlim(-109.25, -101.75) + ylim(36.75, 41.25) +
                xlab(NULL) + ylab(NULL) +
                #coord_map("polyconic" ) #+
                geom_polygon_interactive(aes(tooltip = 
                 paste(paste0(COUNTY, " COUNTY"),
                       paste(round(get(input$COVIDbuttons),1), ylab_perc(), covid_var, sep=" "), 
                       sep="\n"),
                 data_id = COUNTY, onclick = WIKI),color="black")
            
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
                
                #guides(fill=FALSE, x.axis=FALSE) +
                #theme(title)
                bi_theme() + coord_map("mercator") +
                xlim(-109.25, -101.75) + ylim(36.75, 41.25) +
                xlab(NULL) + ylab(NULL) +
                #coord_map("polyconic" ) #+
                geom_polygon_interactive(aes(tooltip = 
                                                 paste(paste0(COUNTY, " COUNTY"),
                                                       get(input$balancebuttons),
                                                       paste0("Population: ", Population),
                                                       paste0("Median Income: $", Median.Household.Income),
                                                       paste0(Perc.Rural, "% Rural"),
                                                       paste(get(input$DemoData),xlab_perc(), sep=" "),
                                                       paste(round(get(input$COVIDbuttons),1),ylab_perc(), sep=" "),
                                                       sep="\n"),
                                         data_id = COUNTY, onclick = WIKI),
                                         color="black") +
                theme(
                    legend.title = element_text(color="darkblue", face = "bold", size=13),
                    legend.position = "right",
                    legend.text = element_text(color="darkblue", face = "bold", size=11),
                    plot.title = element_text(hjust=0.5, size=17),
                    axis.title = element_text(face="bold", size=12),
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
                       var1 = ntile(get(input$DemoData), 3),
                       var2 = ntile(get(input$COVIDbuttons), 3),
                       bi_class = paste0(
                           as.numeric(var1), "-",
                           as.numeric(var2)
                       ))
            CO_COUNTY_BI_CLASS = CO_COUNTY_COVID_FILTER %>%
                mutate(.,
                       demo_var = ntile(get(input$DemoData), 3),
                       covid_var = ntile(get(input$COVIDbuttons), 3),
                       bi_class = paste0(
                           as.numeric(demo_var), "-",
                           as.numeric(covid_var)
                       )) %>%
                select(., COUNTY, demo_var, covid_var, bi_class)
            CO_MAP_COVID1 = left_join(CO_MAP_COVID, CO_COUNTY_BI_CLASS, by="COUNTY")
            CO_MAP_COVID1$bi_class %<>%
                gsub("NA-1", NA, .) %>%
                gsub("NA-2", NA, .) %>%
                gsub("NA-3", NA, .)
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
                labs(subtitle = paste(ylab_clean(), str_wrap(xlab_long_perc(),30), sep="\nvs. ")
                     # ,x = "Bivariate Analysis")
                     )+
                theme(plot.subtitle = element_text(hjust=1),
                      #plot.margin=unit(c(2,2,0,0),"cm")
                      ) +
                guides(fill=FALSE, x.axis=FALSE) +
                #theme(title)
                bi_theme() + coord_map("mercator") +
                xlim(-109.25, -101.75) + ylim(36.75, 41.25) +
                xlab(NULL) + ylab(NULL) +
                #coord_map("polyconic" ) #+
                geom_polygon_interactive(aes(tooltip = 
                            paste(paste0(COUNTY, " COUNTY"),
                                paste(round(get(input$DemoData),1), xlab_perc(), 
                                      paste0("(",demo_var,")"), sep=" "), 
                                paste(round(get(input$COVIDbuttons),1), ylab_frontspace(), 
                                      paste0(" (",covid_var,") "),
                                      sep=""), sep="\n"),
                            data_id = COUNTY, onclick = WIKI
                            ),color="black")
                
            # ggiraph(code = print(map2))
            girafe(ggobj = map2, options = list(
                opts_sizing(rescale = TRUE, width = .7),
                opts_hover(css = "opacity:0.8;"),
                opts_tooltip(offx = 20, offy = -55),
                opts_selection(type = "none"
                               , only_shiny = FALSE
                               )
            ))
           })
           output$bi_cor <- renderGirafe({
               CO_COUNTY_BI_SLIDERS = CO_COUNTY_COVID_FILTER %>%
                   mutate(., 
                          demo_var = ntile(get(input$DemoData), 3),
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
                   gsub("NA-3", NA, .)
               
               
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
                   theme(
                       # axis.text.x = element_text(angle=90, hjust=1, vjust=0.3)
                   ) + 
                   xlab("County Category") +
                   ylab("Percent of Counties with Correlation") +
                   scale_y_continuous(labels = scales::percent) +
                   labs(title="Balancing Factors of Counties \nwith Strong Correlation", 
                        subtitle = paste0(ylab_short(), " vs. ", xlab_long_perc()),
                        caption = "Hover over any bar to see County Type statistics") +
                   theme(panel.grid.major.y = element_blank(),
                         axis.text.x = element_text(face="bold"),
                         axis.text.y = element_text(face="bold"),
                         legend.title = element_text(color="darkblue", face="bold"),
                         legend.text = element_text(color="darkblue"),
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
                          demo_var = ntile(get(input$DemoData), 3),
                          covid_var = ntile(get(input$COVIDbuttons), 3),
                          bi_class = paste0(
                              as.numeric(demo_var), "-",
                              as.numeric(covid_var)
                          )) %>% 
                   select(., COUNTY, demo_var, covid_var, bi_class)
               CO_COUNTY_BI_CLASS$bi_class %<>% 
                   gsub("NA-1", NA, .) %>% 
                   gsub("NA-2", NA, .) %>% 
                   gsub("NA-3", NA, .)
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
           output$bi_map_legend <- renderPlot({
               CO_COUNTY_BI_CLASS = CO_COUNTY_COVID_FILTER %>%
                   mutate(., 
                          demo_var = ntile(get(input$DemoData), 3),
                          covid_var = ntile(get(input$COVIDbuttons), 3),
                          bi_class = paste0(
                              as.numeric(demo_var), "-",
                              as.numeric(covid_var)
                          )) %>% 
                   select(., COUNTY, demo_var, covid_var, bi_class)
               CO_COUNTY_BI_CLASS$bi_class %<>% 
                   gsub("NA-1", NA, .) %>% 
                   gsub("NA-2", NA, .) %>% 
                   gsub("NA-3", NA, .)
             #TRYING TO make a Matrix
               # bi_var_table = CO_COUNTY_BI_CLASS %>% 
               #     group_by(bi_class) %>% 
               #     summarise(counties = n())
               # bi_matrix = matrix(bi_var_table$counties, nrow=3, ncol=3)
               # bi_matrix = t(bi_matrix)
               # bi_matrix = rotate(rotate(rotate(bi_matrix)))
               # bi_colors = c('#4575b4', '#74add1', '#abd9e9', '#e0f3f8', '#ffffbf', '#fee090',
               # '#fdae61', '#f46d43', '#d73027')
               # bi_colors_matrix = matrix(bi_colors, nrow=3, ncol=3)
               # color2D.matplot(bi_matrix, axes=T, 
               #                 xlab = paste0("Higher ", str_wrap(xlab_perc(),29), expression("" %->% "")),
               #                 ylab = paste0("Higher ", ylab_short(),expression("" %->% "")),
               #                 ), cellcolors = c([['#4575b4', '#74add1', '#abd9e9'], 
               #                                   ['#e0f3f8', '#ffffbf', '#fee090'],
               #                                   ['#fdae61', '#f46d43', '#d73027']])
               
               
               bi_df = as.data.frame(bi_matrix)
               #### This has correct numbers. Now to just paste them...
               legend2 = bi_legend(pal = "DkBlue",
                                dim = 3,
                                #xlab = paste0("Higher ", xlab_perc()),
                                xlab = paste0("Higher ", str_wrap(xlab_perc(),29)),
                                ylab = paste0("Higher ", ylab_short()),
                                # xlab = "xlab",
                                # ylab = "ylab",
                                size = 18)
            legend2
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
       })
    
           
           
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
                   summarise(., Counties = n(),
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
                   arrange(., desc(n))
               
               datatable(racestats, rownames = F, 
                         # filter = list(position = 'top', clear = FALSE),
                         extensions = c("Buttons", "Select"),
                         options = list(
                             # search = list(regex = TRUE, caseInsensitive = TRUE),
                             pageLength = 5,
                             scrollX=TRUE, 
                             fixedColumns = list(leftColumns = 1),
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
               
               
               race_xlab_clean = reactive({no_periods(input$RaceDemo)})
               race_xlab_perc = reactive({no_perc(no_periods(input$RaceDemo))})
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
                   facet_grid(. ~ Demographic, scales="free_x") +
                   stat_smooth(method=lm, color="black", alpha=0.2) +
                   ggtitle("Race/Ethnicity Outcomes by County") +
                   theme(plot.title =
                             element_text(face="bold", hjust=0.5, size=22),
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
                         strip.text.x = element_text(face="bold")) +
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
                                             tooltip="March 25th - Shutdown Announced\n
                                             (Stay-at-Home Order)"), 
                                         color="#CC5500", linetype="dashed", size=1) +
                   geom_vline_interactive(xintercept=as.Date("2020-04-27"),aes(
                                             tooltip="April 27th - Phased Reopening Begins\n
                                             (Safer-at-Home Order)"),
                                         color="orange",linetype="dashed", size=1) +
                    geom_vline_interactive(xintercept=as.Date("2020-05-27"),aes(
                        tooltip="May 27th - Phased Reopening Continues\n
                                             In-Person Dining / Summer Camps Reopen"),
                        color="yellow3",linetype="dashed", size=1) +
                        geom_vline_interactive(xintercept=as.Date("2020-06-18"),aes(
                                             tooltip="June 18th - Bars Reopen at 50% Capacity\n
                                             (Protect-Our-Neighbors Order)"),
                                         color="green",linetype="dashed", size=1) +
                   geom_vline_interactive(xintercept=as.Date("2020-06-30"),aes(
                                             tooltip="June 30th - Bars Reclosed"),
                                         color="yellow3",linetype="dashed", size=1) +
                   geom_vline_interactive(xintercept=as.Date("2020-07-16"), aes(
                                             tooltip="July 16th - Governor Polis issues\n
                                             Statewide Mask Order"
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
                                        " County Timeline:\nCOVID ", covid_lab_long_perc()),
                           y=covid_lab_perc(), 
                           subtitle = "including Major Events in Colorado's COVID response") +
                   theme(legend.position = "bottom", 
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
              inddata = COVID19ALL_MERGE %>% 
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
                          pageLength = 5,
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
                          formatRound(columns = -c(1:6), digits=2)
            
           })
})

 
