library(shiny); library(dplyr); library(ggplot2);
library(plotly); library(dataRetrieval); library(tidyr);
library(shinyWidgets); library(RColorBrewer);library(leaflet)

##TODO:
## change to color brewer set1 or dark2
## remove 2018 from x axis

##format data after downloading
formatDailyData <- function(df){
  
  df <- df %>% dplyr::rename(flow = X_00060_00003) %>%
    mutate(flow = replace(flow, which(flow == -999999), NA)) %>%
    mutate(DOY = as.Date(strftime(Date, "%m%d"), "%m%d")) %>%
    mutate(DOYchar = strftime(Date, "%m%d")) %>%
    mutate(year = strftime(Date, "%Y")) %>%
    mutate(year2 = year, year3 = year)
}
formatStateData <- function(df){
  df <- df %>% mutate(name = paste(site_no, station_nm, sep = " "))
  listResult <- lapply(1:nrow(df), function(x) df[x,"site_no"])
  names(listResult) <- df$name
  return(listResult)
}
stateCodes <- lapply(1:50, function(x) state.abb[x])
names(stateCodes) <- state.abb
## -----------------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Streamflow Visualizer!"),
  fluidRow(
    ## sidebar column
    column(4, tabsetPanel(
      tabPanel("Choose Station",
               ## get STAID & fetch data
               selectInput("stateChoice", label = "Select State...",
                           choices = stateCodes,
                           selected = "AL"),
               uiOutput("siteList"),
               helpText("A map of streamflow stations can be found",
                        a("here.",
                          href="https://waterwatch.usgs.gov/?id=ww_current",
                          target="_blank")),
               actionButton("fetch", "Get Data"),     
               helpText("Note: Retrieval can be slow for stations",
                        "with many years of data."), 
               leafletOutput("mymap")
      ),
      ## -------------------------------------------------------
      tabPanel("Graph Settings",
               ## add log scale to y
               h4("Adjust Graph Axes"),
               checkboxInput("logScaleY",
                             label = "Log Scale (Vertical Axis)", value = FALSE),
               
               ## change months of x-axis
               sliderTextInput(
                 inputId = "plotMonths",
                 label = "Horizontal Axis Range:",
                 choices = month.name,
                 selected = month.name[c(1, 12)]),
               hr(),
               h4("Choose which years to plot"),
               ## choose how to select years
               selectInput("yearSelection",
                           label = "Select years...",
                           choices = list("by Chronological Range" = 1,
                                          "Manually" = 2,
                                          "by Flow" = 3),
                           selected = 1),
               ## year selection methods:
               conditionalPanel(
                 condition = "input.yearSelection == 1",
                 uiOutput("yearRange")),
               
               conditionalPanel(
                 condition = "input.yearSelection == 2",
                 uiOutput("yearManual")),
               
               conditionalPanel(
                 condition = "input.yearSelection == 3",
                 helpText("Select years based on streamflow for a",
                          "specific day of the year. Choose date ",
                          "and then click 'Update Date'.",
                          "Input reference day in  MM-DD format."),
                 helpText("Note: Years with missing streamflow data on",
                          "the selected reference day will not be plotted"),
                 textInput("refDay", label = "Reference Day of Year",
                           value = "01-01"),
                 actionButton("updateRefDay", "Update Reference Date"),
                 uiOutput("flowSlider"))
      ))),
    ## main column/output
    column(8,
           ##verbatimTextOutput("testtext"),
           plotlyOutput("graph", height = 300),
           plotlyOutput("graph2", height = 300))
  )
)
## -----------------------------------------------------------------------------
server <- function(input, output){
  
  ## update data based on new station
  flowData <- eventReactive(input$fetch, {
    readNWISdv(input$siteFromList, "00060") %>% formatDailyData(.)
  })
  
  siteData <- eventReactive(input$fetch, {
    readNWISsite(input$siteFromList)
  })
  
  refFlows <- eventReactive(input$updateRefDay, {
    wideDF <- flowData() %>%
      dplyr::select(-DOY, -Date, -X_00060_00003_cd) %>%
      spread(DOYchar, flow)
    wideDF[,gsub("-", "", input$refDay)]
  })
  ## -------------------------------------------------------------------------
  ## create dynamic UI for site selection by state
  stateSites <- eventReactive(input$stateChoice, {
    formatStateData(whatNWISsites(stateCd=input$stateChoice,
                                  parameterCd="00060",
                                  hasDataTypeCd="dv"))
  })
  
  output$siteList <- renderUI({
    selectInput("siteFromList", "Search by Site ID or Name",
                choices = stateSites(),
                selected = NULL,
                selectize = TRUE)
  })
  ## -------------------------------------------------------------------------
  ## create dynamic UI for year selection
  output$yearManual <- renderUI({
    ## select years manually
    checkboxGroupInput("plotYears", "",
                       min(flowData()$year):max(flowData()$year),
                       selected =  min(flowData()$year):max(flowData()$year))
    ## add select all/deselect all
  })
  output$yearRange <- renderUI({
    ## select years by range
    sliderInput("plotRange",
                label = "",
                min = as.numeric(min(flowData()$year)),
                max = as.numeric(max(flowData()$year)),
                value = c(min(flowData()$year), max(flowData()$year)),
                sep="")
  })
  output$flowSlider <- renderUI({
    ## select years by flow range
    sliderInput("flowRange", label = "Select Flow range (cfs) on Reference Date:",
                min = min(refFlows(), na.rm = TRUE),
                max = max(refFlows(), na.rm = TRUE),
                value = c(min(refFlows(), na.rm = TRUE), max(refFlows(), na.rm = TRUE)),
                step = 10)
  })
  ## -------------------------------------------------------------------------
  ##output$testtext <- renderPrint({ refFlows() })
  
  pdata<-reactive({
    if(input$yearSelection == 1){
      ## assign years to print after initially retrieving data
      if(is.null(input$plotRange)){
        plotYears <- min(flowData()$year):max(flowData()$year)
      } else {
        plotYears <- input$plotRange[1]:input$plotRange[2]
      }
      pdata <- subset(flowData(), year %in% plotYears)
    } else if(input$yearSelection == 2){
      pdata <- subset(flowData(), year %in% input$plotYears)
    } else if(input$yearSelection == 3){
      if(is.null(input$flowRange)){
        ##plotYears <- min(flowData()$year):max(flowData()$year)
        plotYears <- 1990:2017
      } else {
        yrs <- min(flowData()$year):max(flowData()$year)
        ind <- which(refFlows() >= input$flowRange[1] & refFlows() <= input$flowRange[2])
        plotYears <- yrs[ind]
      }
      pdata <- subset(flowData(), year %in% plotYears)
    }
    pdata
  })
  
  summary<-reactive({
    pdata<-as.data.frame(pdata())
    summary.wide<- pdata %>% group_by(DOYchar) %>%
      summarise(`25%`=quantile(flow, probs=0.25, na.rm = T),
                `50%`=quantile(flow, probs=0.5, na.rm = T),
                `75%`=quantile(flow, probs=0.75, na.rm = T),
                DOY=DOY[1],
                min.year=min(year),
                max.year=max(year))
    summary <- gather(summary.wide, percentile, flow, '25%':'75%', factor_key=FALSE)
    summary
  })
  
  output$graph <- renderPlotly({

    ## ---------------------------------------------------------------------
    
    #flexible color palette
    getPalette = colorRampPalette(brewer.pal(12, "Set3"))
    pdata<-as.data.frame(pdata())
    
    p1 <- ggplot(pdata,
                 aes(DOY, flow, color = year, linetype = year)) +
      geom_line(aes(group = year3)) +
      theme_bw() +
      xlab("Day of Year") +
      ylab("Flow (cfs)") +
      theme(legend.position="bottom") +
      ggtitle(paste("Site:",siteData()$station_nm)) +
      scale_x_date(limits = as.Date(c(paste0("2018-",input$plotMonths[1], "-01"),
                                      paste0("2018-",input$plotMonths[2], "-30")),
                                    format = "%Y-%B-%d"),
                   date_labels = "%b %d") +
      scale_linetype_manual(values = rep(c("solid", "dashed", "dotdash",
                                    "twodash", "dotted", "4C88C488"), 
                                    length=length(unique(pdata$year)))) +
      scale_color_manual(values = getPalette(length(unique(pdata$year))))
    
    if(input$logScaleY) p1 <- p1 + scale_y_log10()
    ggplotly(p1, tooltip = c("year", "flow"))
  })
  output$graph2 <- renderPlotly({
    
    summary<-as.data.frame(summary())
    
    p2 <- ggplot(summary, aes(DOY,flow))+
      geom_line(aes(group = percentile, linetype=percentile)) +
      scale_linetype_manual(values = c("dotted", "solid", "dotted"))+
      theme_bw() +
      xlab("Day of Year") +
      ylab("Flow (cfs)") +
      ggtitle(paste("Median, Q1, Q3 Daily Mean Flows",min(summary$min.year), "-",max(summary$max.year))) +
      scale_x_date(limits = as.Date(c(paste0("2018-",input$plotMonths[1], "-01"),
                                      paste0("2018-",input$plotMonths[2], "-30")),
                                    format = "%Y-%B-%d"),
                   date_labels = "%b %d") 
    
    if(input$logScaleY) p2 <- p2 + scale_y_log10()
    ggplotly(p2, tooltip = c("percentile", "flow"))
  })
  
  ##--------------------------------------------------------------------
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldImagery) %>%
      addMarkers(lng=siteData()$dec_long_va, lat=siteData()$dec_lat_va, popup=siteData()$station_nm)
      
  })
  
}



shinyApp(ui = ui, server = server)
