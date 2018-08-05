library(shiny); library(dplyr); library(ggplot2);
library(plotly); library(dataRetrieval); library(tidyr);
library(shinyWidgets); library(RColorBrewer);

##TODO:
## change to color brewer set1 or dark2
## remove 2018 from x axis

##format data after downloading
formatDailyData <- function(df){

    df <- df %>% dplyr::rename(flow = X_00060_00003) %>%
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
                            actionButton("fetch", "Get Data"),
                            helpText("A map of streamflow stations can be found",
                                     a("here.",
                                       href="https://waterwatch.usgs.gov/?id=ww_current",
                                       target="_blank")),
                            helpText("Note: Retrieval can be slow for stations",
                                     "with many years of data.")
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
               plotlyOutput("graph", height = 700))
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
    output$graph <- renderPlotly({
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
        ## ---------------------------------------------------------------------
        p1 <- ggplot(pdata,
                     aes(DOY, flow, color = year, linetype = year)) +
            geom_line(aes(group = year3)) +
            theme_bw() +
            xlab("Day of Year") +
            ylab("Flow (cfs)") +
            theme(legend.position="bottom") +
            ggtitle(paste("Site No.", input$STAID, siteData()$station_nm)) +
            scale_x_date(limits = as.Date(c(paste0("2018-",input$plotMonths[1], "-01"),
                                            paste0("2018-",input$plotMonths[2], "-30")),
                                          format = "%Y-%B-%d"),
                         date_labels = "%b %d") +
            scale_linetype_manual(values = c(rep("solid", 12), rep("dashed", 12),
                                             rep("dotdash", 12), rep("twodash", 12),
                                             rep("dotted", 12), rep("4C88C488", 12))) +
            scale_color_manual(values = c(brewer.pal(12, "Set3"), brewer.pal(12, "Set3"),
                                          brewer.pal(12, "Set3"), brewer.pal(12, "Set3"),
                                          brewer.pal(12, "Set3"),  brewer.pal(12, "Set3")))

        if(input$logScaleY) p1 <- p1 + scale_y_log10()
        ggplotly(p1, tooltip = c("year", "flow"))
    })
}

shinyApp(ui = ui, server = server)
## -----------------------------------------------------------------------------

##Old Code: Server
    ##yearsByFlow <- eventReactive(input$updateRefDay, {
    ##    wideDF <- flowData() %>%
    ##        dplyr::select(-DOY, -Date, -X_00060_00003_cd) %>%
    ##        spread(DOYchar, flow)
    ##    wideDF[order(wideDF[,gsub("-", "", input$refDay)]),"year"]
    ##})
    ##output$rankRange <- renderUI({
        ## select years by rank range
    ##    sliderInput("rankRange", label = "Low Flow ----------------------- High Flow ",
    ##                min = 1,
    ##                max = length(unique(flowData()$year)),
    ##                value = c(1, length(unique(flowData()$year))),
    ##                step = 1)
    ##})
