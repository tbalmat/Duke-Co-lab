#####################################################################################################
# Duke University Co-lab Shiny Workshop, Session 3, Spring 2021
# U.S. Domestic Flight Evalution App
# Shiny server script
#####################################################################################################

options(stringsAsFactors=F)
options(scipen=999999)

library(shiny)

# Set current working directory
# Local
setwd("C:/Projects/Duke/Co-lab/Shiny-Spring-2021/Session-3-ggplot")

# Import flight evaluation functions
source("App/FlightEvaluation-Functions.r", echo=F)

# Set global variables
# Configure labels for month and weekday codes
monthLabel <<- c("1"="Jan", "2"="Feb", "3"="Mar", "4"="Apr", "5"="May", "6"="Jun",
                 "7"="Jul", "8"="Aug", "9"="Sep", "10"="Oct", "11"="Nov", "12"="Dec")
weekdayLabel <<- c("1"="Mon", "2"="Tue", "3"="Wed", "4"="Thu", "5"="Fri", "6"="Sat", "7"="Sun")

# Server function
shinyServer(
  function(input, output, session) {

    ##############################################################################################################
    # Function:  Render flight map
    ##############################################################################################################

    graphFlightMap <- function(pthreshFlight, pthreshAirportLabel, colorRange, colorScaleMid, sizeRange,
                               alphaRange, facetVar, carrierDelay) {

      # Ignore if no aggregated flight data available (typically on program initialization)
      if(exists("fldatb")) {

        # Configure facet labels and rows and plot height and width based on faceting variable
        if(facetVar=="Month") {
          facetLabel <- monthLabel
          facetRows <- 2
          gheight <- 700
          gwidth <- 1200
        } else if(facetVar=="DayOfWeek") {
          facetLabel <- weekdayLabel
          facetRows <- 2
          gheight <- 550
          gwidth <- 1800
        } else if(facetVar=="DOT_ID_Reporting_Airline") {
          facetLabel <- alLabel
          facetRows <- NULL
          gheight <- 700
          gwidth <- 1600
        } else {
          facetLabel <- ""
          facetRows <- NULL
          gheight <- 700
          gwidth <- 1350
        }

        # Render plot
        g <- composePlotMap(pthreshFlight, pthreshAirportLabel, colorRange, colorScaleMid,
                            sizeRange, alphaRange, facetVar, facetLabel, facetRows)
        output$plotUSFlights <- renderPlot(g, width=gwidth, height=gheight)

      } else {

        # Display "No data" message and clear plot
        showNotification("No data for flight map", type="warning")
        output$plotUSFlights <- renderPlot(NULL)

      }

    }

    ##############################################################################################################
    # Function:  render flight delay density graph
    ##############################################################################################################

    graphFlightDelayDensity <- function(x, xlim, y, yOrder, yFillColor, reverseFillColor, fillAlpha, facetVar, vline)

      if(exists("fldatb") & exists("aldat")) {

        # Configure plot height and width
        if(input$densFacetVar=="Month") {
          facetLabel <- monthLabel
          facetRows <- 2
          gheight <- 700
          gwidth <- 800
        } else if(input$densFacetVar=="DayOfWeek") {
          facetLabel <- weekdayLabel
          facetRows <- 2
          gheight <- 700
          gwidth <- 1600
        } else if(input$densFacetVar=="DOT_ID_Reporting_Airline") {
          facetLabel <- alLabel
          facetRows <- NULL
          gheight <- 700
          gwidth <- 1600
        } else {
          facetLabel <- ""
          facetRows <- NULL
          gheight <- 700
          gwidth <- 700
        }

        # Compose and render density plot
        g <- composePlotDensity(x, xlim, y, yOrder, yFillColor, reverseFillColor, fillAlpha,
                                facetVar, facetLabel, facetRows, vline)
        output$plotDensityRidge <- renderPlot(g, width=gwidth, height=gheight)

      } else {

        # Display "No data" message and clear plot
        showNotification("No data for density plot", type="warning")
        output$plotDensityRidge <- renderPlot(NULL)

      }

    ##############################################################################################################
    # Data retrieval button event
    # Aggregate and plot flight map and flight delay density ridges
    # Do not execute (ignore) during program initialization, when input$retrieveData transitions from NULL to
    # its default value
    ##############################################################################################################

    observeEvent(input$retrieveData, {

      readData(input$dirDat)
      fldatb <<- aggfdat(input$facetVar, input$carrierDelay, input$includeCancel)

      if(nrow(fldat)>0 & nrow(fldatb)>0) {

        graphFlightMap(input$pthreshFlight, input$pthreshAirportLabel, c(input$color1, input$color2, input$color3),
                       input$colorScaleMid, input$sizeRange, input$alphaRange, input$facetVar, input$carrierDelay)
        graphFlightDelayDensity(input$densX, input$densXLim, input$densY, input$densyOrder,
                                c(input$densFillColor1, input$densFillColor2), input$densReverseFillColor,
                                input$densAlpha, input$densFacetVar, input$densVLine)

      } else {

        showNotification("No data to display.  No observations satisfy query criteria.", type="warning")

      }

    }, ignoreInit=T)

    ##############################################################################################################
    # Monitor controls that affect data aggregation needed for map generation
    # Execute during program initialization (do not ignore)
    ##############################################################################################################

    observeEvent(c(input$facetVar, input$carrierDelay, input$includeCancel), {

      fldatb <<- aggfdat(input$facetVar, input$carrierDelay, input$includeCancel)

    }, ignoreInit=F)

    ##############################################################################################################
    # Observe reactive values that prompt rendering of flight map
    # Execute during program initialization (do not ignore)
    ##############################################################################################################

    observeEvent(c(input$pthreshFlight, input$pthreshAirportLabel, input$color1, input$color2, input$color3,
                   input$colorScaleMid, input$sizeRange, input$alphaRange, input$facetVar, input$carrierDelay), {

      graphFlightMap(input$pthreshFlight, input$pthreshAirportLabel, c(input$color1, input$color2, input$color3),
                     input$colorScaleMid, input$sizeRange, input$alphaRange, input$facetVar, input$carrierDelay)

    }, ignoreInit=F)

    ##############################################################################################################
    # Observe reactive values that prompt rendering of density plot
    # Execute during program initialization (do not ignore)
    ##############################################################################################################

    observeEvent(c(input$densX, input$densXLim, input$densY, input$densyOrder, input$densFillColor1,
                   input$densFillColor2, input$densReverseFillColor, input$densAlpha, input$densFacetVar,
                   input$densVLine), {

      graphFlightDelayDensity(input$densX, input$densXLim, input$densY, input$densyOrder,
                              c(input$densFillColor1, input$densFillColor2), input$densReverseFillColor,
                              input$densAlpha, input$densFacetVar, input$densVLine)

    }, ignoreInit=F)

    ##############################################################################################################
    # Execution begins here
    # Read data from default location
    # Failure to load data during initialization causes reactive function to execute in a state
    # that loses reactive methods (changing input$ objects does not execute associated observe() functions
    # Waiting to load data with the input$retrieveData event does not resolve the issue (reactive objects
    # fail to execute changes even after data are loaded in the observe event)
    # Explicit load of data resolves the issue and all reactive elements behave normally
    ##############################################################################################################

    # Read data
    # Leave aggregation and rendering of map and density plots to observe events during program initialization
    readData("SampleData")

    # Following explicit calls are invalid because they are called outside of a reactive context (reactive
    # data values are not available)
    # agg(input$facetVar, input$carrierDelay, input$includeCancel)
    # graphFlightMap(input$pthreshFlight, input$pthreshAirportLabel, c(input$color1, input$color2, input$color3),
                   # input$colorScaleMid, input$sizeRange, input$alphaRange, input$facetVar, input$carrierDelay)
    # graphFlightDelayDensity(input$densX, input$densXLim, input$densY, input$densyOrder,
                            # c(input$densFillColor1, input$densFillColor2), input$densReverseFillColor,
                            # input$densAlpha, input$densFacetVar, input$densVLine)

    # Set default data directory and output directory locations
    updateTextInput(session, "dirDat", value=paste(getwd(), "/SampleData", sep=""))
    updateTextInput(session, "dirOut", value=getwd())

  }

)
