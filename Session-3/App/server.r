#####################################################################################################
# Duke University Co-lab Shiny Workshop, Session 3, November 2019
# U.S. Domestic Flight Evalution
# Shiny server script
#####################################################################################################

options(stringsAsFactors=F)
options(scipen=999999)

library(shiny)

# Set current working directory
# Local
setwd("C:/Projects/Duke/Co-lab/Shiny/Session-3-ggplot")
# Rstudio Cloud dir
#setwd("/cloud/project/Duke-Co-lab/Shiny/Session-3-ggplot")

# Import flight evaluation functions
source("App/FlightEvaluation-Functions.r", echo=F)

# Server function
shinyServer(
  function(input, output, session) {

    # Data retrieval button event
    observeEvent(input$retrieveData, {
      cat("Read0\n")
      readData(input$dirDat)
      cat("Read1\n")
      fldatb <<- agg(input$aggVar, input$carrierDelay, input$includeCancel)
    })

    # Aggregate flights
    agg <- function(aggVar, carrierDelay, includeCancel) {
      # Ignore if data not yet available (typically on program initialization)
      if(exists("fldat")) {
        cat("agg0\n")
        fldatb <<- aggfdat01(fldat, aggVar, apdat, carrierDelay, includeCancel)
        cat("agg1\n")
      }
    }

    # Monitor controls that affect aggregation for map generation
    observe({
      # Capture reactive values
      # This forces execution of instructions whenever any one variable changes value
      facetVar <- input$facetVar
      carrierDelay <- input$carrierDelay
      includeCancel <- input$includeCancel
      agg(facetVar, carrierDelay, includeCancel)
    })

    # Compose and render flight map when reactive values change
    observe({

      # Record reactive values
      # Assignment forces execution of instructions whenever any one variable changes value
      pthreshFlight <- input$pthreshFlight
      pthreshAirportLabel <- input$pthreshAirportLabel
      colorRange <- c(input$color1, input$color2, input$color3)
      colorScaleMid <- input$colorScaleMid
      sizeRange <- input$sizeRange
      alphaRange <- input$alphaRange
      facetVar <- input$facetVar
      carrierDelay <- input$carrierDelay

      # Ignore if no flight data available (typically on program initialization)
      if(exists("fldatb")) {
        cat("graph-map-0\n")

        # Restrict flights to proportion >= p-threshold
        kflgtp <- which(fldatb[,"p"]>pthreshFlight)

        # Compose unique list of airports with proportion flights above label threshold
        k <- which(fldatb[,"p"]>pthreshAirportLabel)
        if(length(k)>0) {
          if(facetVar!="") {
            # Compose within facet var and retain facet var value, so that sets are produced for each facet level
            aplab <- aggregate(k, by=list(fldatb[k,facetVar]),
                       function(k) {
                         # Assemble vector of unique airports and retrieve associated lat and long
                         ap <- unique(c(fldatb[k,"OriginAirportID"], fldatb[k,"DestAirportID"]))
                         return(apdat[which(apdat[,"airport_id"] %in% ap),c("airport", "latitude", "longitude")])
                       })
            aplab <- do.call(rbind,
                             apply(as.matrix(1:nrow(aplab)), 1,
                               function(i) data.frame(aplab[i,1], aplab[i,2][[1]], aplab[i,2][[2]], aplab[i,2][[3]])))
            colnames(aplab) <- c(facetVar, "airport", "latitude", "longitude")
          } else {
            # Compose within entire set of flights, since grouping (faceting) not requested
            # Assemble vector of unique airports and retrieve associated lat and long
            ap <- unique(c(fldatb[k,"OriginAirportID"], fldatb[k,"DestAirportID"]))
            aplab <- apdat[which(apdat[,"airport_id"] %in% ap),c("airport", "latitude", "longitude")]
          }
        } else {
          aplab <- data.frame("airport"=character(), "latitude"=numeric(), "longitude"=numeric())
        }

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
        progress <- shiny::Progress$new()
        progress$set(message="composing plot", value=1)
        output$plotUSFlights <- renderPlot({# Enable progress indicator
                                            progress <- shiny::Progress$new()
                                            on.exit(progress$close())
                                            progress$set(message="composing plot", value=1)
                                            # Render plot
                                            print(composePlotMap01(fldatb[kflgtp,], colorRange, colorScaleMid, sizeRange,
                                                                   alphaRange, aplab, facetVar, facetLabel, facetRows))},
                                            width=gwidth, height=gheight)
        progress$close()

        cat("graph-map-1\n")
      }
    })

    # Compose and render density plot when reactive values change
    observe({

      # Capture reactive values
      # Assignment forces execution of instructions whenever any one variable changes value
      x <- input$densX
      xlim <- input$densXLim
      y <- input$densY
      yOrder <- input$densyOrder
      yFillColor <- c(input$densFillColor1, input$densFillColor2)
      reverseFillColor <- input$densReverseFillColor
      fillAlpha <- input$densAlpha
      facetVar <- input$densFacetVar
      vline <- input$densVLine

      # Ignore if data not yet available (typically on program initialization)
      if(exists("fldatb")) {
        cat("graph-dens-0\n")

        # Configure plot height and width
        if(facetVar=="Month") {
          facetLabel <- monthLabel
          facetRows <- 2
          gheight <- 700
          gwidth <- 800
        } else if(facetVar=="DayOfWeek") {
          facetLabel <- weekdayLabel
          facetRows <- 2
          gheight <- 700
          gwidth <- 1600
        } else if(facetVar=="DOT_ID_Reporting_Airline") {
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

        # Render plot
        progress <- shiny::Progress$new()
        progress$set(message="composing plot", value=1)
        output$plotDensityRidge <- renderPlot({# Enable progress indicator
                                               progress <- shiny::Progress$new()
                                               on.exit(progress$close())
                                               progress$set(message="composing plot", value=1)
                                               # Render plot
                                               print(composePlotDensity01(fldat, aldat, x, xlim, y, yOrder,
                                                                          yFillColor, reverseFillColor, fillAlpha,
                                                                          facetVar, facetLabel, facetRows, vline))},
                                                width=gwidth, height=gheight)
        progress$close()

        cat("graph-dens-1\n")
      }
    })

    # Read data from default location
    # Failure to load data during initialization causes reactive function to execute in a state
    # that loses reactive methods (changing input$ objects does not execute associated observe() functions
    # Waiting to load data with the input$retrieveData event does not resolve the issue (reactive objects
    # fail to execute changes even after data are loaded in the observe event)
    # Explicit load of data resolves the issue and all reactive elements behave normally
    cat("Read0\n")
    readData("SampleData")
    cat("Read1\n")

    # Set default data directory and output directory locations
    updateTextInput(session, "dirDat", value=paste(getwd(), "/Data", sep=""))
    updateTextInput(session, "dirOut", value=getwd())

  }

)
