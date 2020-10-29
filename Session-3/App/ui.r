#####################################################################################################
# Duke University Co-lab Shiny Workshop, Session 3, November 2019
# U.S. Domestic Flight Evalution
# Shiny user interface script
#####################################################################################################

# Information on shiny available at:
# https://shiny.rstudio.com/
# https://github.com/rstudio/shiny
# https://cran.r-project.org/web/packages/shiny/shiny.pdf

#######################################################################################################
# Prepare and display plots of the distribution of domestic flights originating and terminating in the
# continental United States
#
# The first plot connects flight origin and destination using an arc with size, color, and transparency
# as a function of proportion of all flights
#
# The second plot displays the distribution of density of flights by minutes of delay in departure due
# to carrier or arrival delay minus departure delay (to measure effectiveness of crew ability to
# compensate for departure delay)
# 
# Flight map customization features include
# Filtering of flights based on proportion of overall flights (minimum threshold, to eliminate low volume routes)
# Limiting flights to those classified as delayed due to carrier reasons
# Inclusion of canceled flights
# Paneling (individual plots) by weekday, month, carrier, or no panel
# Adjustment of arc size, color, and transparency by upper and/or lower proportion thresholds
# Display of airport labels with minimum proportion threshold
#
# Density plot customization features include
# x-axis variable specification (carrier delay, arrival-departure delay)
# y-axis variable specification (one distribution for each weekday, month, or carrier)
# Reordering of y-axis ridges by natural order (weekday, month, carrier), mean, or median of ridge distributions
# Paneling by weekday, month, or carrier
# Display of a vertical line at the means or medians of each distribution or at zero (crew goal)
# Lower and upper limits on x-axis
# Adjustment of distribution color and transparency
#
# Data source:  U.S. Bureau of Transportation Statistics Carrier On-Time Performance Data:
# https://www.transtats.bts.gov/ONTIME/
# https://www.transtats.bts.gov/Fields.asp 
#
# For this app, a fifty percent random sample of flights recorded in January, April, July, and October of 2018
# observations are used.  Columns retained from those made available by BTS are FlightDate, Month, DayOfWeek,
# DOT_ID_Reporting_Airline, OriginAirportID, DestAirportID, DepDelay, ArrDelay, Cancelled, CarrierDelay,
# OriginState, and DestState.  Additional information on BTS column definitions is available at https://www.transtats.bts.gov/Fields.asp
#######################################################################################################

library(shiny)

shinyUI(
  fluidPage(

    # Set browser tab title
    title="U.S. Flight Data",

    # Reposition and alter appearance of notification window
    tags$head(
      tags$style(
        HTML(".shiny-notification {font-size:20px; color:black; font-style:bold; width:50%; position:fixed; top:calc(50%); left:calc(25%)}")
        )
    ),

    div(HTML("<h3>Duke University U.S. Domestic Flight Analysis (Bureau of Transportation Statistics 2018 Data)</h3><br>"),
        style="margin-left: 20px"
    ),

    # Use a div to provide a slight left margin
    div(
      fluidRow(width=12,
        tabsetPanel(id="flightTabs",

          # Tab 1:  Continental U.S. map with flight routes displayed
          tabPanel("Flight Map",
            # Prompt
            HTML("<br>"),
            fluidRow(width=12,
              column(width=3, sliderInput("pthreshFlight", "Flight Proportion Threshold", min=0, max=0.05, step=0.00025, value=0.001, width="90%")),
              column(width=3, sliderInput("pthreshAirportLabel", "Airport Label Proportion Threshold", min=0, max=0.05, step=0.00025, value=0.0015, width="90%")),
              column(width=1,
                div(selectInput("color1", "Color Low", choices=c("Red", "Green", "Blue", "Yellow", "Brown", "Purple", "Black"), selected="Blue", multiple=F),
                    style="width: 100%; display: inline-block; vertical-align: top; margin-top: 15px"
                )
              ),
              column(width=1,
                div(selectInput("color2", "Color Mid", choices=c("Red", "Green", "Blue", "Yellow", "Brown", "Purple", "Black"), selected="Green", multiple=F),
                    style="width: 100%; display: inline-block; vertical-align: top; margin-top: 15px"
                )
              ),
              column(width=1,
                div(selectInput("color3", "Color High", choices=c("Red", "Green", "Blue", "Yellow", "Brown", "Purple", "Black"), selected="Red", multiple=F),
                    style="width: 100%; display: inline-block; vertical-align: top; margin-top: 15px"
                )
              ),
              column(width=3, sliderInput("colorScaleMid", "Mid-color p Value", min=0, max=0.05, step=0.00025, value=0.001, width="90%"))
            ),
            #HTML("<br>"),
            fluidRow(width=12,
              column(width=3, sliderInput("sizeRange", "Arc Size Range", min=0, max=2, step=0.05, value=c(0.25, 2), width="90%")),
              column(width=3, sliderInput("alphaRange", "Arc Alpha Range", min=0, max=1, step=0.05, value=c(0.25, 0.85), width="90%")),
              column(width=3,
                div(radioButtons("facetVar", "Facet Variable", choiceValues=c("DayOfWeek", "Month", "DOT_ID_Reporting_Airline", ""),
                                 choiceNames=c("Weekday", "Month", "Carrier", "None"), selected="", inline=T),
                    style="display: inline-block; vertical-align: top; margin-top: 20px;"
                )
              ),
              column(width=1,
                div(checkboxInput("carrierDelay", "Carrier Delay Only", value=F),
                    style="display: inline-block; vertical-align: top; margin-top: 35px"
                )
              ),
              column(width=1,
                div(checkboxInput("includeCancel", "Include Cancellations", value=F),
                    style="display: inline-block; vertical-align: top; margin-top: 35px; margin-left: 35px"
                )
              )
            ),
            # Message line
            #fluidRow(width=12, column(width=12, textOutput("msgMap"))),
            #HTML("<br>"),
            # Graph
            fluidRow(width=12,
              HTML("<center>"),
              plotOutput("plotUSFlights"),
              HTML("</center>")
            )
          ),

          # Panel 2:  Flight density ridges of carrier or arrival delay by select y variable 
          tabPanel("Flight Delay Density Ridges",
            HTML("<br>"),
            fluidRow(width=12,
              div(radioButtons("densX", "x", choiceValues=c("CarrierDelay", "ArrDepDelayDiff"),
                               choiceNames=c("Carrier Delay", "Arrival-Departure Delay"), selected="CarrierDelay", inline=T),
                  style="display: inline-block; vertical-align: top; margin-left: 20px; margin-top: 15px;"
              ),
              div(sliderInput("densXLim", "x Limits", min=-200, max=200, step=5, value=c(-100, 100)),
                  style="display: inline-block; width: 300px; vertical-align: top; margin-left: 75px"
              ),
              div(radioButtons("densY", "y", choiceValues=c("DayOfWeek", "Month", "DOT_ID_Reporting_Airline"),
                               choiceNames=c("Weekday", "Month", "Carrier"), selected="DayOfWeek", inline=T),
                  style="display: inline-block; vertical-align: top; margin-left: 75px; margin-top: 15px"
              ),
              div(radioButtons("densyOrder", "y Order", choiceValues=c("natural", "mean", "median"),
                               choiceNames=c("Natural", "Mean(x)", "Median(x)"), selected="natural", inline=T),
                  style="display: inline-block; vertical-align: top; margin-left: 75px; margin-top: 15px;"
              )
            ),
            fluidRow(width=12,
              div(selectInput("densFillColor1", "Color Low", choices=c("Red", "Blue", "Green", "Gold"), selected="Blue", multiple=F),
                  style="display: inline-block; width: 100px; vertical-align: top; margin-left: 20px; margin-top: 15px"
              ),
              div(selectInput("densFillColor2", "Color High", choices=list("dark"=c("Red", "Blue"), "bright"=c("Green", "Gold")), selected="Gold", multiple=F),
                  style="display: inline-block; width: 100px; vertical-align: top; margin-left: 20px; margin-top: 15px"
              ),
              div(checkboxInput("densReverseFillColor", "Reverse Color", value=F),
                  style="display: inline-block; vertical-align: top; margin-top: 35px; margin-left: 135px"
              ),
              div(sliderInput("densAlpha", "Alpha", min=0, max=1, step=0.05, value=0.5, width="90%"),
                  style="display: inline-block; width: 260px; vertical-align: top; margin-left: 75px"
              ),
              div(radioButtons("densFacetVar", "Facet Variable", choiceValues=c("DayOfWeek", "Month", "DOT_ID_Reporting_Airline", ""),
                               choiceNames=c("Weekday", "Month", "Carrier", "None"), selected="", inline=T),
                  style="display: inline-block; vertical-align: top; margin-left: 45px; margin-top: 15px;"
              ),
              div(radioButtons("densVLine", "Vertical Lines", choiceValues=c("mean", "median", "0", ""),
                               choiceNames=c("Mean", "Median", "Zero", "None"), selected="", inline=T),
                  style="display: inline-block; vertical-align: top; margin-left: 75px; margin-top: 15px"
              )
            ),
            # Message line
            #fluidRow(width=12, column(width=12, textOutput("msgDens"))),
            #HTML("<br>"),
            # Graph
            fluidRow(width=12,
              HTML("<center>"),
              plotOutput("plotDensityRidge"),
              HTML("</center>")
            )
          ),

          # Panel three, data file location
          tabPanel("Data/Directories",
            HTML("<br>"),
            fluidRow(width=12,
              column(width=6, textInput("dirDat", "Data File Directory", width="100%")),
              div(column(width=1, actionButton("retrieveData", "Retrieve")),
                  style="display: inline-block; vertical-align: top; margin-top: 25px;"
              )
            ),
            fluidRow(width=12, column(width=6, textInput("dirOut", "Output File Directory", width="100%")))
          )

        )
      ),
      style="margin-left: 30px"
    )

  )
)