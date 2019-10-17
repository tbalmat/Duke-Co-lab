# Shiny App
# Generate a histogram from random normal values

# Version 4:
# Additional reactive variables for mean and standard deviation
# Model and plot parameter validation
# Use of defined function calls from within server()
# Further HTML formatting (div for margins)
# Use of cascading style sheet
# Implementation of sidebarPanel
# Incremental construction of plot, based on requested features (web page controls)
# Use of input$x within reactive function, as opposed to x <- reactive(x) followed by reference to x()

options(max.print=1000)      # number of elements, not rows
options(stringsAsFactors=F)
options(scipen=999999)
#options(device="windows")

library(shiny)
library(ggplot2)

# A Shiny app consists of ui() and server() functions

# ui() can contain R statements (open a database and query it to populate selection lists, etc.), but its primary
# purpose is to format your web page (notice the explicit use of HTML tags)

# The HTML() function instructs Shiny to pass contained text to the browser verbatim, and is useful for formatting
# your page

# server() is a function containing R statements and function calls 
# Any base function, functions declared in loaded packages (importantly, Shiny, here), or functions that you create
# in global memory cacn be called

# runApp() is a Shiny function that launches your default browser, renders a page based on the ui() function passed,
# then executes the server() function

ui <- function(req) {
  
  fluidPage(

   includeCSS("style.css"),

    div(

      HTML("<br><H2>Duke University Co-lab - Hello Shiny!</H2><br><H3>Generate Random, Normally Distributed Values</H3>"),

      # Prompt
      column(width=2,
        HTML("<br><br><br>"),
        sidebarPanel(width=12,
          div(
            fluidRow(
              sliderInput("n", "number to generate", min=0, max=50000, step=250, value=5000, width="100%"),
              HTML("<br>"),
              sliderInput("w", "bar width", min=0, max=1, step=0.01, value=0.5, width="100%"),
              HTML("<br>"),
              numericInput("mean", "mean", value=0),
              HTML("<br>"),
              numericInput("sd", "Standard deviation", min=0, value=1),
              HTML("<br>"),
              selectInput("barColor", "bar color",
                          list("light"=c("ivory", "yellow"),
                               "med"=c("gray75", "blue3"),
                               "dark"=c("gray35", "black")), selected="blue3"),
              HTML("<br>"),
              checkboxInput("dispCurve", "display curve", T),
              HTML("<br>"),
              radioButtons("curveColor", "curve color", choices=c("orange", "purple"), selected="orange", inline=T)
            ),
            style="margin-left: 10px; margin-right: 10px"
          )
        )
      ),

      column(width=10,
        HTML("<br><br><br><br>"),
        # Graph
        fluidRow(width=12,
                 HTML("<center>"),
                 column(width=12, plotOutput("plot", width="600px", height="600px")),
                 HTML("</center>")
        ),
        # Message line
        HTML("<br><br><br>"),
        fluidRow(width=12,
                 HTML("<center>"),
                 # Object type htmlOutput required to render HTML font tag (color, etc.)
                 column(width=12, htmlOutput("msg")),
                 HTML("</center>")
        )
      ),

      style="margin-left: 20px"

    )

  )

}

# Function to generate either a ggplot object or NULL if standard deviation non-positive
npdPlot <- function(n, w, mn, sd, barColor, dispCurve, curveColor) {
  if(sd>0) {
    g <- ggplot() +
      # Bars
      geom_histogram(aes(x=rnorm(n, mean=mn, sd=sd)), color="white", binwidth=w, fill=barColor)
    if(dispCurve)
      # Continuous normal model curve
      g <- g + geom_line(aes(x=seq(mn-4*sd, mn+4*sd, length.out=100),
                             y=n*w*dnorm(seq(mn-4*sd, mn+4*sd, length.out=100), mn, sd)), color=curveColor, size=2)
      # Static portion of plot
      g <- g + scale_x_continuous(limits=c(mn-4*sd, mn+4*sd)) +
      scale_y_continuous(labels=function(x) format(x, big.mark=",")) +
      theme(plot.title=element_text(size=14, hjust=0.5),
            plot.subtitle=element_text(size=12, hjust=0.5),
            plot.caption=element_text(size=12, hjust=0.5),
            panel.background=element_blank(),
            panel.grid.major.x=element_blank(),
            panel.grid.major.y=element_blank(),
            panel.grid.minor=element_blank(),
            panel.border=element_rect(fill=NA, color="gray75"),
            panel.spacing.x=unit(0, "lines"),
            axis.title.x=element_text(size=12),
            axis.title.y=element_text(size=12),
            axis.text.x=element_text(size=10),
            axis.text.y=element_text(size=10),
            strip.text=element_text(size=10),
            strip.background=element_blank(),
            legend.position="bottom",
            legend.background=element_rect(color="gray"),
            legend.key=element_rect(fill="white"),
            legend.box="horizontal",
            legend.text=element_text(size=8),
            legend.title=element_text(size=8)) +
      labs(title=paste(format(n, big.mark=","), " normal(", mn, ", ", sd, ") pseudo-random values\n", sep=""), x="\nz", y="frequency\n")
  } else {
    g <- NULL
  }
  return(g)
}

# Function to test parameter values and return message or NULL (if all parameters valid)
msg <- function(w, sd) {
  txt <- NULL
  if(w==0) {
    txt <- "Hmmm:  perhaps bar width should be larger"
    color <- "black"
  } else if(sd<=0) {
    txt <- "Error:  standard deviation must be positive"
    color <- "red"
  }
  if(!is.null(txt)) {
    return(paste("<font color=", color, ">", txt, "&nbsp;&nbsp;&nbspTime:  ", Sys.time(), "</font>", sep=""))
  } else {
    return(NULL)
  }
}

server <- function(input, output, session) {
  
  # Use of cat() displays messages in R console, stderr() causes disply in red and writes to log (Shiny server)
  #cat("AAA", file=stderr())

  # Create and render plot
  # Note the use of input$n, input$w, etc. in the reactive function (renderPlot)
  # In this case, the result is the same as using m <- reactive(input$m) followed by reference to m()
  # Cases have been observed where reactive values are not available (such as when placing the executed
  # function directly in the renderPlot() call)
  output$plot <- renderPlot(npdPlot(input$n, input$w, input$mean, input$sd, input$barColor,
                            input$dispCurve, input$curveColor))

  # Generate a parameter (in)validation message (message is NULL if valid)
  output$msg <- renderText(HTML(msg(input$w, input$sd)))

}

# Execute
runApp(list("ui"=ui, "server"=server), launch.browser=T)
