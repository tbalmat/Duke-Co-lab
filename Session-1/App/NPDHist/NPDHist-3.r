# Duke University Co-lab Shiny Workshop, Session 1, Spring 2020

# Shiny App
# Generate a histogram from random normal values

# Version 3:
# Additinal reactive variables for mean and standard deviation
# Model and plot parameter validation
# Use of defined function calls from within server()
# Further HTML formatting
# Implementation of sidebarPanel

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

    div(

      HTML("<br><H2>Duke University Co-lab - Hello Shiny!</H2><br><H3>Generate Random, Normally Distributed Values</H3>"),

      # Prompt
      column(width=2,
        HTML("<br><br><br><br>"),
        sidebarPanel(width=12,
          fluidRow(
            sliderInput("n", "number to generate", min=0, max=50000, step=250, value=5000, width="100%"),
            HTML("<br>"),
            sliderInput("w", "bar width", min=0, max=1, step=0.01, value=0.5, width="100%"),
            HTML("<br>"),
            numericInput("mean", "mean", value=0, step=0.25),
            HTML("<br>"),
            numericInput("sd", "Standard deviation", min=0, value=1)
          )
        )
      ),
  
      column(width=10,
        HTML("<br><br><br>"),
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
npdPlot <- function(n, w, mn, sd) {
  if(sd>0) {
    g <- ggplot() +
      geom_histogram(aes(x=rnorm(n, mean=mn, sd=sd)), color="white", binwidth=w, fill="blue3") +
      # Add continuous normal model curve
      geom_line(aes(x=seq(mn-4*sd, mn+4*sd, length.out=100), y=n*w*dnorm(seq(mn-4*sd, mn+4*sd, length.out=100), mn, sd)), color="orange", size=2) +
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

  # Bind reactive variable(s)
  # They are referenced as functions in a reactive context (renderPlot, renderText, renderPlotly, renderTable, etc.)
  # Change in the value of reactive variables causes reactive function (renderPlot below) to be re-evaluated with new values
  n <- reactive(input$n)
  w <- reactive(input$w)
  mn <- reactive(input$mean)
  sd <- reactive(input$sd)
  
  # Create and render plot
  # Since renderPlot generates a reactive context, references to n(), w(), mn(), and sd() cause
  # evaluation of npdPlot anytime inputs are modified
  # npdPlot() validates w() (non-zero) and sd() (positive) and returns NULL if either are invalid or
  # a ggplot object
  # Note that the contents of npdPlot() could be specified as the first parameter of renderPlot() as
  # renderPlot({if(sd()>0) ...}), but declaring a function enables generation of a standard plot
  # from multiple places in a script (and promotes organization and ease of maintenance)
  # Note that npdPlot() is declared (in the global environment) outside of server()
  output$plot <- renderPlot(npdPlot(n(), w(), mn(), sd()))
  
  # Generate a parameter (in)validation message (message is NULL if valid)
  output$msg <- renderText({HTML(msg(w(), sd()))})

}

# Execute
runApp(list("ui"=ui, "server"=server), launch.browser=T)
