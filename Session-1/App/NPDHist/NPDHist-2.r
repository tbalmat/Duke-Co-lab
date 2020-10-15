# Duke University Co-lab Shiny Workshop, Session 1, Spring 2020

# Shiny App
# Generate a histogram from random normal values
# Version 2, two reactive variables, modification of either causes update of plot

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
      
      HTML("<br><b>Duke University Co-lab - Hello Shiny!<br><br>Generate Random, Normally Distributed Values</b><br><br>"),
      
      # Prompt
      fluidRow(width=12,
               column(width=5, sliderInput("n", "number to generate", min=0, max=50000, step=250, value=5000, width="90%")),
               column(width=2, sliderInput("w", "bar width", min=0, max=1, step=0.01, value=0.5, width="100%"))
      ),
      
      # Message line
      fluidRow(width=12, column(width=12, textOutput("msg"))),
      
      HTML("<br><br><br>"),
      
      # Graph
      fluidRow(width=12,
               HTML("<center>"),
               column(width=12, plotOutput("plot", width="600px", height="600px")),
               HTML("</center>")
      ),
      
      style="margin-left: 20px"
      
    )
    
  )
}

server <- function(input, output, session) {

  # Use of cat() displays messages in R console, stderr() causes disply in red and writes to log (Shiny server)
  #cat("AAA", file=stderr())

  # Bind reactive variables
  # They are referenced as functions in a reactive context (renderPlot, renderText, renderPlotly, renderTable, etc.)
  # Change in the value of reactive variables causes reactive function (renderPlot below) to be re-evaluated with new values
  n <- reactive(input$n)
  w <- reactive(input$w)

  # Create and render plot
  # References to n() and w() cause re-execution of renderPlot() anytime input$n or input$w are modified
  # This gives the "instantaneous" or "fluid" appearance to graph updates in response to on-screen inputs
  output$plot <- renderPlot(
    ggplot() +
      geom_histogram(aes(x=rnorm(n())), color="white", binwidth=w(), fill="blue3") +
      # Add continuous normal model curve
      geom_line(aes(x=seq(-4, 4, 0.01), y=n()*w()*dnorm(seq(-4, 4, 0.01))), color="orange", size=2) +
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
      labs(title=paste(format(n(), big.mark=","), " normal(0, 1) pseudo-random values\n", sep=""), x="\nz", y="frequency\n")
  )
  
}

# Execute
runApp(list("ui"=ui, "server"=server), launch.browser=T)
