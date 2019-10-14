# Shiny App
# Generate a histogram from random normal values

library(shiny)

shinyUI(

   fluidPage(

      HTML("<br><b>Duke University Co-lab - Hello Shiny!<br><br>Generate Random, Normally Distribute Values</b><br><br>"),
      
      # Prompt
      fluidRow(width=12,
               column(width=5, sliderInput("n", "number to generate", min=0, max=50000, step=250, value=5000, width="90%"))
      ),
      
      HTML("<br><br><br>"),
      
      # Graph
      fluidRow(width=12,
               column(width=12, plotOutput("plot", width="600px", height="600px"))
      )
      
    )

)