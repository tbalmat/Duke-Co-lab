# Shiny App
# Generate a histogram from random normal values

library(shiny)

shinyUI(

  fluidPage(

   includeCSS("style.css"),

    div(

      HTML("<H2>Duke University Co-lab - Hello Shiny!</H2><br>"),
      HTML("<H3>Generate Random, Normally Distributed Values</H3>"),
      HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"),
      HTML("<a href=http://127.0.0.1:4292>OPM CPDF Overview</a><br>"),

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

)