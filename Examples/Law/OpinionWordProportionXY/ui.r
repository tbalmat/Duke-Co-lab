# Duke University Law Appeal Text Analysis

# Shiny user interface module

library(shiny)

shinyUI(
  fluidPage(

    HTML("<br><b>Duke University Law School - LexisNexis Appeal Opinion Text Analysis - Word Proportion X-Y Plots</b><br><br>"),

    # prompts
    fluidRow(width=12,
      column(width=2, selectInput("caseClass1", "Case Class 1", choices="")),
      column(width=2, selectInput("caseClass2", "Case Class 2", choices="")),
      div(column(width=2,
                 actionButton("actionViewPlot1", "View", width="60px", style="margin-top: 25px; padding:6.5px;"),
                 actionButton("actionSavePlot1", "Save", width="60px", style="margin-top: 25px; padding:6.5px;"),
          style="display: inline-block; vertical-align: top; margin-left: 0px")), 
      column(width=2, selectInput("opType1", "Opinion Type 1", choices="")),
      column(width=2, selectInput("opType2", "Opinion Type 2", choices="")),
      div(column(width=2,
                 actionButton("actionViewPlot2", "View", width="60px", style="margin-top: 25px; padding:6.5px;"),
                 actionButton("actionSavePlot2", "Save", width="60px", style="margin-top: 25px; padding:6.5px;"),
          style="display: inline-block; vertical-align: top; margin-left: 0px"))
    ),
    fluidRow(width=12,
      column(width=4, sliderInput("pRange", "p-Window", value=c(0, 0.02), min=0, max=0.02, step=0.001)),
      div(column(width=6,
                 textInput("graphDir", "Graph Output Directory", width="100%",
                 value=("C:\\Projects\\Duke\\Law\\LexisNexisCaseAnalysis\\LexisNexisDecisionSample\\OpinionTextAnalysis\\Review\\images"))),
                 style="display: width: 100%; inline-block; vertical-align: top; margin-top: 10px")
    ),

    # Message line
    fluidRow(width=12, column(width=12, textOutput("msg"))),

    HTML("<br>"),

    # Graph
    fluidRow(width=12,
      column(width=12,
             HTML("<center>"),
             plotOutput("wordProportionXY", width="800px", height="800px"),
             HTML("</center>"))
    )

  )
)
