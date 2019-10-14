# Duke University Law Appeal Text Analysis

# Shiny user interface module

library(shiny)

shinyUI(
  fluidPage(

    HTML("<br><b>Duke University Law School - LexisNexis Appeal Opinion Text Analysis - Word Association Networks</b><br><br>"),

    # prompts
    fluidRow(width=12,
      column(width=7, radioButtons("caseClass", "Case Class", choices=rep("", 5), selected="", inline=T)),
      column(width=4, radioButtons("opType", "Opinion Type", choices=rep("", 4), selected="", inline=T))
      #style="display: inline-block; vertical-align: top; margin-top: 20px; margin-left: 25px; text-align: left")
    ),
    fluidRow(width=12,
      column(width=4, sliderInput("nedgethresh", "Min Edge Frequency (per 1,000)", value=2, min=0, max=10, step=0.1)),
      div(column(width=6,
                 textInput("graphDir", "Graph Output Directory", width="100%",
                 value=("C:\\Projects\\Duke\\Law\\LexisNexisCaseAnalysis\\LexisNexisDecisionSample\\OpinionTextAnalysis\\Review\\images"))),
                 style="display: width: 100%; inline-block; vertical-align: top; margin-top: 10px"),
      div(actionButton("actionViewPlots", "View", width="60px", style="margin-top: 25px; padding:6.5px;"),
                       style="display: inline-block; vertical-align: top; margin-left: 10px"),
      div(actionButton("actionSavePlots", "Save", width="60px", style="margin-top: 25px; padding:6.5px;"),
                       style="display: inline-block; vertical-align: top; margin-left: 10px")
    ),

    # Message line
    fluidRow(width=12, column(width=12, textOutput("msg"))),

    HTML("<br>"),

    # Graphs
    fluidRow(width=12,
      column(width=6, plotOutput("wordPairNetFreq", width="600px", height="600px")),
      column(width=6, plotOutput("wordPairNetCorr", width="600px", height="600px"))
    )

  )
)
