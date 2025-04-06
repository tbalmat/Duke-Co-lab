# Duke University Co-lab Shiny Workshop, Session 1, October 2019

# Shiny app user interface function
# Visually explore cross-sectional features of highly aggregated U.S. federal employee data
# Version 3, Shiny with additional sliderBar control features

options(max.print=1000)      # number of elements, not rows
options(stringsAsFactors=F)
options(scipen=999999)
#options(device="windows")

library(shiny)

#######################################################################################################
# Read U.S. Office of Personnel Management Central Personnel Data File (CPDF)

# Source:  Buzzfeed (https://www.buzzfeednews.com/article/jsvine/sharing-hundreds-of-millions-of-federal-payroll-records)
# Limited to general schedule (GS) grades 1 through 15, fiscal years 1988 through 2011, full-time employees

# OPM codebook:  https://www.opm.gov/policy-data-oversight/data-analysis-documentation/data-policy-guidance/reporting-guidance/part-a-human-resources.pdf

# Columns:
# fy ........... U.S. federal government fiscal year
# agency ....... federal agency employed (synthetically generated for workshop)
# age .......... employee age (five year increments, noised induced by OPM)
# grade ........ general schedule (GS) grade
# occCat ....... occupational category 
# yearsEd ...... years of education
# n ............ number of observations (employees) in fy, agency, age, grade, occCat, yearsEd combination
# sumPay ....... sum of basic pay in fy, agegncy, age, grade, occCat, yearsEd combination

# There is one record for each unique combination of fy, agency, age, grade, occCat, yearsEd combination
# n and sumPay are aggregated within fy, agency, age, grade, occCat, yearsEd combinations
#######################################################################################################

#######################################################################################################
# Read observations, compute mean categorical employee pay, and populate selection lists
# Create cpdf explicitly in the global environment, so that is is available to the Server function
# Objects created here are accessible to the UI function, which is also declared in this script
#######################################################################################################

# Local
setwd("C:\\Projects\\Duke\\Co-lab\\Shiny\\Session-1-NPDHist-CPDF\\App\\CPDF")

# RStudio Cloud
#setwd("/cloud/project/Duke-Co-lab/Shiny/Session-1-NPDHist-CPDF/App/CPDF")

cpdf <<- read.table(gzfile("CPDFAggregateDataBuzzfeed-Agency.csv.gz"), header=T, sep=",", strip.white=T)

# Compute mean pay per category (all employees in category assigned identical, mean, pay)
cpdf[,"pay"] <<- cpdf[,"sumPay"]/cpdf[,"n"]

agencyList <- c("all", sort(unique(cpdf[,"agency"])))

#######################################################################################################
# UI function
#######################################################################################################

shinyUI(

  fluidPage(

    includeCSS("style.css"),

    div(

      HTML("<H2>Duke University Co-lab Shiny Workshop</H2><br>"),
      HTML("<H3>OPM Central Personnel Data File Overview</H3>"),
      HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"),
      HTML("<a href=http://127.0.0.1:4291>Random Normal Histograms</a><br><br>"),

      tabsetPanel(id="cpdfTabs",

        # x-y plots
        tabPanel("x-y Plots",
          # Prompts
          column(width=2,
            HTML("<br>"),
            sidebarPanel(width=12,
              # Dependent variable (y-axis)
              selectInput(inputId="t1DepVar", label="depenedent variable", choices=c("age", "grade", "yearsEd", "pay")),
              # Independent variable (x-axis)
              selectInput("t1IndepVar", "indepenedent variable", c("fy", "age", "grade", "occCat", "yearsEd")),
              # Agency filter
              selectInput("t1AgencyFilter", "agency filter", agencyList, selected="all"),
              # Color differentiation variable
              selectInput("t1DiffVar", "color differentiation", c("none", "fy", "age", "grade", "occCat", "yearsEd")),
              # Faceting variable
              # One facet panel will be generated for each level of the variable
              selectInput("t1PanelVar", "panel variable", c("none", "fy", "age", "grade", "occCat", "yearsEd")),
              # Number of facet rows and cols
              # Note that, at most, one should be specified
              numericInput("t1PanelRows", "panel rows", value=NULL),
              numericInput("t1PanelCols", "panel columns", value=NULL),
              # Graph type, line or point
              radioButtons(inputId="t1GraphType", label="graph type", choices=c("line", "point"), inline=T),
              # Point size and transparency
              sliderInput("t1PointSize", "point size", min=1, max=10, step=0.25, value=2),
              sliderInput("t1PointAlpha", "point alpha", min=0.1, max=1, step=0.1, value=1),
              #HTML("<br>"),
              # Button to trigger plot generation
              actionButton(inputId="t1ActionPlot", label="plot", style="color:white; background:linear-gradient(#54b4eb, #2fa4e7 60%, #0088dd)")
            )
          ),  
          # Graph and message line
          column(width=10,
            HTML("<br><br><br><br><br><br>"),
            # Graph
            fluidRow(width=12,
                     HTML("<center>"),
                     column(width=12,
                            plotOutput("t1Plot", width="600px", height="600px"),
                            # Display FY slider bar only when independent var (x-axis) is not FY
                            # This will be used to subset the data for animated FY change analysis
                            conditionalPanel(condition="input.t1IndepVar!='fy'",
                                             sliderInput("t1PlotSlider", "", min=1988, max=2011, step=1, value=1988,
                                                         width="600px", sep="",
                                                         animate=animationOptions(playButton=HTML("<br><b>play</b>"), interval=500)))
                     ),
                     HTML("</center>")
            ),
            # Message line
            HTML("<br><br><br>"),
            fluidRow(width=12,
                     HTML("<center>"),
                     # Object type htmlOutput required to render HTML font tag (color, etc.)
                     column(width=12, htmlOutput("t1Msg")),
                     HTML("</center>")
            )
          )
        ),

        # Distribution plots
        tabPanel("Employee Distribution Plots",
          # Prompts
          column(width=2,
            HTML("<br>"),
            sidebarPanel(width=12,
              # Independent variable (x-axis)
              selectInput("t2IndepVar", "indepenedent variable", c("fy", "age", "grade", "occCat", "yearsEd")),
              # Agency filter
              selectInput("t2AgencyFilter", "agency filter", agencyList, selected="all"),
              # Faceting variable
              # One facet panel will be generated for each level of the variable
              selectInput("t2PanelVar", "panel variable", c("none", "fy", "age", "grade", "occCat", "yearsEd")),
              # Number of facet rows and cols
              # Note that, at most, one should be specified
              numericInput("t21PanelRows", "panel rows", value=NULL),
              numericInput("t2PanelCols", "panel columns", value=NULL),
              # LOESS span
              sliderInput("t2LoessSpan", "LOESS span", min=0, max=1, step=0.05, value=0.75),
              HTML("<br>"),
              # Button to trigger plot generation
              actionButton(inputId="t2ActionPlot", label="plot", style="color:white; background:linear-gradient(#54b4eb, #2fa4e7 60%, #0088dd)")
            )
          ),
          # Graph and message line
          column(width=10,
            HTML("<br><br><br><br><br><br>"),
            # Graph
            fluidRow(width=12,
                     HTML("<center>"),
                     column(width=12, plotOutput("t2Plot", width="600px", height="600px")),
                     HTML("</center>")
            ),
            # Message line
            HTML("<br><br><br>"),
            fluidRow(width=12,
                     HTML("<center>"),
                     # Object type htmlOutput required to render HTML font tag (color, etc.)
                     column(width=12, htmlOutput("t2Msg")),
                     HTML("</center>")
            )
          )
        )

      ),

      style="margin-left: 20px"
 
    )

  )

)
