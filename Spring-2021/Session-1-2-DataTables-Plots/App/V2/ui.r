# Duke University Co-lab Shiny Workshop, Session 1-2, March-Apr 2021

# Shiny app user interface function
# Compose tables of aggregated OPM CPDF U.S. federal employee career variables
# Visually explore cross-sectional features of subsets of employees contained in disjoint rows
# of aggregation table

# Version 2, Shiny implementation
# Features include:
# Use of a tab panel with individual tabs for aggregation and plot controls
# Use of cascading style sheet for HTML formatting
# Immediate reactive controls for preparing the aggregation table (which is efficient)
# An action button for plot rendering (as opposed to immediate reactivity, since plotting time may be "long") 
# Multiple selection of aggregation variables
# Functions for aggregating data and preparing plots
# Table row selection for subsetting source data to be plotted

# Information on shiny available at:
# https://shiny.rstudio.com/
# https://github.com/rstudio/shiny
# https://cran.r-project.org/web/packages/shiny/shiny.pdf

library(shiny)
library(DT)

#######################################################################################################
# Read U.S. Office of Personnel Management Central Personnel Data File (CPDF)
#
# Source:  Buzzfeed (https://www.buzzfeednews.com/article/jsvine/sharing-hundreds-of-millions-of-federal-payroll-records)
#
# Observations are limited to:
# FY between 1988 and 2011
# WorkSchedule=F, PayPlan=GS, and Grade between 01 and 15
# OccupationCategory in {P, A, T, C, O}
# EducationLevel between 01 and 22
# AdjustedBasicPay > 10
# Top five agencies (left two positions) by observation frequency
#
# OPM codebook:  https://www.opm.gov/policy-data-oversight/data-analysis-documentation/data-policy-guidance/reporting-guidance/part-a-human-resources.pdf
#
# Columns:
# PseudoID ................... unique (OPM randomly assigned) employee ID
# FY ......................... U.S. federal government fiscal year
# Agency ..................... federal agency employed
# Grade ...................... general schedule (GS) grade
# OccupationalCategory ....... occupational category 
# Occupation ................. occupation
# Age ........................ employee age (five year increments, noised induced by OPM)
# EducationYears ............. years of education
# BasicPay ................... adjusted basic pay, in 2011 $U.S.
#######################################################################################################

#######################################################################################################
# Read observations, compute mean categorical employee pay, and populate selection lists
# Create cpdf explicitly in the global environment, so that is is available to the Server function
# Objects created here are accessible to the UI function, which is also declared in this script
#######################################################################################################

# Local
setwd("C:/Projects/Duke/Co-lab/Shiny-Spring-2021/Session-1-2-DataTables-Plots")

# RStudio Cloud
#setwd("/cloud/project/Duke-Co-lab/Shiny/Session-2-DataTables-Plots")

# Randomly select one of four employee subsets from the full Buzzfeed data set
#fn <- paste("Data/CPDFSampleDataBuzzfeed-", sprintf("%02.0f", sample(1:4, 1)), ".csv.gz", sep="")

# Specify source data file
fn <- c("Data/CPDFSampleDataBuzzfeed-Qtr-Tot.csv.gz", "Data/CPDFSampleDataBuzzfeed-100k.csv.gz")[1]

# Read observations into global environment so that data are available in server module
cpdf <<- read.table(gzfile(fn), header=T, sep=",", strip.white=T)

# Omit invalid observations
cpdf <<- subset(cpdf, !is.na(cpdf[,"Age"]))

# Convert occupation to four position alpha (for some reason, read.table from gz file converts this column to numeric)
cpdf[,"Occupation"] <<- sprintf("%04.0f", cpdf[,"Occupation"])

gc()

agencyList <- c("all", sort(unique(cpdf[,"Agency"])))

# Specify limits on agency and occupation leading positions
agencyPosLimit <<- c(2, 4)
occPosLimit <<- c(2, 4)

#######################################################################################################
# UI function
#######################################################################################################

shinyUI(

  fluidPage(

    # Include style sheet
    includeCSS("App/V2/style.css"),

    # Set browser tab title
    title="CPDF Analysis",

    # Configure some styles
    # Use the "inspection" feature of your browser (right-button over object of interest) to reveal classes
    tags$head(
      # Reposition and alter appearance of notification window, used by showNotification()
      tags$style(HTML(".shiny-notification {font-size:14px; color:black; font-weight:bold; width:50%;
                      height=200px; position:fixed; top:calc(50%); left:calc(25%)}"))
    ),

    div(

      HTML("<H2>Duke University Co-lab Shiny Workshop</H2><br>"),
      HTML("<H3>OPM Human Capital Overview, Version 2</H3>"),
      HTML("<br>"),

      tabsetPanel(id="cpdfTabs",

        # Tabular view
        tabPanel(title="Selection Table", value="t1",
          # Prompts
          column(width=2,
            HTML("<br>"),
            sidebarPanel(width=12,
              # Dependent variable (continuous)
              selectInput(inputId="t1DepVar", label="dependent variable", choices=c("Grade", "Age", "EducationYears", "BasicPay")),
              # Independent aggregation-by variables
              selectInput("t1IndepVar", "indepenedent variables", c("FY", "Grade", "Age", "EducationYears", "Agency", "OccupationalCategory", "Occupation"), multiple=T),
              # Number of leading positions of agency to use
              sliderInput("t1AgencyPos", "leading agency positions", min=agencyPosLimit[1], max=4, step=1, value=agencyPosLimit[1]),
              # Number of leading positions of occupation to use
              sliderInput("t1OccPos", "leading occupation positions", min=occPosLimit[1], max=4, step=1, value=occPosLimit[1])
              # Button to trigger table generation
              #actionButton(inputId="t1ActionComposeTable", label="compose table", style="color:white; background:linear-gradient(#54b4eb, #2fa4e7 60%, #0088dd)")
            )
          ),
          # Vertical buffer
          column(width=1),
          # Table
          column(width=8,
            DT::dataTableOutput("t1Table", width="60%")
          )
        ),

        # Distribution plots
        tabPanel(title="Distribution Plots", value="t2",
          # Prompts
          column(width=2,
            HTML("<br>"),
            sidebarPanel(width=12,
              # Independent variable (x-axis)
              selectInput("t2IndepVar", "independent variable", c("FY", "Grade", "Age", "EducationYears", "Agency", "OccupationalCategory", "Occupation")),
              # Agency filter
              #selectInput("t2AgencyFilter", "agency filter", agencyList, selected="all"),
              # Faceting variable
              # One facet panel will be generated for each level of the variable
              selectInput("t2PanelVar", "panel variable", c("none", "FY", "Grade", "Age", "EducationYears", "Agency", "OccupationalCategory", "Occupation")),
              # Number of facet rows and cols
              # Note that, at most, one should be specified
              numericInput("t2PanelRows", "panel rows", value=NULL),
              numericInput("t2PanelCols", "panel columns", value=NULL),
              # Point display
              checkboxInput("t2PointDisplay", HTML("<b>display points</b>"), value=F),
              # Point color differentiation variable
              selectInput("t2PointDiffVar", "point differentiation var", c("none", "FY", "Grade", "Age", "EducationYears", "Agency", "OccupationalCategory", "Occupation")),
              # Point size
              sliderInput("t2PointSize", "point size", min=1, max=10, step=0.5, value=3),
              # Point jitter width
              sliderInput("t2PointJitterWidth", "point jitter width", min=0, max=1, step=0.1, value=0.2),
              # Point transparency
              sliderInput("t2PointAlpha", "point alpha", min=0.1, max=1, step=0.05, value=0.5),
              HTML("<br>"),
              # Button to trigger plot generation
              actionButton(inputId="t2ActionPlot", label="plot", style="color:white; background:linear-gradient(#54b4eb, #2fa4e7 60%, #0088dd)")
            )
          ),
          # White space
          column(width=1),
          # Graph and message line
          column(width=8,
            HTML("<br>"),
            # Graph
            fluidRow(width=12,
                     #HTML("<center>"),
                     column(width=12, plotOutput("t2Plot", width="900px", height="900px"))
                     #HTML("</center>")
            ),
            # Message line
            HTML("<br><br>"),
            fluidRow(width=12,
                     #HTML("<center>"),
                     # Object type htmlOutput required to render HTML font tag (color, etc.)
                     column(width=10, htmlOutput("t2Msg"))
                     #HTML("</center>")
            )
          )
        )

      ),

      style="margin-left: 20px"
 
    )

  )

)
