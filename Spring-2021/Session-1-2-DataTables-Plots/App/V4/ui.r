# Duke University Co-lab Shiny Workshop, Session 1-2, Mar-Apr 2012

# Shiny app user interface function
# Compose tables of aggregated OPM CPDF U.S. federal employee career variables
# Visually explore cross-sectional features of subsets of employees contained in disjoint rows
# of aggregation table

# Version 4
# Features include:
# Draggable control panels, table, and plot

# Information on shiny available at:
# https://shiny.rstudio.com/
# https://github.com/rstudio/shiny
# https://cran.r-project.org/web/packages/shiny/shiny.pdf

library(shiny)
library(shinythemes)
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

# Local directory
setwd("C:/Projects/Duke/Co-lab/Shiny-Spring-2021/Session-1-2-DataTables-Plots")

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

    # Include static cascading style sheets definitions
    includeCSS("App/V4/style.css"),

    # Modify browser tab title
    title="CPDF Analysis",

    # Define styles, using modified theme
    # Use the "inspection" feature of your browser (right-button over object of interest) to reveal classes
    # Grab colors with the browser color sampler (dropper)
    theme=shinythemes::shinytheme("spacelab"),
    # Buttons, default and hover
    tags$style(HTML(".btn-default{color:#ffffff;background:linear-gradient(#6c93be, #446e9b 60%, #3e648d);border-color:#000000}")),
    tags$style(HTML(".btn-default:hover{color:#ffffff;background:linear-gradient(#6e96c2, #45709e 80%, #416994); border-color:#447e9b}")),
    # Reposition and alter appearance of notification window, used by showNotification()
    #tags$style(HTML(".shiny-notification {font-size:14px; color:black; font-weight:bold; width:50%;
    #                height=200px; position:fixed; top:calc(50%); left:calc(25%)}")),
    # Modal box appearance
    tags$style(HTML(".modal {margin-top:10%}")),
    tags$style(HTML(".modal-header {background:linear-gradient(#6c93be, #446e9b 60%, #3e648d); border-radius:5px;}")),
    # Opacity of modal box main window
    tags$style(HTML(".modal-backdrop.in{opacity:0.15;filter:alpha(opacity=50)}")),
    # Modal title
    tags$style(HTML(".modal-title{color:#ffffff; margin:0;line-height:1.42857143}")),

    div(

      HTML("<H3>Duke University Co-lab Shiny Workshop</H3><br>"),
      HTML("<H4>OPM Human Capital Overview, Version 4</H4>"),

      # Aggregation table prompts
      fixedPanel(draggable=T, top="12%", left="2%",
        div(
          # Inspect title bar style:  browser, inspect element, scroll through HTML until title bar highlighted, inspect pseudo elements
          # Note element IDs, modify values (height) and uncheck elements to observe behavior
          div("1. Query Obserevations", style="height:30px; background-color:#0066bb; border-radius:5px 5px 0px 0px; color:white; text-align:center; padding:5px"),
          div(
            div(
              # Dependent variable (continuous)
              selectInput(inputId="t1DepVar", label="dependent variable", choices=c("Grade", "Age", "EducationYears", "BasicPay")),
              # Independent aggregation-by variables
              selectInput("t1IndepVar", "indepenedent variables", c("FY", "Grade", "Age", "EducationYears", "Agency", "OccupationalCategory", "Occupation"), multiple=T),
              # Number of leading positions of agency to use
              sliderInput("t1AgencyPos", "leading agency positions", min=agencyPosLimit[1], max=4, step=1, value=agencyPosLimit[1]),
              # Number of leading positions of occupation to use
              sliderInput("t1OccPos", "leading occupation positions", min=occPosLimit[1], max=4, step=1, value=occPosLimit[1]),
              style="padding:16px 16px 2px 16px"
            ),
            style="background-color:#eeeeee; opacity:0.75; border-radius:0px 0px 5px 5px"
          ),
          style="width:235px"
        )
      ),

      # Aggregation table
      fixedPanel(draggable=T, top="12%", left="15%",
        div("2. Select Subset", style="height:30px; background-color:#0066bb; border-radius:5px 5px 0px 0px; color:white; text-align:center; padding:5px"),
        div(
          div(
            DT::dataTableOutput("t1Table"),
            style="padding:10px 16px 10px 16px"
          ),
          style="background-color:#eeeeee; opacity:0.75; border-radius:0px 0px 5px 5px"
        )
      ),

      # Distribution plot prompts
      fixedPanel(draggable=T, top="12%", left="35%",
        div(
          div("3. Configure Plot", style="height:30px; background-color:#0066bb; border-radius:5px 5px 0px 0px; color:white; text-align:center; padding:5px"),
          div(
            div(
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
              actionButton(inputId="t2ActionPlot", label="plot"),
              style="padding:16px 16px 16px 16px"
            ),
            style="background-color:#eeeeee; border-radius:0px 0px 5px 5px"
          ),
          style="width:235px"
        )
      ),

      # Graph
      fixedPanel(draggable=T, top="12%", left="50%",
        div("4. Review Plot", style="height:30px; background-color:#0066bb; border-radius:5px 5px 0px 0px; color:white; text-align:center; padding:5px"),
        div(
          div(
            plotOutput("t2Plot", width="900px", height="900px"),
            style="padding:20px"
          ),
          style="background-color:#eeeeee; border-radius:0px 0px 5px 5px"
        )
      )
 
    )

  )

)
