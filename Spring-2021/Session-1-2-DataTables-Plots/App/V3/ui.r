# Duke University Co-lab Shiny Workshop, Session 1-2, March-Apr 2021

# Shiny app user interface function
# Compose tables of aggregated OPM CPDF U.S. federal employee career variables
# Visually explore cross-sectional features of subsets of employees contained in disjoint rows
# of aggregation table

# Version 3
# Features include:
# Navbar layout
# Use of Shiny theme
# Modification of theme elements using HTML style() instructions
# Use of modal windows for error reporting

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
fn <- c("Data/CPDFSampleDataBuzzfeed-Qtr-Tot.csv.gz", "Data/CPDFSampleDataBuzzfeed-100k.csv.gz")[2]

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
    includeCSS("App/V3/style.css"),

    # Modify browser tab title
    title="CPDF Analysis",

    # Specify a theme
    theme=shinythemes::shinytheme("spacelab"),

    # Enable theme selector
    shinythemes::themeSelector(),

    div(

      HTML("<H3>Duke University Co-lab Shiny Workshop</H3>"),
      HTML("<br><br>"),

      navbarPage(
        id="cpdfTabs", title="OPM Human Capital Overview, Version 3", inverse=T,
        # Themes have fixed search locations:
        # theme=shinytheme("xyz") searches for xyz.min.css in the shinythemes/css directory
        # of the shinythemes package directory in the R installation directory
        # theme="xyz.css" searches for file xyz.css in the www directory within the directory that the
        # shiny app was executed from within, not the current working directory, as set by setwd() 
        # The following method does not report an error, but the theme file is not loaded
        # theme="app/www/iCPAGdb-spacelab.css",
        # The following method does not report an error, but the theme file is not loaded
        # theme="C:/Users/Kyung Soon/Documents/R/win-library/4.0/shinythemes/shinythemes/css/spacelab.min.css",
        # The following methods load specified themes
        # Use the shinytheme() function (that returns a character vector)
        # This will locate the css file in the shinythemes package directory
        # Note that font urls within the css reference relative directory locations, which are
        # interpreted with respect to the css file location
        #theme=shinythemes::shinytheme("superhero"),

        # Aggregation table prompts
        tabPanel(title="Selection Table", value="t1",
          div(
            fluidRow(width=12,
              # Prompts
              column(width=2,
                div(
                  # Dependent variable (continuous)
                  selectInput(inputId="t1DepVar", label="dependent variable", choices=c("Grade", "Age", "EducationYears", "BasicPay")),
                  # Independent aggregation-by variables
                  selectInput("t1IndepVar", "indepenedent variables",
                              c("FY", "Grade", "Age", "EducationYears", "Agency", "OccupationalCategory", "Occupation"), multiple=T),
                  # Number of leading positions of agency to use
                  sliderInput("t1AgencyPos", "leading agency positions", min=agencyPosLimit[1], max=4, step=1, value=agencyPosLimit[1]),
                  # Number of leading positions of occupation to use
                  sliderInput("t1OccPos", "leading occupation positions", min=occPosLimit[1], max=4, step=1, value=occPosLimit[1]),
                  style="margin-top:20px; margin-left:10%; width:80%"
                )
              ),
              # Horizontal space
              column(width=1),
              # Aggregation table
              column(width=9,
                div(
                  #HTML("<center>"),
                  DT::dataTableOutput("t1Table", width="80%"),
                  #HTML("</center>"),
                  style="margin-top:15px; margin-bottom:30px"
                )
              )
            ),
            style="margin-top:-20px; margin-left:-15px; margin-right:-15px; background-color:#eeeeee; border-radius:0px 0px 7px 7px;"
          )
        ),

        # Distribution plots
        tabPanel(title="Distribution Plots", value="t2",
          div(
            fluidRow(width=12,
              # Prompts
              column(width=2,
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
                  #actionButton(inputId="t2ActionPlot", label="plot", style="color:white; background:linear-gradient(#54b4eb, #2fa4e7 60%, #0088dd)"),
                  actionButton(inputId="t2ActionPlot", label="plot"),
                  style="margin-top:20px; margin-left:10%; width:80%"
                )
              ),
              # Horizontal space
              column(width=1),
              # Graph
              column(width=9,
                div(
                  # Graph
                  #HTML("<center>"),
                  column(width=12, plotOutput("t2Plot", width="900px", height="900px")),
                  #HTML("</center>"),
                  style="margin-top:30px; margin-bottom:30px"
                )
              )
            ),
            style="margin-top:-20px; margin-left:-15px; margin-right:-15px; background-color:#eeeeee; border-radius:0px 0px 7px 7px;"
          )
        ),

        # Define styles within (as final step of) navbarPage to overide specific theme styles
        # Hide panel, otherwise clickable, yet invisible elements appear in the bar
        # with contents equal to the contents of the style tag, below
        shinyjs::hidden(
          tabPanel(title="", value="navBarStyles",
            # Use the "inspection" feature of your browser (right-button over object of interest) to reveal classes
            # Grab colors with the browser color sampler (dropper)
            # Buttons, default and hover
            tags$style(HTML(".btn-default{color:#ffffff;background:linear-gradient(#6c93be, #446e9b 60%, #3e648d);border-color:purple}")),
            tags$style(HTML(".btn-default:hover{color:#ffffff;background:linear-gradient(#6e96c2, #45709e 80%, #416994); border-color:#447e9b}")),
            # Hide navbar title
            #tags$style(HTML(".navbar-brand {display: none;}")),
            # Round corners of title bar
            tags$style(HTML(".navbar {border-top-left-radius:7px; border-top-right-radius:7px}")),
            # Reposition and alter appearance of notification window, used by showNotification()
            #tags$style(HTML(".shiny-notification {font-size:14px; color:black; font-weight:bold; width:50%;
            #                 height=200px; position:fixed; top:calc(50%); left:calc(25%)}")),
            # Modal box appearance
            tags$style(HTML(".modal {margin-top:10%}")),
            tags$style(HTML(".modal-header {background:linear-gradient(#6c93be, #446e9b 60%, #3e648d); border-radius:5px;}")),
            # Opacity of main modal box window
            tags$style(HTML(".modal-backdrop.in{opacity:0.15;filter:alpha(opacity=50)}")),
            # Modal box title
            tags$style(HTML(".modal-title{color:#ffffff; margin:0;line-height:1.42857143}")),
            # tabsetPanel buttons
            # In the rendered HTML, the buttons appear as anchor tags within in unordered lists
            # View (toggle) pseudo class of object using the browser inspector to review pseudo classes (hover,
            # active, focus, focus-within) and related css properties
            # Pill button height (all pseudo classes)
            tags$style(HTML(".nav-pills{line-height:0.75}")),
            #tags$style(HTML(".nav-pills>li>a:hover{color:green;}")),
            #tags$style(HTML(".nav-pills>li>a:active{color:red;}")),
            # Create a hidden variable to instruct on tabPanelExplore visibility
            checkboxInput("tabPanelExploreVisible", "", F)
          )
        )

      ),

      style="width:90%; margin-left:20px"
 
    )

  )

)
